module Game exposing (..)

import Html exposing (Html, button, div, form, li, p, span, text, ul)
import Html.Attributes exposing (autofocus, class, disabled, style, type_, value)
import Html.Events exposing (onClick, onInput, onSubmit)
import Romaji exposing (convertWord)
import Words exposing (Word)


type State
    = Romaji -- Enter the word in romaji representation
    | RomajiToHiragana -- Convert the romaji to hiragana
    | WhatDoesWordMean -- Enter what the word means


cleanAttempt : Attempt
cleanAttempt =
    Attempt "" Undecided


init : Word -> ( Model, Cmd Msg )
init word =
    ( { word = word
      , attempt = cleanAttempt
      , state = Romaji
      }
    , Cmd.none
    )


type Result
    = Undecided
    | Correct
    | Incorrect


type alias Model =
    { word : Word
    , attempt : Attempt
    , state : State
    }


type alias Attempt =
    { input : String
    , result : Result
    }


type Msg
    = Submit
    | Input String
    | NextWord
    | Continue


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        attempt =
            model.attempt

        failedAttempt =
            { attempt | result = Incorrect }

        correctAttempt =
            { attempt | result = Correct }
    in
    case msg of
        Submit ->
            case model.state of
                Romaji ->
                    if String.toLower attempt.input == convertWord model.word.normalized then
                        ( { model | attempt = correctAttempt }, Cmd.none )

                    else
                        ( { model | state = Romaji, attempt = failedAttempt }, Cmd.none )

                RomajiToHiragana ->
                    if String.toLower attempt.input == model.word.normalized then
                        ( { model | attempt = correctAttempt }, Cmd.none )

                    else
                        ( { model | state = RomajiToHiragana, attempt = failedAttempt }, Cmd.none )

                WhatDoesWordMean ->
                    if List.any ((==) (String.toLower attempt.input)) model.word.glossary then
                        ( { model | attempt = correctAttempt }, Cmd.none )

                    else
                        ( { model | attempt = failedAttempt }, Cmd.none )

        Input str ->
            let
                updatedAttempt =
                    { attempt | input = str }
            in
            ( { model | attempt = updatedAttempt }, Cmd.none )

        Continue ->
            case model.state of
                Romaji ->
                    ( { model | state = RomajiToHiragana, attempt = cleanAttempt }, Cmd.none )

                RomajiToHiragana ->
                    ( { model | state = WhatDoesWordMean, attempt = cleanAttempt }, Cmd.none )

                WhatDoesWordMean ->
                    ( model, Cmd.none )

        -- Handled by Main.elm
        NextWord ->
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    let
        attempt =
            model.attempt
    in
    div [ class "content" ]
        (case model.state of
            Romaji ->
                [ p [] [ text <| "Your word is " ++ model.word.str, span [ style "white-space" "nowrap" ] [ text <| "(" ++ model.word.normalized ++ ")" ] ]
                , text "It means:"
                , div [ style "overflow" "auto" ]
                    [ ul [] (List.map (\meaning -> li [] [ text meaning ]) model.word.glossary)
                    ]
                , div [ style "flex-grow" "1" ] []
                , form [ onSubmit Submit, style "display" "flex", style "flex-direction" "column", style "gap" "10px" ]
                    [ p [] [ text <| "Please write " ++ model.word.normalized ++ " in romaji" ]
                    , Html.input [ type_ "text", onInput Input, value attempt.input, autofocus True, disabled <| model.attempt.result == Correct ] []
                    , case model.attempt.result of
                        Correct ->
                            button [ type_ "button", onClick Continue ] [ text "Continue!" ]

                        Incorrect ->
                            button [] [ text "Submit" ]

                        Undecided ->
                            button [] [ text "Submit" ]
                    ]
                , resultView attempt.result
                ]

            RomajiToHiragana ->
                [ div [] [ text <| "The word in romaji is " ++ convertWord model.word.normalized ]
                , form [ onSubmit Submit, style "display" "flex", style "flex-direction" "column", style "gap" "10px" ]
                    [ p [] [ text "Enter hiragana of above word" ]
                    , Html.input [ type_ "text", onInput Input, value attempt.input, autofocus True, disabled <| model.attempt.result == Correct ] []
                    , case model.attempt.result of
                        Correct ->
                            button [ type_ "button", onClick Continue ] [ text "Continue!" ]

                        Incorrect ->
                            button [] [ text "Submit" ]

                        Undecided ->
                            button [] [ text "Submit" ]
                    ]
                , resultView attempt.result
                ]

            WhatDoesWordMean ->
                [ div [] [ text <| "Your word is " ++ model.word.str ]
                , text <| model.word.normalized
                , form [ onSubmit Submit, style "display" "flex", style "flex-direction" "column", style "gap" "10px" ]
                    [ p [] [ text "Enter one of the glossary words of the above word" ]
                    , Html.input [ type_ "text", onInput Input, value attempt.input, autofocus True, disabled <| model.attempt.result == Correct ] []
                    , case model.attempt.result of
                        Correct ->
                            button [ type_ "button", onClick NextWord ] [ text "Next word!" ]

                        Incorrect ->
                            button [] [ text "Submit" ]

                        Undecided ->
                            button [] [ text "Submit" ]
                    ]
                , resultView attempt.result
                ]
        )


resultView : Result -> Html msg
resultView res =
    p []
        [ case res of
            Correct ->
                text "That's correct!"

            Incorrect ->
                text "Sorry, that's not the right answer!"

            Undecided ->
                text ""
        ]
