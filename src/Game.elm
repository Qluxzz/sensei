module Game exposing (Model, Msg(..), init, update, view)

import Html exposing (Html, button, div, form, li, p, span, text, ul)
import Html.Attributes exposing (autofocus, class, classList, disabled, style, type_, value)
import Html.Events exposing (onClick, onInput, onSubmit)
import Platform.Cmd as Cmd
import Romaji exposing (CharacterMapping, groupByMora)
import Set exposing (Set)
import Tooltip exposing (withTooltip)
import Words exposing (Word)


type State
    = Romaji -- Enter the word in romaji representation
    | RomajiToHiragana -- Convert the romaji to hiragana
    | WhatDoesWordMean -- Enter what the word means


cleanAttempt : Attempt
cleanAttempt =
    Attempt "" Undecided


init : Word -> Result.Result String ( Model, Cmd Msg )
init word =
    groupByMora word.kana
        |> Result.map
            (\characterMapping ->
                ( { word = word
                  , attempt = cleanAttempt
                  , state = Romaji
                  , characterMapping = characterMapping
                  , romaji = List.map .romaji characterMapping |> String.concat
                  , showGlossaryAtIndex = Set.empty
                  }
                , Cmd.none
                )
            )


type Result
    = Undecided
    | Correct
    | Incorrect


type alias Model =
    { word : Word
    , characterMapping : List CharacterMapping
    , romaji : String
    , attempt : Attempt
    , state : State
    , showGlossaryAtIndex : Set Int
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
    | RevealGlossaryWord Int


normalizeInput : String -> String
normalizeInput =
    String.toLower >> String.trim


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        attempt =
            model.attempt
    in
    case msg of
        Submit ->
            let
                failedAttempt =
                    { attempt | result = Incorrect }

                correctAttempt =
                    { attempt | result = Correct }
            in
            case model.state of
                Romaji ->
                    ( { model
                        | attempt =
                            if normalizeInput attempt.input == model.romaji then
                                correctAttempt

                            else
                                failedAttempt
                      }
                    , Cmd.none
                    )

                RomajiToHiragana ->
                    ( { model
                        | attempt =
                            if normalizeInput attempt.input == model.word.kana then
                                correctAttempt

                            else
                                failedAttempt
                      }
                    , Cmd.none
                    )

                WhatDoesWordMean ->
                    ( { model
                        | attempt =
                            if List.any ((==) (normalizeInput attempt.input)) model.word.glossary then
                                correctAttempt

                            else
                                failedAttempt
                      }
                    , Cmd.none
                    )

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

        RevealGlossaryWord index ->
            ( { model | showGlossaryAtIndex = Set.insert index model.showGlossaryAtIndex }, Cmd.none )

        -- Handled by Main.elm
        NextWord ->
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    let
        attempt : Attempt
        attempt =
            model.attempt
    in
    div [ class "content" ]
        (case model.state of
            Romaji ->
                [ p [] [ text <| "Your word is " ++ model.word.str, span [ style "white-space" "nowrap" ] [ text <| "(" ++ model.word.kana ++ ")" ] ]
                , text "It means:"
                , div [ style "overflow" "auto" ]
                    [ ul [] (List.map (\meaning -> li [] [ text meaning ]) model.word.glossary)
                    ]
                , div [ style "flex-grow" "1" ] []
                , form [ onSubmit Submit, style "display" "flex", style "flex-direction" "column", style "gap" "10px" ]
                    [ p [] ((text <| "Please write ") :: List.map (\{ mora, romaji } -> withTooltip mora romaji) model.characterMapping ++ [ text " in romaji" ])
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
                [ div [] [ text <| "The word in romaji is " ++ model.romaji ]
                , div [ style "flex-grow" "1" ] []
                , form [ onSubmit Submit, style "display" "flex", style "flex-direction" "column", style "gap" "10px" ]
                    [ p [] (text "Enter hiragana for " :: List.map (\{ mora, romaji } -> withTooltip romaji mora) model.characterMapping)
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
                [ div [] [ text <| "Your word is " ++ model.word.str, span [ style "white-space" "nowrap" ] [ text <| "(" ++ model.word.kana ++ ")" ] ]
                , div [ style "overflow" "auto" ]
                    [ ul [ class "hidden-glossary-list" ] (List.indexedMap (\i -> \meaning -> li [ classList [ ( "visible", Set.member i model.showGlossaryAtIndex ) ], onClick (RevealGlossaryWord i) ] [ text meaning ]) model.word.glossary)
                    ]
                , div
                    [ style "flex-grow" "1" ]
                    []
                , form [ onSubmit Submit, style "display" "flex", style "flex-direction" "column", style "gap" "10px" ]
                    [ p [] [ text <| "Enter one of the glossary words for " ++ model.word.str ]
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
