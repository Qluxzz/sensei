module Game exposing (..)

import Html exposing (Html, br, button, div, input, li, p, text, ul)
import Html.Attributes exposing (style, type_, value)
import Html.Events exposing (onClick, onInput)
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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        attempt =
            model.attempt

        failedAttempt =
            { attempt | result = Incorrect }
    in
    case msg of
        Submit ->
            case model.state of
                Romaji ->
                    if String.toLower attempt.input == convertWord model.word.normalized then
                        ( { model | state = RomajiToHiragana, attempt = cleanAttempt }, Cmd.none )

                    else
                        ( { model | state = Romaji, attempt = failedAttempt }, Cmd.none )

                RomajiToHiragana ->
                    if String.toLower attempt.input == model.word.normalized then
                        ( { model | state = WhatDoesWordMean, attempt = cleanAttempt }, Cmd.none )

                    else
                        ( { model | state = RomajiToHiragana, attempt = failedAttempt }, Cmd.none )

                WhatDoesWordMean ->
                    if List.any ((==) (String.toLower attempt.input)) model.word.glossary then
                        ( { model | attempt = failedAttempt }, Cmd.none )

                    else
                        ( { model | state = WhatDoesWordMean, attempt = failedAttempt }, Cmd.none )

        Input str ->
            let
                updatedAttempt =
                    { attempt | input = str }
            in
            ( { model | attempt = updatedAttempt }, Cmd.none )

        -- Handled by Main.elm
        NextWord ->
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    let
        attempt =
            model.attempt
    in
    div [ style "display" "flex", style "flex-direction" "column", style "height" "100%" ]
        (case model.state of
            Romaji ->
                [ descriptionView ("Your word is " ++ model.word.str)
                    (Just <|
                        div []
                            [ text <| model.word.str ++ " means:"
                            , ul [] (List.map (\meaning -> li [] [ text meaning ]) model.word.glossary)
                            ]
                    )
                , resultView attempt.result
                , br [] []
                , text <| model.word.normalized
                , inputView "Enter romaji of above word" attempt.input
                ]

            RomajiToHiragana ->
                [ descriptionView ("The word in romaji is " ++ convertWord model.word.normalized) Nothing
                , resultView attempt.result
                , br [] []
                , inputView "Enter hiragana of above word" attempt.input
                ]

            WhatDoesWordMean ->
                [ descriptionView ("Your word is " ++ model.word.str) Nothing
                , resultView attempt.result
                , br [] []
                , text <| model.word.normalized
                , inputView "Enter one of the glossary words of the above word" attempt.input
                ]
        )


descriptionView : String -> Maybe (Html msg) -> Html msg
descriptionView description extra =
    div [ style "flex-grow" "1" ]
        [ text <| description
        , extra |> Maybe.withDefault (text "")
        ]


resultView : Result -> Html msg
resultView res =
    case res of
        Correct ->
            text "Right"

        Incorrect ->
            text "Wrong!"

        Undecided ->
            text ""


inputView : String -> String -> Html Msg
inputView helperText input =
    div [ style "display" "flex", style "flex-direction" "column", style "gap" "10px" ]
        [ p [] [ text helperText ]
        , Html.input [ type_ "text", onInput Input, value input ] []
        , button [ onClick Submit ] [ text "Submit" ]
        ]
