module Game exposing (Model, MoraResult(..), Msg, OutMsg(..), hiraganaToMora, init, romajiToMora, update, view)

import Html exposing (Html, button, div, form, li, p, span, text, ul)
import Html.Attributes exposing (autofocus, class, classList, disabled, id, style, type_, value)
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


type OutMsg
    = NextWord
    | RomajiAttemptResult (List ( String, MoraResult )) -- For each mora, did the user guess it correctly?


type Msg
    = Submit
    | Input String
    | Continue
    | RevealGlossaryWord Int


normalizeInput : String -> String
normalizeInput =
    String.toLower >> String.trim


update : Msg -> Model -> ( Model, Cmd Msg, Maybe OutMsg )
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

                normalizedInput =
                    normalizeInput attempt.input
            in
            case model.state of
                Romaji ->
                    ( { model
                        | attempt =
                            if normalizedInput == model.romaji then
                                correctAttempt

                            else
                                failedAttempt
                      }
                    , Cmd.none
                    , Just (RomajiAttemptResult (hiraganaToMora normalizedInput model.characterMapping))
                    )

                RomajiToHiragana ->
                    ( { model
                        | attempt =
                            if normalizedInput == model.word.kana then
                                correctAttempt

                            else
                                failedAttempt
                      }
                    , Cmd.none
                    , Just (RomajiAttemptResult (romajiToMora normalizedInput model.characterMapping))
                    )

                WhatDoesWordMean ->
                    ( { model
                        | attempt =
                            if List.any (normalizeInput >> (==) normalizedInput) model.word.glossary then
                                correctAttempt

                            else
                                failedAttempt
                      }
                    , Cmd.none
                    , Nothing
                    )

        Input str ->
            let
                updatedAttempt =
                    { attempt | input = str }
            in
            ( { model | attempt = updatedAttempt }, Cmd.none, Nothing )

        Continue ->
            case model.state of
                Romaji ->
                    ( { model | state = RomajiToHiragana, attempt = cleanAttempt }, Cmd.none, Nothing )

                RomajiToHiragana ->
                    ( { model | state = WhatDoesWordMean, attempt = cleanAttempt }, Cmd.none, Nothing )

                WhatDoesWordMean ->
                    ( model, Cmd.none, Just NextWord )

        RevealGlossaryWord index ->
            ( { model | showGlossaryAtIndex = Set.insert index model.showGlossaryAtIndex }, Cmd.none, Nothing )


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
                [ kanjiDisplay model.word.str
                , p [] [ text <| "Your word is " ++ model.word.str, span [ style "white-space" "nowrap" ] [ text <| "(" ++ model.word.kana ++ ")" ] ]
                , p [] [ text "It means:" ]
                , div [ style "overflow" "auto" ]
                    [ ul [] (List.map (\meaning -> li [] [ text meaning ]) model.word.glossary)
                    ]
                , div [ style "flex-grow" "1" ] []
                , form [ onSubmit Submit, style "display" "flex", style "flex-direction" "column", style "gap" "10px" ]
                    [ Html.label [ Html.Attributes.for "input-field" ] ((text <| "Please write ") :: List.map (\{ mora, romaji } -> withTooltip mora romaji) model.characterMapping ++ [ text " in romaji" ])
                    , Html.input [ Html.Attributes.id "input-field", Html.Attributes.attribute "aria-label" "input-field", type_ "text", onInput Input, value attempt.input, autofocus True, disabled <| model.attempt.result == Correct ] []
                    , case model.attempt.result of
                        Correct ->
                            button [ type_ "button", onClick Continue ] [ text "Continue!" ]

                        Incorrect ->
                            button [] [ text "Submit" ]

                        Undecided ->
                            button [ disabled <| String.isEmpty attempt.input ] [ text "Submit" ]
                    ]
                , resultView attempt.result
                ]

            RomajiToHiragana ->
                [ kanjiDisplay model.word.str
                , div [] [ text <| "The word in romaji is " ++ model.romaji ]
                , div [ style "flex-grow" "1" ] []
                , form [ onSubmit Submit, style "display" "flex", style "flex-direction" "column", style "gap" "10px" ]
                    [ Html.label [ Html.Attributes.for "input-field" ] (text "Enter hiragana for " :: List.map (\{ mora, romaji } -> withTooltip romaji mora) model.characterMapping)
                    , Html.input [ Html.Attributes.id "input-field", Html.Attributes.attribute "aria-label" "input-field", type_ "text", onInput Input, value attempt.input, autofocus True, disabled <| model.attempt.result == Correct ] []
                    , case model.attempt.result of
                        Correct ->
                            button [ type_ "button", onClick Continue ] [ text "Continue!" ]

                        Incorrect ->
                            button [] [ text "Submit" ]

                        Undecided ->
                            button [ disabled <| String.isEmpty attempt.input ] [ text "Submit" ]
                    ]
                , resultView attempt.result
                ]

            WhatDoesWordMean ->
                [ kanjiDisplay model.word.str
                , div [] [ text <| "Your word is " ++ model.word.str, span [ style "white-space" "nowrap" ] [ text <| "(" ++ model.word.kana ++ ")" ] ]
                , div [ style "overflow" "auto" ]
                    [ ul [ class "hidden-glossary-list" ]
                        (List.indexedMap
                            (\i ->
                                \meaning ->
                                    let
                                        visible =
                                            Set.member i model.showGlossaryAtIndex
                                    in
                                    li [ id <| "glossary-item-" ++ String.fromInt (i + 1), classList [ ( "visible", visible ) ], onClick (RevealGlossaryWord i) ]
                                        [ text
                                            (if visible then
                                                meaning

                                             else
                                                ""
                                            )
                                        ]
                            )
                            model.word.glossary
                        )
                    ]
                , div
                    [ style "flex-grow" "1" ]
                    []
                , form [ onSubmit Submit, style "display" "flex", style "flex-direction" "column", style "gap" "10px" ]
                    [ Html.label [ Html.Attributes.for "input-field" ] [ text <| "Enter one of the glossary words" ]
                    , Html.input [ Html.Attributes.id "input-field", Html.Attributes.attribute "aria-label" "input-field", type_ "text", onInput Input, value attempt.input, autofocus True, disabled <| model.attempt.result == Correct ] []
                    , case model.attempt.result of
                        Correct ->
                            button [ type_ "button", onClick Continue ] [ text "Next word!" ]

                        Incorrect ->
                            button [] [ text "Submit" ]

                        Undecided ->
                            button [ disabled <| String.isEmpty attempt.input ] [ text "Submit" ]
                    ]
                , resultView attempt.result
                ]
        )


kanjiDisplay : String -> Html msg
kanjiDisplay kanji =
    Html.h1
        [ style "font-size" ("min(calc(100cqw / " ++ String.fromInt (String.length kanji) ++ " - 10px), calc(50cqw - 10px))")
        , style "text-align" "center"
        , style "line-height" "1"
        ]
        [ text kanji ]


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


type MoraResult
    = CorrectMora
    | IncorrectMora


{-| Validates attempt and give result per hiragana/kana character
-}
getResultPerMora : (CharacterMapping -> String) -> String -> List CharacterMapping -> List ( String, MoraResult )
getResultPerMora field attempt correct =
    List.foldr
        (\character ->
            \( att, acc ) ->
                let
                    str =
                        field character

                    length =
                        String.length str

                    chars =
                        String.right length att

                    remaining =
                        String.dropRight length att
                in
                ( remaining
                , ( character.mora
                  , if str == chars then
                        CorrectMora

                    else
                        IncorrectMora
                  )
                    :: acc
                )
        )
        ( attempt, [] )
        correct
        |> Tuple.second


romajiToMora : String -> List CharacterMapping -> List ( String, MoraResult )
romajiToMora =
    getResultPerMora .romaji


hiraganaToMora : String -> List CharacterMapping -> List ( String, MoraResult )
hiraganaToMora =
    getResultPerMora .mora
