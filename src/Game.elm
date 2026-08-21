module Game exposing (Model, MoraResult(..), Msg, OutMsg(..), hiraganaToMora, init, romajiToMora, update, view)

import Html exposing (Html)
import Html.Attributes
import Html.Events
import Romaji
import Set exposing (Set)
import Tooltip
import Words


type State
    = Romaji -- Enter the word in romaji representation
    | RomajiToHiragana -- Convert the romaji to hiragana
    | WhatDoesWordMean -- Enter what the word means


cleanAttempt : Attempt
cleanAttempt =
    Attempt "" Undecided


init : Words.Word -> Result.Result String ( Model, Cmd Msg )
init word =
    Romaji.groupByMora word.kana
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
    { word : Words.Word
    , characterMapping : List Romaji.CharacterMapping
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
                    , Just (RomajiAttemptResult (romajiToMora normalizedInput model.characterMapping))
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
                    , Just (RomajiAttemptResult (hiraganaToMora normalizedInput model.characterMapping))
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
    Html.div [ Html.Attributes.class "content" ]
        (case model.state of
            Romaji ->
                [ kanjiDisplay model.word.str
                , Html.p [] [ Html.text <| "Your word is " ++ model.word.str, Html.span [ Html.Attributes.style "white-space" "nowrap" ] [ Html.text <| "(" ++ model.word.kana ++ ")" ] ]
                , Html.p [] [ Html.text "It means:" ]
                , Html.div [ Html.Attributes.style "overflow" "auto" ]
                    [ Html.ul [] (List.map (\meaning -> Html.li [] [ Html.text meaning ]) model.word.glossary)
                    ]
                , Html.div [ Html.Attributes.style "flex-grow" "1" ] []
                , Html.form [ Html.Events.onSubmit Submit, Html.Attributes.style "display" "flex", Html.Attributes.style "flex-direction" "column", Html.Attributes.style "gap" "10px" ]
                    [ Html.label [ Html.Attributes.for "input-field" ] ((Html.text <| "Please write ") :: List.map (\{ mora, romaji } -> Tooltip.withTooltip mora romaji) model.characterMapping ++ [ Html.text " in romaji" ])
                    , Html.input [ Html.Attributes.id "input-field", Html.Attributes.attribute "aria-label" "input-field", Html.Attributes.type_ "text", Html.Events.onInput Input, Html.Attributes.value attempt.input, Html.Attributes.autofocus True, Html.Attributes.disabled <| model.attempt.result == Correct ] []
                    , if model.attempt.result == Correct then
                        Html.button [ Html.Attributes.type_ "button", Html.Events.onClick Continue ] [ Html.text "Continue!" ]

                      else
                        Html.button [ Html.Attributes.type_ "submit", Html.Attributes.disabled <| String.isEmpty attempt.input ] [ Html.text "Submit" ]
                    ]
                , resultView attempt.result
                ]

            RomajiToHiragana ->
                [ kanjiDisplay model.word.str
                , Html.div [] [ Html.text <| "The word in romaji is " ++ model.romaji ]
                , Html.div [ Html.Attributes.style "flex-grow" "1" ] []
                , Html.form [ Html.Events.onSubmit Submit, Html.Attributes.style "display" "flex", Html.Attributes.style "flex-direction" "column", Html.Attributes.style "gap" "10px" ]
                    [ Html.label [ Html.Attributes.for "input-field" ] (Html.text "Enter hiragana for " :: List.map (\{ mora, romaji } -> Tooltip.withTooltip romaji mora) model.characterMapping)
                    , Html.input [ Html.Attributes.id "input-field", Html.Attributes.attribute "aria-label" "input-field", Html.Attributes.type_ "text", Html.Events.onInput Input, Html.Attributes.value attempt.input, Html.Attributes.autofocus True, Html.Attributes.disabled <| model.attempt.result == Correct ] []
                    , if model.attempt.result == Correct then
                        Html.button [ Html.Attributes.type_ "button", Html.Events.onClick Continue ] [ Html.text "Continue!" ]

                      else
                        Html.button [ Html.Attributes.type_ "submit", Html.Attributes.disabled <| String.isEmpty attempt.input ] [ Html.text "Submit" ]
                    ]
                , resultView attempt.result
                ]

            WhatDoesWordMean ->
                [ kanjiDisplay model.word.str
                , Html.div [] [ Html.text <| "Your word is " ++ model.word.str, Html.span [ Html.Attributes.style "white-space" "nowrap" ] [ Html.text <| "(" ++ model.word.kana ++ ")" ] ]
                , Html.div [ Html.Attributes.style "overflow" "auto" ]
                    [ Html.ul [ Html.Attributes.class "hidden-glossary-list" ]
                        (List.indexedMap
                            (\i ->
                                \meaning ->
                                    let
                                        visible =
                                            Set.member i model.showGlossaryAtIndex
                                    in
                                    Html.li [ Html.Attributes.id <| "glossary-item-" ++ String.fromInt (i + 1), Html.Attributes.classList [ ( "visible", visible ) ], Html.Events.onClick (RevealGlossaryWord i) ]
                                        [ Html.text
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
                , Html.div
                    [ Html.Attributes.style "flex-grow" "1" ]
                    []
                , Html.form [ Html.Events.onSubmit Submit, Html.Attributes.style "display" "flex", Html.Attributes.style "flex-direction" "column", Html.Attributes.style "gap" "10px" ]
                    [ Html.label [ Html.Attributes.for "input-field" ] [ Html.text <| "Enter one of the glossary words" ]
                    , Html.input [ Html.Attributes.id "input-field", Html.Attributes.attribute "aria-label" "input-field", Html.Attributes.type_ "text", Html.Events.onInput Input, Html.Attributes.value attempt.input, Html.Attributes.autofocus True, Html.Attributes.disabled <| model.attempt.result == Correct ] []
                    , if model.attempt.result == Correct then
                        Html.button [ Html.Attributes.type_ "button", Html.Events.onClick Continue ] [ Html.text "Continue!" ]

                      else
                        Html.button [ Html.Attributes.type_ "submit", Html.Attributes.disabled <| String.isEmpty attempt.input ] [ Html.text "Submit" ]
                    ]
                , resultView attempt.result
                ]
        )


kanjiDisplay : String -> Html msg
kanjiDisplay kanji =
    Html.h1
        [ Html.Attributes.style "font-size" ("min(calc(100cqw / " ++ String.fromInt (String.length kanji) ++ " - 10px), calc(50cqw - 10px))")
        , Html.Attributes.style "text-align" "center"
        , Html.Attributes.style "line-height" "1"
        ]
        [ Html.text kanji ]


resultView : Result -> Html msg
resultView res =
    Html.p []
        [ case res of
            Correct ->
                Html.text "That's correct!"

            Incorrect ->
                Html.text "Sorry, that's not the right answer!"

            Undecided ->
                Html.text ""
        ]


type MoraResult
    = CorrectMora
    | IncorrectMora


{-| Validates attempt and give result per hiragana/kana character
-}
getResultPerMora : (Romaji.CharacterMapping -> String) -> String -> List Romaji.CharacterMapping -> List ( String, MoraResult )
getResultPerMora field attempt correct =
    List.foldl
        (\character ->
            \( att, acc ) ->
                let
                    str =
                        field character

                    length =
                        String.length str

                    chars =
                        String.left length att

                    remaining =
                        String.dropLeft length att
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
        |> List.reverse


romajiToMora : String -> List Romaji.CharacterMapping -> List ( String, MoraResult )
romajiToMora =
    getResultPerMora .romaji


hiraganaToMora : String -> List Romaji.CharacterMapping -> List ( String, MoraResult )
hiraganaToMora =
    getResultPerMora .mora
