module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (placeholder, style, type_)
import Html.Events exposing (onInput, onSubmit)
import Romaji exposing (convertWord)


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , view = view
        , update = update
        }


type alias Word =
    { kanji : String
    , hiraganaKatakana : String
    , description : List String
    }


type Result
    = Undecided
    | Correct
    | Incorrect String


type alias Model =
    { word : Word
    , attempt : String
    , result : Result
    }


init : Model
init =
    Model
        { kanji = "阿吽の呼吸"
        , hiraganaKatakana = "あうんのこきゅう"
        , description =
            [ "the harmonizing, mentally and physically, of two parties engaged in an activity"
            , "singing from the same hymn-sheet"
            , "dancing to the same beat"
            ]
        }
        ""
        Undecided


type Msg
    = SubmitAttempt
    | Input String


update : Msg -> Model -> Model
update msg model =
    case msg of
        SubmitAttempt ->
            if model.attempt == convertWord model.word.hiraganaKatakana then
                { model | result = Correct }

            else
                { model | result = Incorrect <| "Sorry, that's not right\nIt should be: " ++ convertWord model.word.hiraganaKatakana }

        Input str ->
            { model | attempt = str }


view : Model -> Html Msg
view model =
    form [ onSubmit SubmitAttempt ]
        [ p []
            [ text <| "Your word is " ++ model.word.kanji
            , br [] []
            , ul [] (List.map (\meaning -> li [] [ text meaning ]) model.word.description)
            , br [] []
            , text <| model.word.hiraganaKatakana
            , br [] []
            , div [ style "display" "flex", style "gap" "10px" ]
                [ p [] [ text "Enter romaji of above word" ]
                , input [ type_ "text", onInput Input ] []
                , case model.result of
                    Correct ->
                        text "Success!"

                    Incorrect txt ->
                        text txt

                    Undecided ->
                        text ""
                ]
            ]
        ]
