module Game exposing (..)

import Html exposing (Html, br, div, form, input, li, p, text, ul)
import Html.Attributes exposing (style, type_)
import Html.Events exposing (onInput, onSubmit)
import Romaji exposing (convertWord)
import Words exposing (Word)


init : Word -> ( Model, Cmd Msg )
init word =
    ( Model
        word
        ""
        Undecided
    , Cmd.none
    )


type Result
    = Undecided
    | Correct
    | Incorrect String


type alias Model =
    { word : Word
    , attempt : String
    , result : Result
    }


type Msg
    = SubmitAttempt
    | Input String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SubmitAttempt ->
            if model.attempt == convertWord model.word.normalized then
                ( { model | result = Correct }, Cmd.none )

            else
                ( { model | result = Incorrect <| "Sorry, that's not right\nIt should be: " ++ convertWord model.word.normalized }, Cmd.none )

        Input str ->
            ( { model | attempt = str }, Cmd.none )


view : Model -> Html Msg
view model =
    form [ onSubmit SubmitAttempt ]
        [ p []
            [ text <| "Your word is " ++ model.word.str
            , br [] []
            , text <| model.word.str ++ " means:"
            , ul [] (List.map (\meaning -> li [] [ text meaning ]) model.word.glossary)
            , br [] []
            , text <| model.word.normalized
            , br [] []
            , div [ style "display" "flex", style "gap" "10px" ]
                [ p [] [ text "Enter romaji of above word" ]
                , input [ type_ "text", onInput Input ] []
                ]
            , br [] []
            , case model.result of
                Correct ->
                    text "Success!"

                Incorrect txt ->
                    text txt

                Undecided ->
                    text ""
            ]
        ]
