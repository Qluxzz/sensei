module Main exposing (main)

import Array
import Browser
import Game
import Html exposing (..)
import Random
import Words exposing (amountOfWords, words)


main : Program () Model Msg
main =
    Browser.document
        { init = \_ -> init
        , view =
            \model ->
                { title = "Sensei"
                , body = [ view model ]
                }
        , update = update
        , subscriptions = \_ -> Sub.none
        }


type alias Model =
    { state : State
    }


type State
    = Loading
    | Playing Game.Model
    | Failed String


init : ( Model, Cmd Msg )
init =
    ( { state = Loading }, randomWordIndex )


randomWordIndex : Cmd Msg
randomWordIndex =
    Random.generate GetEntryId (Random.int 0 amountOfWords)


type Msg
    = GetEntryId Int
    | GameMsg Game.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model.state of
        Loading ->
            case msg of
                GetEntryId index ->
                    let
                        word =
                            Array.get index words
                    in
                    case word of
                        Just w ->
                            let
                                ( gameModel, cmd ) =
                                    Game.init w
                            in
                            ( { state = Playing gameModel }, Cmd.map GameMsg cmd )

                        _ ->
                            ( { state = Failed "Failed to get word" }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        Playing m ->
            case msg of
                GameMsg gameMsg ->
                    let
                        ( updatedModel, cmd ) =
                            Game.update gameMsg m
                    in
                    if gameMsg == Game.NextWord then
                        ( { state = Loading }, randomWordIndex )

                    else
                        ( { state = Playing updatedModel }, Cmd.map GameMsg cmd )

                _ ->
                    ( model, Cmd.none )

        Failed _ ->
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    case model.state of
        Playing gameModel ->
            Html.map GameMsg (Game.view gameModel)

        Loading ->
            text "Loading word..."

        Failed errMsg ->
            text errMsg
