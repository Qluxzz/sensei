module Main exposing (main)

import Array
import Browser
import Game
import Html exposing (Html, div, header, text)
import Html.Attributes exposing (class)
import Random
import Words exposing (amountOfWords, words)


main : Program () Model Msg
main =
    Browser.document
        { init = \_ -> init
        , view =
            \model ->
                { title = "Sensei"
                , body = view model
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
                    case
                        Array.get index words
                            |> Result.fromMaybe ("Failed to find word with id " ++ String.fromInt index)
                            |> Result.andThen Game.init
                    of
                        Ok ( gameModel, cmd ) ->
                            ( { state = Playing gameModel }, Cmd.map GameMsg cmd )

                        Err err ->
                            ( { state = Failed err }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        Playing gameModel ->
            case msg of
                GameMsg gameMsg ->
                    let
                        ( updatedModel, cmd, outMsg ) =
                            Game.update gameMsg gameModel
                    in
                    case outMsg of
                        Just Game.NextWord ->
                            ( { state = Loading }, randomWordIndex )

                        Nothing ->
                            ( { state = Playing updatedModel }, Cmd.map GameMsg cmd )

                _ ->
                    ( model, Cmd.none )

        Failed _ ->
            ( model, Cmd.none )


baseView : Html Msg -> List (Html Msg)
baseView content =
    [ header [] [ text "Sensei" ]
    , div [ class "container" ] [ content ]
    ]


view : Model -> List (Html Msg)
view model =
    baseView
        (case model.state of
            Playing gameModel ->
                Html.map GameMsg (Game.view gameModel)

            Loading ->
                text "Loading word..."

            Failed errMsg ->
                text errMsg
        )
