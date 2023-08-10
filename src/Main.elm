port module Main exposing (main)

import Array
import Browser
import Dict
import Game
import Html exposing (Html, div, header, text)
import Html.Attributes exposing (class)
import Json.Decode
import Json.Encode
import Random
import Romaji exposing (groupByMora)
import Words exposing (amountOfWords, words)


main : Program Json.Decode.Value Model Msg
main =
    Browser.document
        { init =
            \v ->
                Json.Decode.decodeValue flagsDecoder v
                    |> Result.withDefault { weights = Nothing }
                    |> init
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
    , weights : Dict.Dict String Float -- Weights per mora to give user more relevant words depending on which mora they need to train more
    }


type State
    = Loading
    | Playing Game.Model
    | Failed String


init : Flags -> ( Model, Cmd Msg )
init flags =
    case flags.weights of
        Just weights ->
            let
                sorted =
                    sortWordsByWeight words weights

                -- TODO: This should not return the same word every time,
                -- keep track of which words the user have tried so they get some alternation
                newWord =
                    sorted
                        |> List.head
                        |> Result.fromMaybe "Failed to get word"
                        |> Result.andThen Game.init
            in
            case newWord of
                Ok ( newGameModel, newGameCmd ) ->
                    ( { state = Playing newGameModel, weights = weights }
                    , Cmd.batch
                        [ Cmd.map GameMsg newGameCmd
                        ]
                    )

                Err err ->
                    ( { state = Failed err, weights = weights }, Cmd.none )

        Nothing ->
            ( { state = Loading
              , weights = flags.weights |> Maybe.withDefault Dict.empty
              }
            , randomWordIndex
            )


type Msg
    = RandomWordIndex Int
    | GameMsg Game.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model.state of
        Loading ->
            case msg of
                -- This should only happen once when the user first visits the page
                -- After this we should have stored some weights data to give better words to the user
                RandomWordIndex index ->
                    case
                        Array.get index words
                            |> Result.fromMaybe ("Failed to find word with id " ++ String.fromInt index)
                            |> Result.andThen Game.init
                    of
                        Ok ( gameModel, cmd ) ->
                            ( { model | state = Playing gameModel }, Cmd.map GameMsg cmd )

                        Err err ->
                            ( { model | state = Failed err }, Cmd.none )

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
                        Just (Game.RomajiAttemptResult result) ->
                            let
                                updatedWeights =
                                    updateWeights result model.weights
                            in
                            ( { model | state = Playing updatedModel, weights = updatedWeights }
                            , Cmd.batch
                                [ Cmd.map GameMsg cmd
                                , persistStatistics (statisticsEncoder { weights = updatedWeights })
                                ]
                            )

                        Just Game.NextWord ->
                            let
                                sorted =
                                    sortWordsByWeight words model.weights

                                -- TODO: This should not return the same word every time,
                                -- keep track of which words the user have tried so they get some alternation
                                newWord =
                                    sorted
                                        |> List.head
                                        |> Result.fromMaybe "Failed to get word"
                                        |> Result.andThen Game.init
                            in
                            case newWord of
                                Ok ( newGameModel, newGameCmd ) ->
                                    ( { model | state = Playing newGameModel }
                                    , Cmd.batch
                                        [ Cmd.map GameMsg newGameCmd
                                        ]
                                    )

                                Err err ->
                                    ( { model | state = Failed err }, Cmd.none )

                        Nothing ->
                            ( { model | state = Playing updatedModel }, Cmd.map GameMsg cmd )

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



-- PORTS


port persistStatistics : Json.Encode.Value -> Cmd msg



-- HELPERS


type alias Statistics =
    { weights : Dict.Dict String Float }


statisticsEncoder : Statistics -> Json.Encode.Value
statisticsEncoder statistics =
    Json.Encode.object
        [ ( "weights"
          , Json.Encode.dict identity Json.Encode.float statistics.weights
          )
        ]


type alias Flags =
    { weights : Maybe (Dict.Dict String Float) }


flagsDecoder : Json.Decode.Decoder Flags
flagsDecoder =
    Json.Decode.map Flags
        (Json.Decode.maybe (Json.Decode.field "weights" (Json.Decode.dict Json.Decode.float)))


{-| Weighs a word according to a list of weights
-}
weigh : Dict.Dict String Float -> String -> Float
weigh weights word =
    groupByMora word
        |> Result.map
            (List.foldr
                (\group ->
                    (*) (Dict.get group.mora weights |> Maybe.withDefault 1.0)
                )
                1.0
            )
        |> Result.withDefault 1.0


sortWordsByWeight : Array.Array Words.Word -> Dict.Dict String Float -> List Words.Word
sortWordsByWeight words weights =
    words
        |> Array.toList
        |> List.map (\w -> ( weigh weights w.kana, w ))
        |> List.sortBy Tuple.first
        |> List.map Tuple.second


updateWeights : List ( String, Game.Result ) -> Dict.Dict String Float -> Dict.Dict String Float
updateWeights results weights =
    List.foldr (\res -> \acc -> updateWeight acc res) weights results


updateWeight :
    Dict.Dict String Float
    -> ( String, Game.Result )
    -> Dict.Dict String Float
updateWeight weights ( kana, result ) =
    Dict.update
        kana
        (\maybeExisting ->
            Just
                (case maybeExisting of
                    Just e ->
                        case result of
                            Game.Correct ->
                                min 1.0 (e + (e * 0.1))

                            Game.Incorrect ->
                                max 0.0 (e - (e * 0.1))

                            Game.Undecided ->
                                e

                    Nothing ->
                        case result of
                            Game.Correct ->
                                1.0

                            Game.Incorrect ->
                                0.1

                            Game.Undecided ->
                                1.0
                )
        )
        weights


randomWordIndex : Cmd Msg
randomWordIndex =
    Random.generate RandomWordIndex (Random.int 0 amountOfWords)
