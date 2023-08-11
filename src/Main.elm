port module Main exposing (main)

import Array
import Browser
import Dict
import Game
import Html exposing (Html, div, header, text)
import Html.Attributes exposing (class)
import Json.Decode
import Json.Encode
import Maybe
import Random
import Random.Extra
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
                -- If all existing weights are at 1, meaning the user has guessed everything correctly
                -- we just continue to randomize the next word
                randomize =
                    weights |> Dict.values |> List.all ((==) 1)
            in
            if randomize then
                ( { state = Loading, weights = weights }, randomWordIndex amountOfWords )

            else
                let
                    ids =
                        getIdsOfWordsUserShouldTrainOn words weights
                in
                ( { state = Loading, weights = weights }, randomIdFromList ids )

        Nothing ->
            ( { state = Loading
              , weights = flags.weights |> Maybe.withDefault Dict.empty
              }
            , randomWordIndex amountOfWords
            )


type Msg
    = RandomWordIndex Int
    | GameMsg Game.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model.state of
        Loading ->
            case msg of
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
                                    List.foldr updateWeight model.weights result
                            in
                            ( { model | state = Playing updatedModel, weights = updatedWeights }
                            , Cmd.batch
                                [ Cmd.map GameMsg cmd
                                , persistStatistics (statisticsEncoder { weights = updatedWeights })
                                ]
                            )

                        Just Game.NextWord ->
                            let
                                -- If all existing weights are at 1, meaning the user nails all kanas
                                -- we just continue to randomize the next word
                                randomize =
                                    model.weights |> Dict.values |> List.all ((==) 1)
                            in
                            if randomize then
                                ( { model | state = Loading }, randomWordIndex amountOfWords )

                            else
                                let
                                    ids =
                                        getIdsOfWordsUserShouldTrainOn words model.weights
                                in
                                ( { model | state = Loading }, randomIdFromList ids )

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


{-| Returns all words with a less than 1 ratio, meaning the user has failed some kana in the word
-}
getIdsOfWordsUserShouldTrainOn : Array.Array Words.Word -> Dict.Dict String Float -> List Int
getIdsOfWordsUserShouldTrainOn words weights =
    words
        |> Array.toIndexedList
        |> List.foldr
            (\( id, word ) ->
                \acc ->
                    if weigh weights word.kana < 1 then
                        id :: acc

                    else
                        acc
            )
            []


updateWeight :
    ( String, Game.MoraResult )
    -> Dict.Dict String Float
    -> Dict.Dict String Float
updateWeight ( kana, result ) weights =
    Dict.update
        kana
        (\maybeExisting ->
            Just
                (case maybeExisting of
                    Just e ->
                        case result of
                            Game.CorrectMora ->
                                min 1.0 (e + 0.1)

                            Game.IncorrectMora ->
                                max 0.0 (e - 0.1)

                    Nothing ->
                        case result of
                            Game.CorrectMora ->
                                1.0

                            Game.IncorrectMora ->
                                0.1
                )
        )
        weights


randomWordIndex : Int -> Cmd Msg
randomWordIndex amount =
    Random.generate RandomWordIndex (Random.int 0 amount)


randomIdFromList : List Int -> Cmd Msg
randomIdFromList ids =
    case ids of
        [] ->
            randomWordIndex amountOfWords

        _ ->
            Random.generate RandomWordIndex
                (Random.Extra.sample ids
                    -- This should never happen since we check if the list is empty before
                    |> Random.map (Maybe.withDefault 0)
                )
