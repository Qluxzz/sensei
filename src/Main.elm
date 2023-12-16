port module Main exposing (main, updateWeight, weigh)

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
import Romaji exposing (groupByMora, hiragana)
import Set
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
            ( { state = Loading, weights = weights }, getNextWord weights )

        Nothing ->
            ( { state = Loading, weights = Dict.empty }, randomWordIndex amountOfWords )


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
                            ( { model | state = Loading }, getNextWord model.weights )

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
getIdsOfWordsUserShouldTrainOn : Dict.Dict String Float -> List Int
getIdsOfWordsUserShouldTrainOn weights =
    getIdsOfMatchingWords words (\{ kana } -> weigh weights kana < 1)


{-| Get words including characters given in list
-}
getIdsOfWordsThatIncludesChars : List String -> List Int
getIdsOfWordsThatIncludesChars characters =
    getIdsOfMatchingWords words (\{ kana } -> List.any (\s -> String.contains s kana) characters)


getIdsOfMatchingWords : Array.Array Words.Word -> (Words.Word -> Bool) -> List Int
getIdsOfMatchingWords words isGood =
    words
        |> Array.toIndexedList
        |> List.foldr
            (\( id, word ) ->
                \acc ->
                    if isGood word then
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
                        clamp
                            0.1
                            1.0
                            (case result of
                                Game.CorrectMora ->
                                    e + 0.1

                                Game.IncorrectMora ->
                                    e - 0.1
                            )

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


type NextWordType
    = Random -- User has weights for all chars and all have a weight of 1
    | Untrained -- User has no weight for some chars
    | Weighted -- User has a weight of < 1 for some chars


getNextWord : Dict.Dict String Float -> Cmd Msg
getNextWord weights =
    let
        typeOfNextWord =
            -- We have no weights, just shuffle the next word
            if Dict.isEmpty weights then
                Random

            else if weights |> Dict.values |> List.all ((==) 1.0) then
                -- User has a weight for all chars
                if weights |> Dict.size |> (==) (List.length hiragana) then
                    Random

                else
                    Untrained

            else
                Weighted
    in
    case typeOfNextWord of
        Random ->
            randomWordIndex amountOfWords

        Weighted ->
            let
                ids =
                    getIdsOfWordsUserShouldTrainOn weights
            in
            randomIdFromList ids

        Untrained ->
            let
                trainedCharacters =
                    Dict.keys weights |> Set.fromList

                untrainedChars =
                    Set.diff (Set.fromList Romaji.hiragana) trainedCharacters
                        |> Set.toList

                ids =
                    getIdsOfWordsThatIncludesChars untrainedChars
            in
            randomIdFromList ids
