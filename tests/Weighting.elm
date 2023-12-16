module Weighting exposing (..)

import Dict
import Expect
import Game
import Main exposing (updateWeight, weigh)
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Test weighting of words"
        [ test "Weighing a word against no weights, returns default weight" <|
            \_ -> weigh Dict.empty "おかね" |> Expect.equal 1.0
        , test "Weighing a word against weights, weighs the word correctly" <|
            \_ -> weigh (Dict.fromList [ ( "お", 0.1 ) ]) "おかね" |> Expect.within (Expect.Absolute 0.0000001) 0.1
        , test "Weighing a word against weights, weighs the word correctly 2" <|
            \_ -> weigh (Dict.fromList [ ( "お", 0.1 ), ( "か", 0.6 ) ]) "おかね" |> Expect.within (Expect.Absolute 0.0000001) 0.06
        , describe "Updating the weight of a mora works"
            [ test "When the user answers correctly the weight is increased" <|
                \_ ->
                    updateWeight ( "お", Game.CorrectMora ) (Dict.fromList [ ( "お", 0.1 ) ])
                        |> Expect.equalDicts (Dict.fromList [ ( "お", 0.2 ) ])
            , test "When the user answers incorrectly the weight is decreased" <|
                \_ ->
                    updateWeight ( "お", Game.IncorrectMora ) (Dict.fromList [ ( "お", 0.5 ) ])
                        |> Expect.equalDicts (Dict.fromList [ ( "お", 0.4 ) ])
            ]
        ]
