module Weighting exposing (suite)

import Dict
import Expect
import Game
import Main
import Test exposing (Test)


suite : Test
suite =
    Test.describe "Test weighting of words"
        [ Test.test "Weighing a word against no weights, returns default weight" <|
            \_ -> Main.weigh Dict.empty "おかね" |> Expect.equal 1.0
        , Test.test "Weighing a word against weights, weighs the word correctly" <|
            \_ -> Main.weigh (Dict.fromList [ ( "お", 0.1 ) ]) "おかね" |> Expect.within (Expect.Absolute 0.0000001) 0.1
        , Test.test "Weighing a word against weights, weighs the word correctly 2" <|
            \_ -> Main.weigh (Dict.fromList [ ( "お", 0.1 ), ( "か", 0.6 ) ]) "おかね" |> Expect.within (Expect.Absolute 0.0000001) 0.06
        , Test.describe "Updating the weight of a mora works"
            [ Test.test "When the user answers correctly the weight is increased" <|
                \_ ->
                    Main.updateWeight ( "お", Game.CorrectMora ) (Dict.fromList [ ( "お", 0.1 ) ])
                        |> Expect.equalDicts (Dict.fromList [ ( "お", 0.2 ) ])
            , Test.test "When the user answers incorrectly the weight is decreased" <|
                \_ ->
                    Main.updateWeight ( "お", Game.IncorrectMora ) (Dict.fromList [ ( "お", 0.5 ) ])
                        |> Expect.equalDicts (Dict.fromList [ ( "お", 0.4 ) ])
            , Test.test "If weight didn't exist before, and the answer was incorrect, correct weight is set" <|
                \_ ->
                    Main.updateWeight ( "お", Game.IncorrectMora ) Dict.empty
                        |> Expect.equalDicts (Dict.fromList [ ( "お", 0.1 ) ])
            , Test.test "If weight didn't exist before, and the answer was correct, correct weight is set" <|
                \_ ->
                    Main.updateWeight ( "お", Game.CorrectMora ) Dict.empty
                        |> Expect.equalDicts (Dict.fromList [ ( "お", 1.0 ) ])
            ]
        ]
