module ValidateRomaji exposing (..)

import Expect
import Game
import Romaji exposing (CharacterMapping)
import Test exposing (Test, describe, test)


cases : List ( ( String, List CharacterMapping ), List ( String, Game.Result ) )
cases =
    [ ( ( "kanmen"
        , [ { mora = "か", romaji = "ka" }, { mora = "ん", romaji = "n" }, { mora = "も", romaji = "mo" }, { mora = "ん", romaji = "n" } ]
        )
      , [ ( "か", Game.Correct ), ( "ん", Game.Correct ), ( "も", Game.Incorrect ), ( "ん", Game.Correct ) ]
      )
    ]


suite : Test
suite =
    describe "When user writes answer in romaji, split each mora and compare if correct or not"
        (List.map
            (\( ( attempt, correct ), wanted ) ->
                test ("Attempt " ++ attempt ++ " should yield " ++ Debug.toString wanted) <|
                    \_ ->
                        Expect.equalLists (Game.getResultPerMora attempt correct) wanted
            )
            cases
        )
