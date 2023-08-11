module ValidateRomaji exposing (..)

import Expect
import Game
import Romaji exposing (CharacterMapping)
import Test exposing (Test, describe, test)


cases : List ( ( String, List CharacterMapping ), List ( String, Game.MoraResult ) )
cases =
    [ ( ( "kanmen"
        , [ { mora = "か", romaji = "ka" }, { mora = "ん", romaji = "n" }, { mora = "も", romaji = "mo" }, { mora = "ん", romaji = "n" } ]
        )
      , [ ( "か", Game.CorrectMora ), ( "ん", Game.CorrectMora ), ( "も", Game.IncorrectMora ), ( "ん", Game.CorrectMora ) ]
      )
    ]


suite : Test
suite =
    describe "Validate attempt against correct per mora"
        (List.map
            (\( ( attempt, correct ), wanted ) ->
                test ("Attempt " ++ attempt ++ " should yield " ++ Debug.toString wanted) <|
                    \_ ->
                        Expect.equalLists (Game.getResultPerMora attempt correct) wanted
            )
            cases
        )
