module ValidateRomaji exposing (hiraganaToMoraCases, romajiToMoraCases, suite)

import Expect
import Game
import Romaji
import Test exposing (Test)


romajiToMoraCases : List ( ( String, List Romaji.CharacterMapping ), List ( String, Game.MoraResult ) )
romajiToMoraCases =
    [ ( ( "kanmen"
        , [ { mora = "か", romaji = "ka" }, { mora = "ん", romaji = "n" }, { mora = "も", romaji = "mo" }, { mora = "ん", romaji = "n" } ]
        )
      , [ ( "か", Game.CorrectMora ), ( "ん", Game.CorrectMora ), ( "も", Game.IncorrectMora ), ( "ん", Game.CorrectMora ) ]
      )
    , ( ( "kan"
        , [ { mora = "か", romaji = "ka" }, { mora = "ん", romaji = "n" }, { mora = "も", romaji = "mo" }, { mora = "ん", romaji = "n" } ]
        )
      , [ ( "か", Game.CorrectMora ), ( "ん", Game.CorrectMora ), ( "も", Game.IncorrectMora ), ( "ん", Game.IncorrectMora ) ]
      )
    ]


hiraganaToMoraCases : List ( ( String, List Romaji.CharacterMapping ), List ( String, Game.MoraResult ) )
hiraganaToMoraCases =
    [ ( ( "かんもん"
        , [ { mora = "か", romaji = "ka" }, { mora = "ん", romaji = "n" }, { mora = "も", romaji = "mo" }, { mora = "ん", romaji = "n" } ]
        )
      , [ ( "か", Game.CorrectMora ), ( "ん", Game.CorrectMora ), ( "も", Game.CorrectMora ), ( "ん", Game.CorrectMora ) ]
      )
    , ( ( "かん"
        , [ { mora = "か", romaji = "ka" }, { mora = "ん", romaji = "n" }, { mora = "も", romaji = "mo" }, { mora = "ん", romaji = "n" } ]
        )
      , [ ( "か", Game.CorrectMora ), ( "ん", Game.CorrectMora ), ( "も", Game.IncorrectMora ), ( "ん", Game.IncorrectMora ) ]
      )
    , ( ( "あっしゅく"
        , [ { mora = "あ", romaji = "a" }, { mora = "っしゅ", romaji = "sshu" }, { mora = "く", romaji = "ku" } ]
        )
      , [ ( "あ", Game.CorrectMora ), ( "っしゅ", Game.CorrectMora ), ( "く", Game.CorrectMora ) ]
      )
    ]


suite : Test
suite =
    Test.describe "Validate attempt"
        [ Test.describe "Romaji to mora"
            (List.map
                (\( ( attempt, correct ), wanted ) ->
                    Test.test ("Attempt " ++ attempt ++ " should yield " ++ Debug.toString wanted) <|
                        \_ ->
                            Expect.equalLists (Game.romajiToMora attempt correct) wanted
                )
                romajiToMoraCases
            )
        , Test.describe "Hiragana to mora"
            (List.map
                (\( ( attempt, correct ), wanted ) ->
                    Test.test ("Attempt " ++ attempt ++ " should yield " ++ Debug.toString wanted) <|
                        \_ ->
                            Expect.equalLists (Game.hiraganaToMora attempt correct) wanted
                )
                hiraganaToMoraCases
            )
        ]
