module ValidateRomaji exposing (..)

import Expect
import Game
import Romaji exposing (CharacterMapping)
import Test exposing (Test, describe, test)


romajiToMoraCases : List ( ( String, List CharacterMapping ), List ( String, Game.MoraResult ) )
romajiToMoraCases =
    [ ( ( "kanmen"
        , [ { mora = "か", romaji = "ka" }, { mora = "ん", romaji = "n" }, { mora = "も", romaji = "mo" }, { mora = "ん", romaji = "n" } ]
        )
      , [ ( "か", Game.CorrectMora ), ( "ん", Game.CorrectMora ), ( "も", Game.IncorrectMora ), ( "ん", Game.CorrectMora ) ]
      )
    ]


hiraganaToMoraCases : List ( ( String, List CharacterMapping ), List ( String, Game.MoraResult ) )
hiraganaToMoraCases =
    [ ( ( "かんもん"
        , [ { mora = "か", romaji = "ka" }, { mora = "ん", romaji = "n" }, { mora = "も", romaji = "mo" }, { mora = "ん", romaji = "n" } ]
        )
      , [ ( "か", Game.CorrectMora ), ( "ん", Game.CorrectMora ), ( "も", Game.CorrectMora ), ( "ん", Game.CorrectMora ) ]
      )
    , ( ( "あっしゅく"
        , [ { mora = "あ", romaji = "a" }, { mora = "っしゅ", romaji = "sshu" }, { mora = "く", romaji = "ku" } ]
        )
      , [ ( "あ", Game.CorrectMora ), ( "っしゅ", Game.CorrectMora ), ( "く", Game.CorrectMora ) ]
      )
    ]


suite : Test
suite =
    describe "Validate attempt"
        [ describe "Romaji to mora"
            (List.map
                (\( ( attempt, correct ), wanted ) ->
                    test ("Attempt " ++ attempt ++ " should yield " ++ Debug.toString wanted) <|
                        \_ ->
                            Expect.equalLists (Game.romajiToMora attempt correct) wanted
                )
                romajiToMoraCases
            )
        , describe "Hiragana to mora"
            (List.map
                (\( ( attempt, correct ), wanted ) ->
                    test ("Attempt " ++ attempt ++ " should yield " ++ Debug.toString wanted) <|
                        \_ ->
                            Expect.equalLists (Game.hiraganaToMora attempt correct) wanted
                )
                hiraganaToMoraCases
            )
        ]
