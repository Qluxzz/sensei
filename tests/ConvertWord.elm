module ConvertWord exposing (suite, suite2)

import Expect
import Romaji exposing (convertWord, groupByMora)
import Test exposing (Test, describe, test)


type alias Input =
    String


type alias Expected =
    String


cases : List ( Input, Expected )
cases =
    [ ( "いっとう", "ittou" )
    , ( "あうんのこきゅう", "aunnokokyuu" )
    ]


suite : Test
suite =
    describe "Convert word to hiragana and or katakana"
        (List.map
            (\( input, expected ) ->
                test (input ++ " = " ++ expected) <|
                    \_ ->
                        convertWord input
                            |> Expect.equal expected
            )
            cases
        )


cases2 : List ( Input, Result String (List ( String, String )) )
cases2 =
    [ ( "いっとう", Ok [ ( "い", "i" ), ( "っと", "tto" ), ( "う", "u" ) ] )
    , ( "あうんのこきゅう", Ok [ ( "あ", "a" ), ( "う", "u" ), ( "ん", "n" ), ( "の", "no" ), ( "こ", "ko" ), ( "きゅ", "kyu" ), ( "う", "u" ) ] )
    , ( "いんしゅ", Ok [ ( "い", "i" ), ( "ん", "n" ), ( "しゅ", "shu" ) ] )
    , ( "ん", Ok [ ( "ん", "n" ) ] )
    , ( "abc", Err "Failed to find romaji for 'a'" )
    ]


suite2 : Test
suite2 =
    describe "Get romaji per mora in word"
        (List.map
            (\( input, expected ) ->
                test input <|
                    \_ ->
                        groupByMora input
                            |> Expect.equal expected
            )
            cases2
        )
