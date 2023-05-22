module ConvertWord exposing (..)

import Expect
import Romaji exposing (convertWord)
import Test exposing (..)


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
