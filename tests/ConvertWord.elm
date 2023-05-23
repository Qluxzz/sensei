module ConvertWord exposing (suite)

import Expect
import Romaji exposing (CharacterMapping, groupByMora)
import Test exposing (Test, describe, test)


cases : List ( String, Result String (List CharacterMapping) )
cases =
    [ ( "いっとう"
      , Ok
            [ { mora = "い", romaji = "i" }
            , { mora = "っと", romaji = "tto" }
            , { mora = "う", romaji = "u" }
            ]
      )
    , ( "あうんのこきゅう"
      , Ok
            [ { mora = "あ", romaji = "a" }
            , { mora = "う", romaji = "u" }
            , { mora = "ん", romaji = "n" }
            , { mora = "の", romaji = "no" }
            , { mora = "こ", romaji = "ko" }
            , { mora = "きゅ", romaji = "kyu" }
            , { mora = "う", romaji = "u" }
            ]
      )
    , ( "いんしゅ"
      , Ok
            [ { mora = "い", romaji = "i" }
            , { mora = "ん", romaji = "n" }
            , { mora = "しゅ", romaji = "shu" }
            ]
      )
    , ( "ん", Ok [ { mora = "ん", romaji = "n" } ] )
    , ( "abc", Err "Failed to find romaji for 'a'" )
    ]


suite : Test
suite =
    describe "Get romaji per mora in word"
        (List.map
            (\( input, expected ) ->
                test input <|
                    \_ ->
                        groupByMora input
                            |> Expect.equal expected
            )
            cases
        )
