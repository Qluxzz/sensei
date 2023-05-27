module ConvertWord exposing (suite, suite2)

import Array
import Expect
import Romaji exposing (CharacterMapping, groupByMora)
import Test exposing (Test, describe, test)
import Words exposing (words)


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
    , ( "abc", Err "Failed to group word 'abc' by mora\n. Inner error: Failed to find romaji for 'a'" )
    , ( "とっきょちょう"
      , Ok
            [ { mora = "と", romaji = "to" }
            , { mora = "っきょ", romaji = "kkyo" }
            , { mora = "ちょ", romaji = "cho" }
            , { mora = "う", romaji = "u" }
            ]
      )
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


suite2 : Test
suite2 =
    describe "No word should fail to be grouped by mora"
        (Array.indexedMap
            (\i ->
                \{ normalized } ->
                    -- Multiples of the same word can appear as a verb or a noun
                    -- so we append the index to avoid "same test name error"
                    test (String.fromInt i ++ ": " ++ normalized) <| \_ -> groupByMora normalized |> Expect.ok
            )
            words
            |> Array.toList
        )
