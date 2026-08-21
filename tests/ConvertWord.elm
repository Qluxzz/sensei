module ConvertWord exposing (suite)

import Array
import Expect
import Romaji
import Test exposing (Test)
import Words


cases : List ( String, Result String (List Romaji.CharacterMapping) )
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
    , ( "abc", Err "Failed to group word 'abc' by mora\n. Inner error: Failed to find romaji for 'c'" )
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
    Test.describe "Convert words to list of mora and romaji representation"
        [ Test.describe "Get romaji per mora in word"
            (List.map
                (\( input, expected ) ->
                    Test.test input <|
                        \_ ->
                            Romaji.groupByMora input
                                |> Expect.equal expected
                )
                cases
            )
        , Test.describe "No word should fail to be grouped by mora"
            (Array.indexedMap
                (\i ->
                    \{ kana } ->
                        -- Multiples of the same word can appear as a verb or a noun
                        -- so we append the index to avoid "same test name error"
                        Test.test (String.fromInt i ++ ": " ++ kana) <| \_ -> Romaji.groupByMora kana |> Expect.ok
                )
                Words.words
                |> Array.toList
            )
        ]
