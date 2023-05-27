module Romaji exposing (CharacterMapping, groupByMora)

import Dict exposing (Dict)


hiraganaToRomaji : Dict String String
hiraganaToRomaji =
    Dict.fromList
        [ ( "あ", "a" )
        , ( "い", "i" )
        , ( "う", "u" )
        , ( "え", "e" )
        , ( "お", "o" )
        , ( "か", "ka" )
        , ( "き", "ki" )
        , ( "く", "ku" )
        , ( "け", "ke" )
        , ( "こ", "ko" )
        , ( "さ", "sa" )
        , ( "し", "shi" )
        , ( "す", "su" )
        , ( "せ", "se" )
        , ( "そ", "so" )
        , ( "た", "ta" )
        , ( "ち", "chi" )
        , ( "つ", "tsu" )
        , ( "て", "te" )
        , ( "と", "to" )
        , ( "な", "na" )
        , ( "に", "ni" )
        , ( "ぬ", "nu" )
        , ( "ね", "ne" )
        , ( "の", "no" )
        , ( "は", "ha" )
        , ( "ひ", "hi" )
        , ( "ふ", "fu" )
        , ( "へ", "he" )
        , ( "ほ", "ho" )
        , ( "ま", "ma" )
        , ( "み", "mi" )
        , ( "む", "mu" )
        , ( "め", "me" )
        , ( "も", "mo" )
        , ( "や", "ya" )
        , ( "ゆ", "yu" )
        , ( "よ", "yo" )
        , ( "ら", "ra" )
        , ( "り", "ri" )
        , ( "る", "ru" )
        , ( "れ", "re" )
        , ( "ろ", "ro" )
        , ( "わ", "wa" )
        , ( "を", "wo" )
        , ( "ん", "n" )
        , ( "が", "ga" )
        , ( "ぎ", "gi" )
        , ( "ぐ", "gu" )
        , ( "げ", "ge" )
        , ( "ご", "go" )
        , ( "ざ", "za" )
        , ( "じ", "ji" )
        , ( "ず", "zu" )
        , ( "ぜ", "ze" )
        , ( "ぞ", "zo" )
        , ( "だ", "da" )
        , ( "ぢ", "ji" )
        , ( "づ", "zu" )
        , ( "で", "de" )
        , ( "ど", "do" )
        , ( "ば", "ba" )
        , ( "び", "bi" )
        , ( "ぶ", "bu" )
        , ( "べ", "be" )
        , ( "ぼ", "bo" )
        , ( "ぱ", "pa" )
        , ( "ぴ", "pi" )
        , ( "ぷ", "pu" )
        , ( "ぺ", "pe" )
        , ( "ぽ", "po" )
        , ( "きゃ", "kya" )
        , ( "きゅ", "kyu" )
        , ( "きょ", "kyo" )
        , ( "しゃ", "sha" )
        , ( "しゅ", "shu" )
        , ( "しょ", "sho" )
        , ( "ちゃ", "cha" )
        , ( "ちゅ", "chu" )
        , ( "ちょ", "cho" )
        , ( "にゃ", "nya" )
        , ( "にゅ", "nyu" )
        , ( "にょ", "nyo" )
        , ( "ひゃ", "hya" )
        , ( "ひゅ", "hyu" )
        , ( "ひょ", "hyo" )
        , ( "みゃ", "mya" )
        , ( "みゅ", "myu" )
        , ( "みょ", "myo" )
        , ( "りゃ", "rya" )
        , ( "りゅ", "ryu" )
        , ( "りょ", "ryo" )
        , ( "ぎゃ", "gya" )
        , ( "ぎゅ", "gyu" )
        , ( "ぎょ", "gyo" )
        , ( "じゃ", "ja" )
        , ( "じゅ", "ju" )
        , ( "じょ", "jo" )
        , ( "びゃ", "bya" )
        , ( "びゅ", "byu" )
        , ( "びょ", "byo" )
        , ( "ぴゃ", "pya" )
        , ( "ぴゅ", "pyu" )
        , ( "ぴょ", "pyo" )
        , ( "ぢ", "ji" )
        , ( "づ", "zu" )
        , ( "ぶ", "bu" )
        , ( "ぷ", "pu" )
        , ( "へ", "he" )
        , ( "べ", "be" )
        , ( "ぺ", "pe" )
        , ( "ほ", "ho" )
        , ( "ぼ", "bo" )
        , ( "ぽ", "po" )
        , ( "ぴ", "pi" )
        , ( "ぱ", "pa" )
        , ( "ゔ", "vu" )
        , ( "ゐ", "wi" )
        , ( "ゑ", "we" )
        , ( "ん", "n" )
        , ( "ゝ", "" )
        , ( "ゞ", "" )
        , ( "ー", "-" )
        , ( "、", "," )
        , ( "。", "." )
        , ( "「", "\"" )
        , ( "」", "\"" )
        , ( "・", "/" )
        ]


katakanaToRomaji : Dict String String
katakanaToRomaji =
    Dict.fromList
        [ ( "ア", "a" )
        , ( "イ", "i" )
        , ( "ウ", "u" )
        , ( "エ", "e" )
        , ( "オ", "o" )
        , ( "カ", "ka" )
        , ( "キ", "ki" )
        , ( "ク", "ku" )
        , ( "ケ", "ke" )
        , ( "コ", "ko" )
        , ( "サ", "sa" )
        , ( "シ", "shi" )
        , ( "ス", "su" )
        , ( "セ", "se" )
        , ( "ソ", "so" )
        , ( "タ", "ta" )
        , ( "チ", "chi" )
        , ( "ツ", "tsu" )
        , ( "テ", "te" )
        , ( "ト", "to" )
        , ( "ナ", "na" )
        , ( "ニ", "ni" )
        , ( "ヌ", "nu" )
        , ( "ネ", "ne" )
        , ( "ノ", "no" )
        , ( "ハ", "ha" )
        , ( "ヒ", "hi" )
        , ( "フ", "fu" )
        , ( "ヘ", "he" )
        , ( "ホ", "ho" )
        , ( "マ", "ma" )
        , ( "ミ", "mi" )
        , ( "ム", "mu" )
        , ( "メ", "me" )
        , ( "モ", "mo" )
        , ( "ヤ", "ya" )
        , ( "ユ", "yu" )
        , ( "ヨ", "yo" )
        , ( "ラ", "ra" )
        , ( "リ", "ri" )
        , ( "ル", "ru" )
        , ( "レ", "re" )
        , ( "ロ", "ro" )
        , ( "ワ", "wa" )
        , ( "ヲ", "wo" )
        , ( "ン", "n" )
        , ( "ガ", "ga" )
        , ( "ギ", "gi" )
        , ( "グ", "gu" )
        , ( "ゲ", "ge" )
        , ( "ゴ", "go" )
        , ( "ザ", "za" )
        , ( "ジ", "ji" )
        , ( "ズ", "zu" )
        , ( "ゼ", "ze" )
        , ( "ゾ", "zo" )
        , ( "ダ", "da" )
        , ( "ヂ", "ji" )
        , ( "ヅ", "zu" )
        , ( "デ", "de" )
        , ( "ド", "do" )
        , ( "バ", "ba" )
        , ( "ビ", "bi" )
        , ( "ブ", "bu" )
        , ( "ベ", "be" )
        , ( "ボ", "bo" )
        , ( "パ", "pa" )
        , ( "ピ", "pi" )
        , ( "プ", "pu" )
        , ( "ペ", "pe" )
        , ( "ポ", "po" )
        , ( "キャ", "kya" )
        , ( "キュ", "kyu" )
        , ( "キョ", "kyo" )
        , ( "シャ", "sha" )
        , ( "シュ", "shu" )
        , ( "ショ", "sho" )
        , ( "チャ", "cha" )
        , ( "チュ", "chu" )
        , ( "チョ", "cho" )
        , ( "ニャ", "nya" )
        , ( "ニュ", "nyu" )
        , ( "ニョ", "nyo" )
        , ( "ヒャ", "hya" )
        , ( "ヒュ", "hyu" )
        , ( "ヒョ", "hyo" )
        , ( "ミャ", "mya" )
        , ( "ミュ", "myu" )
        , ( "ミョ", "myo" )
        , ( "リャ", "rya" )
        , ( "リュ", "ryu" )
        , ( "リョ", "ryo" )
        , ( "ギャ", "gya" )
        , ( "ギュ", "gyu" )
        , ( "ギョ", "gyo" )
        , ( "ジャ", "ja" )
        , ( "ジュ", "ju" )
        , ( "ジョ", "jo" )
        , ( "ビャ", "bya" )
        , ( "ビュ", "byu" )
        , ( "ビョ", "byo" )
        , ( "ピャ", "pya" )
        , ( "ピュ", "pyu" )
        , ( "ピョ", "pyo" )
        , ( "ヴ", "vu" )
        , ( "ヰ", "wi" )
        , ( "ヱ", "we" )
        , ( "ヽ", "" )
        , ( "ヾ", "" )
        , ( "ー", "-" )
        , ( "、", "," )
        , ( "。", "." )
        , ( "「", "\"" )
        , ( "」", "\"" )
        , ( "・", "/" )
        ]


hiraganaKatakanaToRomaji : Dict String String
hiraganaKatakanaToRomaji =
    Dict.union hiraganaToRomaji katakanaToRomaji


type alias CharacterMapping =
    { mora : String -- Hiragana or katakana
    , romaji : String
    }


{-| Converts a word in hiragana/katakana to a list of moras and their romaji equivalence
-}
groupByMora : String -> Result String (List CharacterMapping)
groupByMora input =
    Result.mapError (\e -> "Failed to group word '" ++ input ++ "' by mora\n. Inner error: " ++ e) (groupByMoraHelper [] (String.toList input))


groupByMoraHelper : List CharacterMapping -> List Char -> Result String (List CharacterMapping)
groupByMoraHelper acc input =
    case input of
        'ッ' :: second :: third :: rest ->
            -- The first char of the mora should be duplicated
            case Dict.get (String.fromChar second ++ String.fromChar third) hiraganaKatakanaToRomaji of
                Just mora ->
                    case String.uncons mora of
                        Just ( a, b ) ->
                            groupByMoraHelper (acc ++ [ { mora = "ッ" ++ String.fromChar second ++ String.fromChar third, romaji = String.fromChar a ++ String.fromChar a ++ b } ]) rest

                        Nothing ->
                            Err <| "Failed to split mora apart for '" ++ String.fromChar second ++ String.fromChar third ++ "'"

                Nothing ->
                    case Dict.get (String.fromChar second) hiraganaKatakanaToRomaji of
                        Just mora ->
                            case String.uncons mora of
                                Just ( a, b ) ->
                                    groupByMoraHelper (acc ++ [ { mora = "っ" ++ String.fromChar second, romaji = String.fromChar a ++ String.fromChar a ++ b } ]) (third :: rest)

                                Nothing ->
                                    Err <| "Failed to split mora apart for '" ++ String.fromChar second ++ "'"

                        Nothing ->
                            Err <| "Failed to find romaji for '" ++ String.fromChar second ++ "'"

        'っ' :: second :: third :: rest ->
            -- The first char of the mora should be duplicated
            case Dict.get (String.fromChar second ++ String.fromChar third) hiraganaKatakanaToRomaji of
                Just mora ->
                    case String.uncons mora of
                        Just ( a, b ) ->
                            groupByMoraHelper (acc ++ [ { mora = "っ" ++ String.fromChar second ++ String.fromChar third, romaji = String.fromChar a ++ String.fromChar a ++ b } ]) rest

                        Nothing ->
                            Err <| "Failed to split mora apart for '" ++ String.fromChar second ++ String.fromChar third ++ "'"

                Nothing ->
                    case Dict.get (String.fromChar second) hiraganaKatakanaToRomaji of
                        Just mora ->
                            case String.uncons mora of
                                Just ( a, b ) ->
                                    groupByMoraHelper (acc ++ [ { mora = "っ" ++ String.fromChar second, romaji = String.fromChar a ++ String.fromChar a ++ b } ]) (third :: rest)

                                Nothing ->
                                    Err <| "Failed to split mora apart for '" ++ String.fromChar second ++ "'"

                        Nothing ->
                            Err <| "Failed to find romaji for '" ++ String.fromChar second ++ "'"

        'っ' :: second :: rest ->
            -- The first char of the mora should be duplicated
            case Dict.get (String.fromChar second) hiraganaKatakanaToRomaji of
                Just mora ->
                    case String.uncons mora of
                        Just ( a, b ) ->
                            groupByMoraHelper (acc ++ [ { mora = "っ" ++ String.fromChar second, romaji = String.fromChar a ++ String.fromChar a ++ b } ]) rest

                        Nothing ->
                            Err <| "Failed to split mora apart for '" ++ String.fromChar second ++ "'"

                Nothing ->
                    Err <| "Failed to find romaji for '" ++ String.fromChar second ++ "'"

        'ッ' :: second :: rest ->
            -- The first char of the mora should be duplicated
            case Dict.get (String.fromChar second) hiraganaKatakanaToRomaji of
                Just mora ->
                    case String.uncons mora of
                        Just ( a, b ) ->
                            groupByMoraHelper (acc ++ [ { mora = "ッ" ++ String.fromChar second, romaji = String.fromChar a ++ String.fromChar a ++ b } ]) rest

                        Nothing ->
                            Err <| "Failed to split mora apart for '" ++ String.fromChar second ++ "'"

                Nothing ->
                    Err <| "Failed to find romaji for '" ++ String.fromChar second ++ "'"

        first :: second :: rest ->
            case Dict.get (String.fromChar first ++ String.fromChar second) hiraganaKatakanaToRomaji of
                Just match ->
                    groupByMoraHelper (acc ++ [ { mora = String.fromChar first ++ String.fromChar second, romaji = match } ]) rest

                Nothing ->
                    case Dict.get (String.fromChar first) hiraganaKatakanaToRomaji of
                        Just match ->
                            groupByMoraHelper (acc ++ [ { mora = String.fromChar first, romaji = match } ]) (second :: rest)

                        Nothing ->
                            Err <| "Failed to find romaji for '" ++ String.fromChar first ++ "'"

        first :: _ ->
            case Dict.get (String.fromChar first) hiraganaKatakanaToRomaji of
                Just match ->
                    Ok <| acc ++ [ { mora = String.fromChar first, romaji = match } ]

                Nothing ->
                    Err <| "Failed to find romaji for '" ++ String.fromChar first ++ "'"

        [] ->
            Ok acc
