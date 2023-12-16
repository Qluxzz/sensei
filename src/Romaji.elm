module Romaji exposing (CharacterMapping, groupByMora, hiragana)

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
        , ( "ー", "-" )
        ]


{-| List of all characters in hiragana
-}
hiragana : List String
hiragana =
    Dict.keys hiraganaToRomaji


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
        , ( "ー", "-" )
        ]


hiraganaKatakanaToRomaji : Dict String String
hiraganaKatakanaToRomaji =
    Dict.union hiraganaToRomaji katakanaToRomaji


type alias CharacterMapping =
    { mora : String -- Hiragana or katakana
    , romaji : String
    }


specialCharacters : { doubleFollowingConsonant : List String, combo : List String }
specialCharacters =
    { doubleFollowingConsonant =
        [ "っ", "ッ" ]
    , combo =
        [ "ャ", "ュ", "ョ", "ゃ", "ゅ", "ょ" ]
    }


{-| Converts a word in hiragana/katakana to a list of moras and their romaji equivalence
-}
groupByMora : String -> Result String (List CharacterMapping)
groupByMora input =
    let
        moras =
            groupByMoraHelper [] input

        res : Result String (List CharacterMapping)
        res =
            List.foldl
                (\moraType ->
                    \acc ->
                        case acc of
                            Ok acc2 ->
                                case getMoraByType moraType of
                                    Just str ->
                                        Ok ({ mora = getMoraString moraType, romaji = str } :: acc2)

                                    Nothing ->
                                        Err ("Failed to find romaji for '" ++ getMoraString moraType ++ "'")

                            Err _ ->
                                acc
                )
                (Ok [])
                moras
    in
    Result.mapError (\e -> "Failed to group word '" ++ input ++ "' by mora\n. Inner error: " ++ e) res


groupByMoraHelper : List MoraType -> String -> List MoraType
groupByMoraHelper acc input =
    if String.isEmpty input then
        acc

    else
        let
            moraType =
                if List.member (String.left 1 input) specialCharacters.doubleFollowingConsonant then
                    if List.member (input |> String.dropLeft 2 |> String.left 1) specialCharacters.combo then
                        DoubleConsonantAndCombo (String.left 3 input)

                    else
                        DoubleConsonant (String.left 2 input)

                else if List.member (input |> String.dropLeft 1 |> String.left 1) specialCharacters.combo then
                    Combo (String.left 2 input)

                else
                    Single (String.left 1 input)

            dropAmount =
                case moraType of
                    Single _ ->
                        1

                    DoubleConsonant _ ->
                        2

                    Combo _ ->
                        2

                    DoubleConsonantAndCombo _ ->
                        3
        in
        groupByMoraHelper (moraType :: acc) (String.dropLeft dropAmount input)


type MoraType
    = Single String -- い
    | DoubleConsonant String -- っと
    | Combo String -- きゅ
    | DoubleConsonantAndCombo String -- っきょ


getMoraByType : MoraType -> Maybe String
getMoraByType type_ =
    case type_ of
        Single str ->
            Dict.get str hiraganaKatakanaToRomaji

        Combo str ->
            Dict.get str hiraganaKatakanaToRomaji

        DoubleConsonant str ->
            case String.uncons str of
                Just ( _, rest ) ->
                    Dict.get rest hiraganaKatakanaToRomaji |> Maybe.map (\mora -> String.left 1 mora ++ mora)

                _ ->
                    Nothing

        DoubleConsonantAndCombo str ->
            case String.uncons str of
                Just ( _, rest ) ->
                    Dict.get rest hiraganaKatakanaToRomaji |> Maybe.map (\mora -> String.left 1 mora ++ mora)

                _ ->
                    Nothing


getMoraString : MoraType -> String
getMoraString type_ =
    case type_ of
        Single str ->
            str

        DoubleConsonant str ->
            str

        Combo str ->
            str

        DoubleConsonantAndCombo str ->
            str
