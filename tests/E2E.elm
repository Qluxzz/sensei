module E2E exposing (..)

import Expect
import Game
import ProgramTest
import Test exposing (..)
import Test.Html.Selector
import Words exposing (Word)


type alias TestCase =
    { word : Word, romaji : String }


testCase : TestCase
testCase =
    { word =
        { str = "可愛い"
        , kana = "かわいい"
        , glossary = [ "cute", "adorable", "charming", "lovely", "pretty", "dear", "precious", "darling", "pet", "innocent", "childlike", "childish", "lovable", "dainty", "little", "tiny" ]
        }
    , romaji = "kawaii"
    }


gameLoopTest : Test
gameLoopTest =
    test "game is playable" <|
        \() ->
            case Game.init testCase.word of
                Err e ->
                    Expect.fail e

                Ok modelAndCmd ->
                    ProgramTest.createElement
                        { init = \_ -> modelAndCmd
                        , update =
                            \msg ->
                                \model ->
                                    let
                                        ( updatedModel, updatedMsg, _ ) =
                                            Game.update msg model
                                    in
                                    ( updatedModel, updatedMsg )
                        , view = Game.view
                        }
                        -- Romaji stage
                        |> ProgramTest.start ()
                        |> ProgramTest.fillIn "input-field" "input-field" testCase.romaji
                        |> ProgramTest.clickButton "Submit"
                        |> ProgramTest.ensureViewHas [ Test.Html.Selector.exactText "That's correct!" ]
                        |> ProgramTest.clickButton "Continue!"
                        -- Kana stage
                        |> ProgramTest.ensureViewHas [ Test.Html.Selector.exactText ("The word in romaji is " ++ testCase.romaji) ]
                        |> ProgramTest.fillIn "input-field" "input-field" testCase.word.kana
                        |> ProgramTest.clickButton "Submit"
                        |> ProgramTest.ensureViewHas [ Test.Html.Selector.exactText "That's correct!" ]
                        |> ProgramTest.clickButton "Continue!"
                        -- Glossary stage
                        |> ProgramTest.ensureViewHas [ Test.Html.Selector.exactText ("Your word is " ++ testCase.word.str) ]
                        |> ProgramTest.fillIn "input-field" "input-field" (List.head testCase.word.glossary |> Maybe.withDefault "ERROR")
                        |> ProgramTest.clickButton "Submit"
                        |> ProgramTest.expectViewHas [ Test.Html.Selector.exactText "That's correct!" ]
