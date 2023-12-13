module E2E exposing (..)

import Game
import Json.Encode
import ProgramTest
import Test exposing (..)
import Test.Html.Query
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


createWithInitalizedWord : ProgramTest.ProgramTest Game.Model Game.Msg (Cmd Game.Msg)
createWithInitalizedWord =
    case Game.init testCase.word of
        Err e ->
            ProgramTest.createFailed "Game.init" e

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
                |> ProgramTest.start ()


gameLoopTest : Test
gameLoopTest =
    describe "Game loop"
        [ test "game is playable" <|
            \_ ->
                createWithInitalizedWord
                    -- Romaji stage
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
        , test "Error message when inputting wrong word" <|
            \_ ->
                let
                    wrongWord =
                        "Not the right answer!"
                in
                createWithInitalizedWord
                    -- Romaji stage
                    |> ProgramTest.fillIn "input-field" "input-field" wrongWord
                    |> ProgramTest.clickButton "Submit"
                    |> ProgramTest.ensureViewHas [ Test.Html.Selector.exactText "Sorry, that's not the right answer!" ]
                    |> ProgramTest.fillIn "input-field" "input-field" testCase.romaji
                    |> ProgramTest.clickButton "Submit"
                    |> ProgramTest.ensureViewHas [ Test.Html.Selector.exactText "That's correct!" ]
                    |> ProgramTest.clickButton "Continue!"
                    -- Kana stage
                    |> ProgramTest.ensureViewHas [ Test.Html.Selector.exactText ("The word in romaji is " ++ testCase.romaji) ]
                    |> ProgramTest.fillIn "input-field" "input-field" wrongWord
                    |> ProgramTest.clickButton "Submit"
                    |> ProgramTest.ensureViewHas [ Test.Html.Selector.exactText "Sorry, that's not the right answer!" ]
                    |> ProgramTest.fillIn "input-field" "input-field" testCase.word.kana
                    |> ProgramTest.clickButton "Submit"
                    |> ProgramTest.ensureViewHas [ Test.Html.Selector.exactText "That's correct!" ]
                    |> ProgramTest.clickButton "Continue!"
                    -- Glossary stage
                    |> ProgramTest.ensureViewHas [ Test.Html.Selector.exactText ("Your word is " ++ testCase.word.str) ]
                    |> ProgramTest.fillIn "input-field" "input-field" wrongWord
                    |> ProgramTest.clickButton "Submit"
                    |> ProgramTest.ensureViewHas [ Test.Html.Selector.exactText "Sorry, that's not the right answer!" ]
                    |> ProgramTest.fillIn "input-field" "input-field" (List.head testCase.word.glossary |> Maybe.withDefault "ERROR")
                    |> ProgramTest.clickButton "Submit"
                    |> ProgramTest.expectViewHas [ Test.Html.Selector.exactText "That's correct!" ]
        , test "Incorrect input says incorrect" <|
            \_ ->
                createWithInitalizedWord
                    -- Romaji stage
                    |> ProgramTest.fillIn "input-field" "input-field" "a"
                    |> ProgramTest.clickButton "Submit"
                    |> ProgramTest.expectViewHas [ Test.Html.Selector.exactText "Sorry, that's not the right answer!" ]
        , test "Clicking on hidden glossary word reveals it" <|
            \_ ->
                createWithInitalizedWord
                    -- Romaji stage
                    |> ProgramTest.fillIn "input-field" "input-field" testCase.romaji
                    |> ProgramTest.clickButton "Submit"
                    |> ProgramTest.clickButton "Continue!"
                    -- Kana stage
                    |> ProgramTest.fillIn "input-field" "input-field" testCase.word.kana
                    |> ProgramTest.clickButton "Submit"
                    |> ProgramTest.clickButton "Continue!"
                    -- Glossary stage
                    |> ProgramTest.ensureViewHas [ Test.Html.Selector.exactText ("Your word is " ++ testCase.word.str) ]
                    |> ProgramTest.ensureViewHasNot [ Test.Html.Selector.exactText "cute" ]
                    |> ProgramTest.simulateDomEvent (Test.Html.Query.find [ Test.Html.Selector.id "glossary-item-1" ]) ( "click", Json.Encode.string "" )
                    |> ProgramTest.expectViewHas [ Test.Html.Selector.exactText "cute" ]
        ]
