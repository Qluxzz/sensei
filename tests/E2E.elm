module E2E exposing (..)

import Main
import ProgramTest
import Test exposing (..)
import Test.Html.Selector as Q


suite : Test
suite =
    test "Renders as expected" <|
        \_ ->
            ProgramTest.createDocument
                { init = Main.init
                , update = Main.update
                , view = Main.view
                }
                |> ProgramTest.start ()
                |> ProgramTest.expectViewHas
                    [ Q.exactText "New Document"
                    ]
