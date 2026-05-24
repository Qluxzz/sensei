module E2E exposing (suite)

import Main
import ProgramTest
import Test
import Test.Html.Selector


suite : Test.Test
suite =
    Test.test "Renders as expected" <|
        \_ ->
            ProgramTest.createDocument
                { init = Main.init
                , update = Main.update
                , view = Main.view
                }
                |> ProgramTest.start ()
                |> ProgramTest.expectViewHas
                    [ Test.Html.Selector.exactText "New Document"
                    ]
