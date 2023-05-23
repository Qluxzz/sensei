module Tooltip exposing (withTooltip)

import Html exposing (Html, button, text)
import Html.Attributes exposing (attribute, type_)


{-| Adds a tooltip that shows when element is hovered/clicked
-}
withTooltip : String -> String -> Html msg
withTooltip text_ tooltip =
    button [ type_ "button", attribute "data-tooltip" tooltip ] [ text text_ ]
