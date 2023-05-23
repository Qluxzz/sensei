module Tooltip exposing (withTooltip)

import Html exposing (Html, span, text)
import Html.Attributes exposing (attribute)


{-| Adds a tooltip that shows when element is hovered/touched
-}
withTooltip : String -> String -> Html msg
withTooltip text_ tooltip =
    span [ attribute "data-tooltip" tooltip ] [ text text_ ]
