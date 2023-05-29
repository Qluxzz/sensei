module Tooltip exposing (withTooltip)

import Html exposing (Html, text)
import Html.Attributes


{-| Adds a tooltip that shows when element is hovered/touched
-}
withTooltip : String -> String -> Html msg
withTooltip text_ tooltip =
    Html.button
        [ Html.Attributes.type_ "button"
        , Html.Attributes.attribute "data-tooltip" tooltip
        , Html.Attributes.tabindex -1
        ]
        [ text text_ ]
