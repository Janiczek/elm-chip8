module Display exposing (view)

import Html exposing (Html)
import Html.Attributes as Attrs
import Memory exposing (Memory)
import Util


view : Memory -> Html msg
view memory =
    Html.div []
        [ Html.h2 [] [ Html.text "Display" ]
        , Html.div
            [ Attrs.style "background-color" Util.white
            , Attrs.style "width" Util.screenWidthScaledStringPx
            , Attrs.style "height" Util.screenHeightScaledStringPx
            , Attrs.style "position" "relative"
            ]
            (List.map (\( x, y ) -> Util.viewPixel Util.black x y) (Memory.getDisplayActivePixels memory))
        ]
