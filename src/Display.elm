module Display exposing (view)

import Memory exposing (Memory)
import Playground as P
import Util


view : Memory -> List P.Shape
view memory =
    viewDisplayBg
        :: List.map (\( x, y ) -> Util.viewPixel Util.black x y) (Memory.getDisplayActivePixels memory)


viewDisplayBg : P.Shape
viewDisplayBg =
    P.rectangle Util.white Util.screenWidthScaled Util.screenHeightScaled
