module Display exposing (view)

import Playground as P
import Util


view : List P.Shape
view =
    [ viewDisplayBg
    , Util.viewPixel Util.black 1 5
    ]


viewDisplayBg : P.Shape
viewDisplayBg =
    P.rectangle Util.white Util.screenWidthScaled Util.screenHeightScaled
