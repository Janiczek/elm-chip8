module Display exposing (view)

import Playground as P
import Util


view : List P.Shape
view =
    [ viewDisplayBg
    ]


viewDisplayBg : P.Shape
viewDisplayBg =
    P.rectangle Util.white Util.screenWidthScaled Util.screenHeightScaled
