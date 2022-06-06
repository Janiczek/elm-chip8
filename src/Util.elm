module Util exposing
    ( black
    , px
    , screenHeightScaled
    , screenWidth
    , screenWidthScaled
    , viewPixel
    , white
    )

import Playground as P


viewPixel : P.Color -> Int -> Int -> P.Shape
viewPixel color x y =
    P.square color screenScaleFloat
        |> P.move -(screenWidthScaled / 2) (screenHeightScaled / 2)
        |> P.move (screenScaleFloat / 2) -(screenScaleFloat / 2)
        |> P.move (px x) -(px y)


white : P.Color
white =
    P.rgb 253 246 227


black : P.Color
black =
    P.rgb 101 123 131


px : Int -> Float
px n =
    toFloat (n * screenScale)


screenScale : Int
screenScale =
    4


screenScaleFloat : Float
screenScaleFloat =
    toFloat screenScale


screenWidth : Int
screenWidth =
    64


screenHeight : Int
screenHeight =
    32


screenWidthScaled : Float
screenWidthScaled =
    toFloat (screenWidth * screenScale)


screenHeightScaled : Float
screenHeightScaled =
    toFloat (screenHeight * screenScale)
