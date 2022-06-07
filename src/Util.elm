module Util exposing
    ( black
    , bytePair
    , hex
    , hexBytePair
    , px
    , screenHeightScaled
    , screenWidth
    , screenWidthScaled
    , toBitList
    , viewPixel
    , white
    )

import Bitwise
import ParseInt
import Playground as P
import RadixInt


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


hex : Int -> String
hex n =
    "0x" ++ ParseInt.toHex n


bytePair : ( Int, Int ) -> Int
bytePair ( hi, lo ) =
    Bitwise.or
        (Bitwise.shiftLeftBy 8 hi)
        lo


hexBytePair : ( Int, Int ) -> String
hexBytePair ( hi, lo ) =
    hex (bytePair ( hi, lo ))


toBitList : Int -> List Int
toBitList byte =
    byte
        |> RadixInt.fromInt (RadixInt.Base 2)
        |> RadixInt.toList
        |> List.reverse
        |> zeroPadLeft 8


zeroPadLeft : Int -> List Int -> List Int
zeroPadLeft length list =
    let
        toAdd : Int
        toAdd =
            max 0 (length - List.length list)
    in
    List.repeat toAdd 0 ++ list