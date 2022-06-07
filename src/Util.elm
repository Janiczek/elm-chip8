module Util exposing
    ( black
    , bytePair
    , hex
    , hexBytePair
    , px
    , screenHeightScaledStringPx
    , screenWidth
    , screenWidthScaledStringPx
    , toBitList
    , viewPixel
    , white
    )

import Bitwise
import Html exposing (Html)
import Html.Attributes as Attrs
import ParseInt
import RadixInt


viewPixel : String -> Int -> Int -> Html msg
viewPixel color x y =
    Html.div
        [ Attrs.style "background-color" color
        , Attrs.style "width" screenScaleStringPx
        , Attrs.style "height" screenScaleStringPx
        , Attrs.style "transform" ("translate(" ++ px x ++ "," ++ px y ++ ")")
        , Attrs.style "position" "absolute"
        , Attrs.style "left" "0"
        , Attrs.style "top" "0"
        ]
        []


white : String
white =
    "rgb(253,246,227)"


black : String
black =
    "rgb(101,123,131)"


px : Int -> String
px n =
    String.fromInt (n * screenScale) ++ "px"


screenScale : Int
screenScale =
    4


screenScaleStringPx : String
screenScaleStringPx =
    String.fromInt screenScale ++ "px"


screenWidth : Int
screenWidth =
    64


screenHeight : Int
screenHeight =
    32


screenWidthScaledStringPx : String
screenWidthScaledStringPx =
    String.fromInt (screenWidth * screenScale) ++ "px"


screenHeightScaledStringPx : String
screenHeightScaledStringPx =
    String.fromInt (screenHeight * screenScale) ++ "px"


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
