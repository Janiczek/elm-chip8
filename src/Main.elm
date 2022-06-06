module Main exposing (main)

import Playground as P


main : Program () (P.Playground Model) P.Msg
main =
    P.game view update init


type alias Model =
    { pixel : Int }


init : Model
init =
    { pixel = 0 }


view : P.Computer -> Model -> List P.Shape
view computer model =
    let
        x =
            model.pixel |> modBy screenWidth

        y =
            (model.pixel // screenWidth) |> modBy screenHeight
    in
    [ displayBgShape
    , pixelShape x y
    ]


update : P.Computer -> Model -> Model
update computer model =
    { model | pixel = model.pixel + 1 }


pixelShape : Int -> Int -> P.Shape
pixelShape x y =
    P.square black screenScale
        |> P.move -(screenWidthScaled / 2) (screenHeightScaled / 2)
        |> P.move (screenScale / 2) -(screenScale / 2)
        |> P.move (toFloat x * screenScale) -(toFloat y * screenScale)


displayBgShape : P.Shape
displayBgShape =
    P.rectangle white screenWidthScaled screenHeightScaled


screenScale : Float
screenScale =
    4


screenWidth : Int
screenWidth =
    64


screenHeight : Int
screenHeight =
    32


screenWidthScaled : Float
screenWidthScaled =
    toFloat screenWidth * screenScale


screenHeightScaled : Float
screenHeightScaled =
    toFloat screenHeight * screenScale


white : P.Color
white =
    P.rgb 253 246 227


black : P.Color
black =
    P.rgb 101 123 131



-- MEMORY:
-- 0x1000 bytes
-- 0x000 - 0x1FF are off the limits (interpreter itself) -- but we can store font data there
-- 0x200 - 0xE9F = usable
-- 0xEA0 - 0xEFF are off the limits (call stack, internal use, other variables)
-- 0xF00 - 0xFFF are off the limits (display refresh)
--------------------------------
-- REGISTERS:
-- 16 8-bit registers: V0..VF
-- VF reused as a flag (should be avoided in application code)
-- address register: I (12-bit)
--------------------------------
-- STACK:
-- stores return addresses when subroutines are called
-- at least 12 levels of nesting
--------------------------------
-- TIMERS:
-- two timers at 60Hz, down to 0
-- Delay timer (Read/Write)
-- Sound timer (when non-zero, beeps) (Write only?)
--------------------------------
-- KEYBOARD:
-- 16 keys 0..F
-- 8 = UP, 4 = LEFT, 6 = RIGHT, 2 = DOWN
--------------------------------
-- GRAPHICS:
-- 64x32 pixels, monochrome (black/white)
-- drawing via sprites (Width = 8px, Height = 1..15px)
-- sprite pixels are XOR'd with screen pixels
-- VF set to 1 if a pixel is flipped 1->0 (used for collision detection)
--------------------------------
-- OPCODES:
-- 35 of them
-- all are 2 bytes long, big-endian (the expected way)
