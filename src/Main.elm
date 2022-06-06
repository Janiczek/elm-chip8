module Main exposing (main)

import Playground as P


main : Program () (P.Playground ()) P.Msg
main =
    P.picture
        [ displayBgShape
        , pixelShape 1 3
        , pixelShape 2 3
        , pixelShape 3 3
        , pixelShape 4 3
        , pixelShape 0 0
        ]


pixelShape : Int -> Int -> P.Shape
pixelShape x y =
    P.square black screenScale
        |> P.move -(screenWidth * screenScale / 2) (screenHeight * screenScale / 2)
        |> P.move (screenScale / 2) -(screenScale / 2)
        |> P.move (toFloat x * screenScale) -(toFloat y * screenScale)


displayBgShape : P.Shape
displayBgShape =
    P.rectangle white (screenWidth * screenScale) (screenHeight * screenScale)


screenScale : Float
screenScale =
    4


screenWidth : Float
screenWidth =
    64


screenHeight : Float
screenHeight =
    32


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
