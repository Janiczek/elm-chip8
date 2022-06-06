module Main exposing (main)

import ExamplePrograms
import Memory exposing (Memory)
import Playground as P


main : Program () (P.Playground Model) P.Msg
main =
    P.game view update init


type alias Model =
    { memory : Memory
    , pc : Int

    -- DISPLAY: the 0x800 pixels = 0x100 bytes live inside 0xF00..0xFFF of the memory
    -- TODO: what about display : Set (Int, Int)
    }


init : Model
init =
    { memory =
        Memory.init
            |> Memory.loadProgram ExamplePrograms.maze
    , pc = Memory.programStart
    }


view : P.Computer -> Model -> List P.Shape
view computer model =
    [ [ viewDisplayBg
      , viewPixel black 1 5
      ]
    , viewMemory model.pc model.memory
    ]
        |> List.concat


update : P.Computer -> Model -> Model
update computer model =
    -- TODO
    model


viewPixel : P.Color -> Int -> Int -> P.Shape
viewPixel color x y =
    P.square color screenScaleFloat
        |> P.move -(screenWidthScaled / 2) (screenHeightScaled / 2)
        |> P.move (screenScaleFloat / 2) -(screenScaleFloat / 2)
        |> P.move (px x) -(px y)


viewDisplayBg : P.Shape
viewDisplayBg =
    P.rectangle white screenWidthScaled screenHeightScaled


memoryWidth : Int
memoryWidth =
    64


viewMemory : Int -> Memory -> List P.Shape
viewMemory pc memory =
    memory
        |> Memory.toList
        |> List.indexedMap
            (\i byte ->
                let
                    x : Int
                    x =
                        i |> modBy memoryWidth

                    y : Int
                    y =
                        i // memoryWidth

                    b : Float
                    b =
                        toFloat byte

                    color : P.Color
                    color =
                        if pc == i then
                            P.red

                        else
                            P.rgb b b b
                in
                viewPixel color x y
                    |> P.move (px (screenWidth + 8)) (px 32)
            )


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
