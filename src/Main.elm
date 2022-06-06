module Main exposing (main)

import Display
import Error exposing (Error)
import ExamplePrograms
import Instruction exposing (Instruction(..))
import Memory exposing (Memory)
import Playground as P
import Util


main : Program () (P.Playground Model) P.Msg
main =
    P.game view update init


type alias Model =
    -- TODO some error state? don't run endlessly
    { memory : Memory
    , pc : Int
    , state : State

    -- DISPLAY: the 0x800 pixels = 0x100 bytes live inside 0xF00..0xFFF of the memory
    -- TODO: what about display : Set (Int, Int)
    }


type State
    = Running
    | Paused
    | Halted Error


init : Model
init =
    { memory = Memory.init |> Memory.loadProgram ExamplePrograms.maze
    , pc = Memory.programStart
    , state = Running
    }


view : P.Computer -> Model -> List P.Shape
view computer model =
    [ Display.view
    , Memory.view model.pc model.memory
    , case model.state of
        Running ->
            []

        Paused ->
            [ P.words P.orange "Paused" ]

        Halted err ->
            [ P.words P.red (Error.toString err)
                |> P.moveDown 96
            ]
    ]
        |> List.concat


update : P.Computer -> Model -> Model
update computer model =
    model
        |> stepTimes {- (min computer.time.delta 4) -} 1


stepTimes : Int -> Model -> Model
stepTimes n model =
    doNTimes n step model


doNTimes : Int -> (a -> a) -> a -> a
doNTimes n fn value =
    if n <= 0 then
        value

    else
        doNTimes (n - 1) fn (fn value)


step : Model -> Model
step model =
    case Memory.getInstruction model.pc model.memory of
        Err err ->
            { model | state = Halted err }

        Ok instruction ->
            model
                |> runInstruction instruction
                |> incrementPC


incrementPC : Model -> Model
incrementPC model =
    { model | pc = model.pc + 2 }


runInstruction : Instruction -> Model -> Model
runInstruction instruction model =
    case instruction of
        TodoInstruction ->
            model



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
