module Main exposing (main)

import Bitwise
import Display
import Error exposing (Error(..))
import ExamplePrograms
import Instruction exposing (Address(..), Byte(..), Instruction(..))
import Memory exposing (Memory)
import Playground as P
import Random exposing (Generator)
import Registers exposing (Registers)
import Util


main : Program () (P.Playground Model) P.Msg
main =
    P.game view update init


type alias Model =
    -- TODO some error state? don't run endlessly
    { memory : Memory
    , pc : Address
    , i : Address
    , registers : Registers
    , state : State
    , randomSeed : Random.Seed

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
    , pc = Address Memory.programStart
    , i = Address 0
    , registers = Registers.init
    , state = Running
    , randomSeed =
        -- TODO when we ditch the Playground package, wire in a random seed from JS flags
        Random.initialSeed 0
    }


view : P.Computer -> Model -> List P.Shape
view computer model =
    [ Display.view
    , Memory.view model.pc model.memory

    -- TODO show the registers
    -- TODO show pc
    -- TODO show i
    -- TODO show random seed
    -- TODO show disassembled code
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
    if isHalted model.state then
        model

    else
        doNTimes n step model


isHalted : State -> Bool
isHalted state =
    case state of
        Halted _ ->
            True

        Paused ->
            False

        Running ->
            False


doNTimes : Int -> (a -> a) -> a -> a
doNTimes n fn value =
    if n <= 0 then
        value

    else
        doNTimes (n - 1) fn (fn value)


step : Model -> Model
step model =
    if isHalted model.state then
        model

    else
        case Memory.getInstruction model.pc model.memory of
            Err err ->
                { model | state = Halted err }

            Ok instruction ->
                model
                    |> runInstruction instruction
                    -- TODO only increment in some cases (eg. not when jumping)
                    |> incrementPC


incrementPC : Model -> Model
incrementPC model =
    let
        (Address pc) =
            model.pc
    in
    { model | pc = Address (pc + 2) }


runInstruction : Instruction -> Model -> Model
runInstruction instruction model =
    let
        todo : Model -> Model
        todo m =
            { m | state = Halted (UnimplementedInstruction instruction) }
    in
    case Debug.log "run" instruction of
        Clear ->
            todo model

        Return ->
            todo model

        Jump addr ->
            todo model

        Call addr ->
            todo model

        DoIfNeq reg (Byte byte) ->
            let
                regValue =
                    Registers.get reg model.registers
            in
            if regValue /= byte then
                model

            else
                incrementPC model

        DoIfEq reg byte ->
            todo model

        DoIfNeqReg reg1 reg2 ->
            todo model

        SetRegConst reg byte ->
            todo model

        AddRegConst reg byte ->
            todo model

        SetRegReg { from, to } ->
            todo model

        OrRegReg { from, to } ->
            todo model

        AndRegReg { from, to } ->
            todo model

        XorRegReg { from, to } ->
            todo model

        AddRegReg { from, to } ->
            todo model

        SubRegReg { from, to } ->
            todo model

        ShiftRightRegReg { from, to } ->
            todo model

        SubReverseRegReg { from, to } ->
            todo model

        ShiftLeftRegReg { from, to } ->
            todo model

        DoIfEqReg reg1 reg2 ->
            todo model

        SetI addr ->
            { model | i = addr }

        JumpPlusV0 addr ->
            todo model

        SetRandomAnd register (Byte mask) ->
            let
                ( randomByte, newSeed ) =
                    Random.step byteGenerator model.randomSeed

                maskedByte : Int
                maskedByte =
                    Bitwise.and mask randomByte
            in
            { model
                | randomSeed = newSeed
                , registers = Registers.set register maskedByte model.registers
            }

        DrawSprite { vx, vy, height } ->
            todo model

        DoIfKeyNotPressed reg ->
            todo model

        DoIfKeyPressed reg ->
            todo model

        GetDelayTimer reg ->
            todo model

        SetPressedKey reg ->
            todo model

        SetDelayTimer reg ->
            todo model

        SetAudioTimer reg ->
            todo model

        AddI reg ->
            todo model

        SetIToFontAddr reg ->
            todo model

        BcdDecode reg ->
            todo model

        SaveRegsUpTo reg ->
            todo model

        LoadRegsUpTo reg ->
            todo model


byteGenerator : Generator Int
byteGenerator =
    Random.int 0 255



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
