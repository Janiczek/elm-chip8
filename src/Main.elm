module Main exposing (main)

import Bitwise
import Browser
import Browser.Events
import Display
import Error exposing (Error(..))
import ExamplePrograms exposing (ProgramROM(..))
import Html exposing (Html)
import Html.Attributes as Attrs
import Html.Events as Events
import Instruction exposing (Address(..), Byte(..), Instruction(..))
import Instruction.Parser
import Memory exposing (Memory)
import Random exposing (Generator)
import Registers exposing (Register(..), Registers)
import Result.Extra as Result
import Time
import Util


main : Program Flags Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


type alias Flags =
    { initialSeed : Int
    }


type alias Model =
    { memory : Memory
    , pc : Address
    , i : Address
    , registers : Registers
    , delayTimer : Int
    , callStack : List Address
    , state : State
    , randomSeed : Random.Seed
    , initialSeed : Int
    }


type Msg
    = Tick Float
    | DelayTimerTick
    | RunClicked
    | PauseClicked
    | ResetClicked
    | StepClicked
    | ProgramSelected ProgramROM


type State
    = Running
    | Paused
    | Halted Error


initROM : ProgramROM
initROM =
    Trip8


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { memory = Memory.init |> Memory.loadProgram (ExamplePrograms.program initROM)
      , pc = Address Memory.programStart
      , i = Address 0
      , registers = Registers.init
      , delayTimer = 0
      , callStack = []
      , state = Paused
      , randomSeed = Random.initialSeed flags.initialSeed
      , initialSeed = flags.initialSeed
      }
    , Cmd.none
    )


loadROM : ProgramROM -> Model -> Model
loadROM rom model =
    { model
        | memory = Memory.init |> Memory.loadProgram (ExamplePrograms.program rom)
        , pc = Address Memory.programStart
        , i = Address 0
        , registers = Registers.init
        , delayTimer = 0
        , callStack = []
        , state = Paused
        , randomSeed = Random.initialSeed model.initialSeed
    }


reset : Model -> Model
reset model =
    { model
        | memory = Memory.init |> Memory.loadProgram (ExamplePrograms.program initROM)
        , pc = Address Memory.programStart
        , i = Address 0
        , registers = Registers.init
        , delayTimer = 0
        , callStack = []
        , state = Paused
        , randomSeed = Random.initialSeed model.initialSeed
    }


view : Model -> Browser.Document Msg
view model =
    { title = "CHIP-8 Emulator"
    , body =
        [ Html.div
            [ Attrs.style "display" "flex"
            , Attrs.style "flex-direction" "row"
            , Attrs.style "gap" "8px"
            ]
            [ Html.div []
                [ Display.view model.memory
                , Memory.view
                    { pc = model.pc
                    , i = model.i
                    }
                    model.memory
                , viewState model.state
                , viewButtons model.state
                ]
            , Html.div []
                [ viewRegisters model
                , viewCallStack model.callStack

                -- TODO viewInitialSeed model.randomSeed
                , viewDisassembledCode model
                ]
            ]
        ]
    }


viewState : State -> Html msg
viewState state =
    Html.div []
        [ Html.h2 [] [ Html.text "State" ]
        , case state of
            Running ->
                Html.text "Running"

            Paused ->
                Html.text "Paused"

            Halted err ->
                Html.text <| "Halted: " ++ Error.toString err
        ]


viewButtons : State -> Html Msg
viewButtons state =
    Html.div []
        [ Html.div
            []
            [ Html.button
                [ Events.onClick RunClicked
                , Attrs.disabled (isRunning state)
                ]
                [ Html.text "Run" ]
            , Html.button
                [ Events.onClick StepClicked ]
                [ Html.text "Step" ]
            , Html.button
                [ Events.onClick PauseClicked
                , Attrs.disabled (not (isRunning state))
                ]
                [ Html.text "Pause" ]
            , Html.button
                [ Events.onClick ResetClicked ]
                [ Html.text "Reset" ]
            ]
        , Html.h4 [] [ Html.text "Load a ROM:" ]
        , Html.div []
            (List.map
                (\program ->
                    Html.button
                        [ Events.onClick (ProgramSelected program) ]
                        [ Html.text <| ExamplePrograms.name program ]
                )
                ExamplePrograms.all
            )
        ]


viewCallStack : List Address -> Html msg
viewCallStack callStack =
    Html.div []
        [ Html.h2 [] [ Html.text "Call stack" ]
        , if List.isEmpty callStack then
            Html.text "- empty -"

          else
            Html.ul []
                (List.map
                    (\(Address addr) -> Html.li [] [ Html.text <| Util.hex addr ])
                    callStack
                )
        ]


viewRegisters : Model -> Html msg
viewRegisters { pc, i, delayTimer, registers } =
    let
        viewRegister : { special : Bool } -> String -> Int -> Html msg
        viewRegister { special } name value =
            Html.li
                [ Attrs.style "font-weight"
                    (if special then
                        "bold"

                     else
                        "normal"
                    )
                ]
                [ Html.text <| name ++ ": " ++ Util.hex value ]

        (Address pc_) =
            pc

        (Address i_) =
            i
    in
    Html.div []
        [ Html.h2 [] [ Html.text "Registers" ]
        , Html.ul []
            (viewRegister { special = True } "PC" pc_
                :: viewRegister { special = True } "I" i_
                :: viewRegister { special = True } "Delay" delayTimer
                :: List.map
                    (\reg ->
                        viewRegister
                            { special = reg == VF }
                            (Registers.name reg)
                            (Registers.get reg registers)
                    )
                    Registers.all
            )
        ]


viewDisassembledCode : Model -> Html msg
viewDisassembledCode model =
    let
        (Address pc) =
            model.pc

        range : List ( Address, ( Byte, Byte ) )
        range =
            List.range -5 5
                |> List.filterMap
                    (\delta ->
                        let
                            address1 =
                                Address (pc + 2 * delta)

                            address2 =
                                Address (pc + 2 * delta + 1)
                        in
                        Result.map2 (\hi lo -> ( address1, ( hi, lo ) ))
                            (Memory.get address1 model.memory)
                            (Memory.get address2 model.memory)
                            |> Result.toMaybe
                    )

        viewLine : ( Address, ( Byte, Byte ) ) -> Html msg
        viewLine ( Address addr, ( (Byte hi_) as hi, (Byte lo_) as lo ) ) =
            let
                ( instruction, arguments ) =
                    case Instruction.Parser.parse ( hi, lo ) of
                        Err _ ->
                            ( "", "" )

                        Ok instr ->
                            ( Instruction.toString instr
                            , Instruction.arguments instr
                            )
            in
            Html.tr
                [ Attrs.style "background-color"
                    (if addr == pc then
                        "yellow"

                     else
                        "transparent"
                    )
                ]
                [ Html.td [ cellPadding ] [ Html.text <| Util.hex addr ]
                , Html.td [ cellPadding ] [ Html.text <| Util.hexBytePair ( hi_, lo_ ) ]
                , Html.td [ cellPadding ] [ Html.text instruction ]
                , Html.td [ cellPadding ] [ Html.text arguments ]
                ]

        cellPadding =
            Attrs.style "padding" "0 4px"
    in
    Html.div
        []
        [ Html.h2 [] [ Html.text "Code" ]
        , Html.table
            [ Attrs.style "border-collapse" "collapse"
            , Attrs.style "font-family" "Menlo, Monaco, \"Cascadia Mono\", Consolas, \"Courier New\", monospace"
            ]
            [ Html.thead []
                [ Html.tr []
                    [ Html.th [ cellPadding ] [ Html.text "Address" ]
                    , Html.th [ cellPadding ] [ Html.text "Opcode" ]
                    , Html.th [ cellPadding ] [ Html.text "Instruction" ]
                    , Html.th [ cellPadding ] [ Html.text "Arguments" ]
                    ]
                ]
            , Html.tbody []
                (List.map viewLine range)
            ]
        ]


sixtyHertz : Float
sixtyHertz =
    1000 / 60


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ if isRunning model.state then
            Browser.Events.onAnimationFrameDelta Tick

          else
            Sub.none
        , if model.delayTimer > 0 then
            Time.every sixtyHertz (\_ -> DelayTimerTick)

          else
            Sub.none
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick msDelta ->
            ( model
                |> stepTimes (min (round msDelta) 4)
            , Cmd.none
            )

        DelayTimerTick ->
            ( { model | delayTimer = model.delayTimer - 1 }
            , Cmd.none
            )

        RunClicked ->
            ( { model | state = Running }
            , Cmd.none
            )

        PauseClicked ->
            ( { model | state = Paused }
            , Cmd.none
            )

        ResetClicked ->
            ( reset model
            , Cmd.none
            )

        StepClicked ->
            ( step model
            , Cmd.none
            )

        ProgramSelected program ->
            ( loadROM program model
            , Cmd.none
            )


stepTimes : Int -> Model -> Model
stepTimes n model =
    if isRunning model.state then
        doNTimes n stepIfRunning model

    else
        model


isRunning : State -> Bool
isRunning state =
    case state of
        Running ->
            True

        Paused ->
            False

        Halted _ ->
            False


doNTimes : Int -> (a -> a) -> a -> a
doNTimes n fn value =
    if n <= 0 then
        value

    else
        doNTimes (n - 1) fn (fn value)


stepIfRunning : Model -> Model
stepIfRunning model =
    if isRunning model.state then
        step model

    else
        model


step : Model -> Model
step model =
    case Memory.getInstruction model.pc model.memory of
        Err err ->
            { model | state = Halted err }

        Ok instruction ->
            model
                |> runInstruction instruction
                |> incrementPCIfNeeded instruction


incrementPCIfNeeded : Instruction -> Model -> Model
incrementPCIfNeeded instruction model =
    if shouldIncrementPC instruction then
        incrementPC model

    else
        model


incrementPC : Model -> Model
incrementPC model =
    let
        (Address pc) =
            model.pc
    in
    { model | pc = Address (pc + 2) }


shouldIncrementPC : Instruction -> Bool
shouldIncrementPC instruction =
    case instruction of
        Clear ->
            True

        Return ->
            False

        Jump _ ->
            False

        Call _ ->
            False

        DoIfNeq _ _ ->
            True

        DoIfEq _ _ ->
            True

        DoIfNeqReg _ _ ->
            True

        SetRegConst _ _ ->
            True

        AddRegConst _ _ ->
            True

        SetRegReg _ ->
            True

        OrRegReg _ ->
            True

        AndRegReg _ ->
            True

        XorRegReg _ ->
            True

        AddRegReg _ ->
            True

        SubRegReg _ ->
            True

        ShiftRightRegReg _ ->
            True

        SubReverseRegReg _ ->
            True

        ShiftLeftRegReg _ ->
            True

        DoIfEqReg _ _ ->
            True

        SetI _ ->
            True

        JumpPlusV0 _ ->
            False

        SetRandomAnd _ _ ->
            True

        DrawSprite _ ->
            True

        DoIfKeyNotPressed _ ->
            True

        DoIfKeyPressed _ ->
            True

        GetDelayTimer _ ->
            True

        SetPressedKey _ ->
            True

        SetDelayTimer _ ->
            True

        SetAudioTimer _ ->
            True

        AddI _ ->
            True

        SetIToFontAddr _ ->
            True

        BcdDecode _ ->
            True

        SaveRegsUpTo _ ->
            True

        LoadRegsUpTo _ ->
            True


runInstruction : Instruction -> Model -> Model
runInstruction instruction model =
    let
        todo : Model -> Model
        todo m =
            { m | state = Halted (UnimplementedInstruction instruction) }
    in
    case instruction of
        Clear ->
            { model | memory = Memory.clearDisplay model.memory }

        Return ->
            case model.callStack of
                addr :: rest ->
                    { model
                        | pc = addr
                        , callStack = rest
                    }

                _ ->
                    { model | state = Halted ReturningWithEmptyCallStack }

        Jump addr ->
            { model | pc = addr }

        Call addr ->
            let
                (Address pc) =
                    model.pc
            in
            { model
                | pc = addr
                , callStack = Address (pc + 2) :: model.callStack
            }

        DoIfNeq reg (Byte byte) ->
            if Registers.get reg model.registers /= byte then
                model

            else
                incrementPC model

        DoIfEq reg byte ->
            todo model

        DoIfNeqReg reg1 reg2 ->
            todo model

        SetRegConst reg (Byte byte) ->
            { model | registers = Registers.set reg byte model.registers }

        AddRegConst reg (Byte byte) ->
            { model
                | registers =
                    Registers.map
                        reg
                        (\regValue -> min 255 (regValue + byte))
                        model.registers
            }

        SetRegReg { from, to } ->
            { model | registers = Registers.set to (Registers.get from model.registers) model.registers }

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
            let
                x : Int
                x =
                    Registers.get vx model.registers

                y : Int
                y =
                    Registers.get vy model.registers

                (Address i) =
                    model.i

                spriteRows : Result Error (List Byte)
                spriteRows =
                    List.range i (i + height - 1)
                        |> Result.combineMap (\addr -> Memory.get (Address addr) model.memory)

                xorBits : List Byte -> Result Error ( Memory, { hadCollision : Bool } )
                xorBits rows =
                    rows
                        |> List.indexedMap Tuple.pair
                        |> List.concatMap
                            (\( dy, Byte row ) ->
                                List.map2
                                    (\x_ bit ->
                                        let
                                            y_ : Int
                                            y_ =
                                                y + dy
                                        in
                                        ( ( x_, y_ ), bit )
                                    )
                                    (List.range x (x + 7))
                                    (Util.toBitList row)
                            )
                        |> List.foldl
                            (\( ( x_, y_ ), bit ) result ->
                                result
                                    |> Result.andThen
                                        (\( accMemory, { hadCollision } ) ->
                                            Memory.xorDisplayBit ( x_, y_ ) bit accMemory
                                                |> Result.map (Tuple.mapSecond (\new -> { hadCollision = hadCollision || new.hadCollision }))
                                        )
                            )
                            (Ok ( model.memory, { hadCollision = False } ))
            in
            case spriteRows |> Result.andThen xorBits of
                Err err ->
                    { model | state = Halted err }

                Ok ( newMemory, { hadCollision } ) ->
                    let
                        newVF : Int
                        newVF =
                            if hadCollision then
                                1

                            else
                                0
                    in
                    { model
                        | memory = newMemory
                        , registers = Registers.set VF newVF model.registers
                    }

        DoIfKeyNotPressed reg ->
            todo model

        DoIfKeyPressed reg ->
            todo model

        GetDelayTimer reg ->
            { model | registers = Registers.set reg model.delayTimer model.registers }

        SetPressedKey reg ->
            todo model

        SetDelayTimer reg ->
            { model | delayTimer = Registers.get reg model.registers }

        SetAudioTimer reg ->
            todo model

        AddI reg ->
            let
                (Address i) =
                    model.i
            in
            { model | i = Address ((i + Registers.get reg model.registers) |> modBy 0x0FFF) }

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
