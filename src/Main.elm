module Main exposing (Flags, Model, Msg, State, main)

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
import Json.Decode as Decode exposing (Decoder)
import List.Extra as List
import List.Zipper as Zipper exposing (Zipper)
import Maybe.Extra as Maybe
import Memory exposing (Memory)
import RadixInt
import Random exposing (Generator)
import Registers exposing (Register(..), Registers)
import Result.Extra as Result
import Set exposing (Set)
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
    { initialSeed : Int
    , computer : Zipper Computer
    , pressedKeys : Set Int
    }


type Msg
    = Tick Float
    | DelayTimerTick
    | RunClicked
    | PauseClicked
    | ResetClicked
    | StepClicked
    | PreviousStateClicked
    | NextStateClicked
    | NewRandomSeedClicked
    | KeyDown Int
    | KeyUp Int
    | ProgramSelected ProgramROM


type State
    = Running
    | Paused
    | Halted Error
    | WaitingForKey Register


type alias Computer =
    { memory : Memory
    , pc : Address
    , i : Address
    , registers : Registers
    , delayTimer : Int
    , callStack : List Address
    , state : State
    , randomSeed : Random.Seed
    , rom : ProgramROM
    }


initROM : ProgramROM
initROM =
    FramedMk2


initComputer : Int -> ProgramROM -> Computer
initComputer initialSeed rom =
    { memory = Memory.init |> Memory.loadProgram (ExamplePrograms.program rom)
    , pc = Address Memory.programStart
    , i = Address 0
    , registers = Registers.init
    , delayTimer = 0
    , callStack = []
    , state = Paused
    , randomSeed = Random.initialSeed initialSeed
    , rom = rom
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { initialSeed = flags.initialSeed
      , computer = Zipper.singleton (initComputer flags.initialSeed initROM)
      , pressedKeys = Set.empty
      }
    , Cmd.none
    )


loadROM : ProgramROM -> Model -> Model
loadROM rom model =
    { model | computer = Zipper.singleton (initComputer model.initialSeed rom) }


reset : Model -> Model
reset model =
    let
        currentRom : ProgramROM
        currentRom =
            (Zipper.current model.computer).rom
    in
    { model | computer = Zipper.singleton (initComputer model.initialSeed currentRom) }


view : Model -> Browser.Document Msg
view model =
    let
        computer : Computer
        computer =
            Zipper.current model.computer
    in
    { title = "CHIP-8 Emulator"
    , body =
        [ Html.div
            [ Attrs.style "display" "flex"
            , Attrs.style "gap" "8px"
            ]
            [ Html.div
                [ Attrs.style "display" "flex"
                , Attrs.style "flex-direction" "column"
                , Attrs.style "gap" "8px"
                ]
                [ Display.view computer.memory
                , Memory.view
                    { pc = computer.pc
                    , i = computer.i
                    }
                    computer.memory
                , viewButtons model.computer
                , viewHistory model.computer
                , viewState computer.state
                , viewRomButtons
                ]
            , Html.div []
                [ viewRegisters model.initialSeed computer
                , viewDisassembledCode computer
                , viewCallStack computer.callStack
                , viewPressedKeys model.pressedKeys

                -- TODO view sprite currently under I
                ]
            ]
        ]
    }


viewRomButtons : Html Msg
viewRomButtons =
    Html.div []
        [ Html.h4 [] [ Html.text "Load a ROM:" ]
        , Html.div []
            (ExamplePrograms.all
                |> List.greedyGroupsOf 5
                |> List.map
                    (\programs ->
                        Html.div []
                            (List.map
                                (\program ->
                                    Html.button
                                        [ Events.onClick (ProgramSelected program) ]
                                        [ Html.text <| ExamplePrograms.name program ]
                                )
                                programs
                            )
                    )
            )
        ]


viewState : State -> Html msg
viewState state =
    Html.div
        [ Attrs.style "max-width" "300px" ]
        [ Html.h2 [] [ Html.text "State" ]
        , case state of
            Running ->
                Html.text "Running"

            Paused ->
                Html.text "Paused"

            Halted err ->
                Html.text <| "Halted: " ++ Error.toString err

            WaitingForKey reg ->
                Html.text <| "Waiting for key on register " ++ Registers.name reg
        ]


viewButtons : Zipper Computer -> Html Msg
viewButtons computer =
    let
        currentState : State
        currentState =
            (Zipper.current computer).state
    in
    Html.div []
        [ Html.div []
            [ Html.button
                [ Events.onClick RunClicked
                , Attrs.disabled (isRunning currentState || isWaitingForKey currentState)
                ]
                [ Html.text "Run" ]
            , Html.button
                [ Events.onClick StepClicked ]
                [ Html.text "Step" ]
            , Html.button
                [ Events.onClick PauseClicked
                , Attrs.disabled (not (isRunning currentState || isWaitingForKey currentState))
                ]
                [ Html.text "Pause" ]
            , Html.button
                [ Events.onClick ResetClicked ]
                [ Html.text "Reset" ]
            , Html.button
                [ Events.onClick NewRandomSeedClicked ]
                [ Html.text "New random seed" ]
            ]
        , Html.div []
            [ Html.button
                [ Events.onClick PreviousStateClicked
                , Attrs.disabled (isRunning currentState || isWaitingForKey currentState || Zipper.isFirst computer)
                ]
                [ Html.text "← Previous" ]
            , Html.button
                [ Events.onClick NextStateClicked
                , Attrs.disabled (isRunning currentState || isWaitingForKey currentState || Zipper.isLast computer)
                ]
                [ Html.text "Next →" ]
            ]
        ]


viewHistory : Zipper Computer -> Html msg
viewHistory computer =
    -- TODO let user click on a state to skip to it?
    let
        size : String
        size =
            "5px"

        emptyDots : Int -> List (Html msg)
        emptyDots n =
            List.repeat n emptyDot

        emptyDot : Html msg
        emptyDot =
            dot "white"

        fullDot : Html msg
        fullDot =
            dot "red"

        dot : String -> Html msg
        dot color =
            Html.div
                [ Attrs.style "min-width" size
                , Attrs.style "min-height" size
                , Attrs.style "background-color" color
                , Attrs.style "border" "1px solid black"
                ]
                []
    in
    Html.div
        [ Attrs.style "display" "flex"
        , Attrs.style "flex-wrap" "wrap"
        , Attrs.style "gap" "1px"
        , Attrs.style "padding-top" "8px"
        , Attrs.style "max-width" "300px"
        ]
        ([ emptyDots (List.length (Zipper.before computer))
         , [ fullDot ] -- present
         , emptyDots (List.length (Zipper.after computer))
         ]
            |> List.concat
        )


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


viewPressedKeys : Set Int -> Html msg
viewPressedKeys pressedKeys =
    Html.div []
        [ Html.h2 [] [ Html.text "Pressed keys" ]
        , Html.div [] [ Html.text <| String.join ", " (List.map Util.hex (Set.toList pressedKeys)) ]
        ]


viewRegisters : Int -> Computer -> Html msg
viewRegisters initialSeed { pc, i, delayTimer, registers } =
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
                :: viewRegister { special = True } "Initial random seed" initialSeed
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


viewDisassembledCode : Computer -> Html msg
viewDisassembledCode computer =
    let
        (Address pc) =
            computer.pc

        range : List ( Address, ( Byte, Byte ) )
        range =
            List.range -5 5
                |> List.filterMap
                    (\delta ->
                        let
                            address1 : Address
                            address1 =
                                Address (pc + 2 * delta)

                            address2 : Address
                            address2 =
                                Address (pc + 2 * delta + 1)
                        in
                        Result.map2 (\hi lo -> ( address1, ( hi, lo ) ))
                            (Memory.get address1 computer.memory)
                            (Memory.get address2 computer.memory)
                            |> Result.toMaybe
                    )

        viewLine : ( Address, ( Byte, Byte ) ) -> Html msg
        viewLine ( Address addr, ( (Byte hi_) as hi, (Byte lo_) as lo ) ) =
            let
                ( instruction, code ) =
                    case Instruction.Parser.parse ( hi, lo ) of
                        Err _ ->
                            ( "", "" )

                        Ok instr ->
                            ( Instruction.toString instr
                            , Instruction.code instr
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
                , Html.td [ cellPadding, Attrs.style "color" "grey" ] [ Html.text instruction ]
                , Html.td [ cellPadding ] [ Html.text code ]
                ]

        cellPadding : Html.Attribute msg
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
                    [ Html.th [ cellPadding, Attrs.style "text-align" "left" ] [ Html.text "Address" ]
                    , Html.th [ cellPadding, Attrs.style "text-align" "left" ] [ Html.text "Opcode" ]
                    , Html.th [ cellPadding, Attrs.style "text-align" "left" ] [ Html.text "Instruction" ]
                    , Html.th [ cellPadding, Attrs.style "text-align" "left" ] [ Html.text "Code" ]
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
    let
        computer : Computer
        computer =
            Zipper.current model.computer
    in
    Sub.batch
        [ if isRunning computer.state then
            Browser.Events.onAnimationFrameDelta Tick

          else
            Sub.none
        , if computer.delayTimer > 0 then
            Time.every sixtyHertz (\_ -> DelayTimerTick)

          else
            Sub.none
        , Browser.Events.onKeyDown (keyDecoder KeyDown)
        , Browser.Events.onKeyUp (keyDecoder KeyUp)
        ]


keyDecoder : (Int -> msg) -> Decoder msg
keyDecoder toMsg =
    Decode.field "key" Decode.string
        |> Decode.andThen toKey
        |> Decode.map toMsg


toKey : String -> Decoder Int
toKey string =
    case String.uncons string of
        Just ( char, "" ) ->
            case Char.toLower char of
                '0' ->
                    Decode.succeed 0

                '1' ->
                    Decode.succeed 1

                '2' ->
                    Decode.succeed 2

                '3' ->
                    Decode.succeed 3

                '4' ->
                    Decode.succeed 4

                '5' ->
                    Decode.succeed 5

                '6' ->
                    Decode.succeed 6

                '7' ->
                    Decode.succeed 7

                '8' ->
                    Decode.succeed 8

                '9' ->
                    Decode.succeed 9

                'a' ->
                    Decode.succeed 10

                'b' ->
                    Decode.succeed 11

                'c' ->
                    Decode.succeed 12

                'd' ->
                    Decode.succeed 13

                'e' ->
                    Decode.succeed 14

                'f' ->
                    Decode.succeed 15

                _ ->
                    Decode.fail "unsupported key"

        _ ->
            Decode.fail "unsupported key"


mapComputer : (Computer -> Computer) -> Model -> Model
mapComputer fn model =
    { model | computer = Zipper.mapCurrent fn model.computer }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick msDelta ->
            ( { model | computer = stepTimes (round msDelta) model.pressedKeys model.computer }
            , Cmd.none
            )

        DelayTimerTick ->
            ( model
                |> mapComputer (\c -> { c | delayTimer = c.delayTimer - 1 })
            , Cmd.none
            )

        RunClicked ->
            ( { model | computer = Zipper.mapAfter (\_ -> []) model.computer }
                |> mapComputer (\c -> { c | state = Running })
            , Cmd.none
            )

        PauseClicked ->
            ( model
                |> mapComputer (\c -> { c | state = Paused })
            , Cmd.none
            )

        ResetClicked ->
            ( reset model
            , Cmd.none
            )

        NewRandomSeedClicked ->
            ( { model
                | initialSeed =
                    Random.step
                        (Random.int 0 Random.maxInt)
                        (Random.initialSeed model.initialSeed)
                        |> Tuple.first
              }
            , Cmd.none
            )

        StepClicked ->
            ( { model
                | computer =
                    model.computer
                        |> Zipper.mapAfter (\_ -> [])
                        |> step model.pressedKeys
              }
            , Cmd.none
            )

        PreviousStateClicked ->
            ( { model
                | computer =
                    model.computer
                        |> Zipper.previous
                        |> Maybe.map (Zipper.mapCurrent pauseIfRunning)
                        |> Maybe.withDefault model.computer
              }
            , Cmd.none
            )

        NextStateClicked ->
            ( { model
                | computer =
                    model.computer
                        |> Zipper.next
                        |> Maybe.map (Zipper.mapCurrent pauseIfRunning)
                        |> Maybe.withDefault model.computer
              }
            , Cmd.none
            )

        ProgramSelected program ->
            ( loadROM program model
            , Cmd.none
            )

        KeyDown n ->
            let
                modelWithKey : Model
                modelWithKey =
                    { model | pressedKeys = Set.insert n model.pressedKeys }

                computer : Computer
                computer =
                    Zipper.current model.computer
            in
            ( case computer.state of
                WaitingForKey reg ->
                    { modelWithKey
                        | computer =
                            modelWithKey.computer
                                |> stepComputer (setPressedKey reg n)
                    }

                Running ->
                    modelWithKey

                Halted _ ->
                    modelWithKey

                Paused ->
                    modelWithKey
            , Cmd.none
            )

        KeyUp n ->
            ( { model | pressedKeys = Set.remove n model.pressedKeys }
            , Cmd.none
            )


pauseIfRunning : Computer -> Computer
pauseIfRunning computer =
    if isRunning computer.state || isWaitingForKey computer.state then
        { computer | state = Paused }

    else
        computer


stepTimes : Int -> Set Int -> Zipper Computer -> Zipper Computer
stepTimes n pressedKeys computer =
    let
        currentComputer =
            Zipper.current computer
    in
    if isRunning currentComputer.state then
        doNTimes n (stepIfRunning pressedKeys) computer

    else
        computer


isRunning : State -> Bool
isRunning state =
    case state of
        Running ->
            True

        Paused ->
            False

        Halted _ ->
            False

        WaitingForKey _ ->
            False


isWaitingForKey : State -> Bool
isWaitingForKey state =
    case state of
        Running ->
            False

        Paused ->
            False

        Halted _ ->
            False

        WaitingForKey _ ->
            True


doNTimes : Int -> (a -> a) -> a -> a
doNTimes n fn value =
    if n <= 0 then
        value

    else
        doNTimes (n - 1) fn (fn value)


stepIfRunning : Set Int -> Zipper Computer -> Zipper Computer
stepIfRunning pressedKeys computer =
    let
        currentComputer =
            Zipper.current computer
    in
    if isRunning currentComputer.state then
        step pressedKeys computer

    else
        computer


stepComputer : (Computer -> Computer) -> Zipper Computer -> Zipper Computer
stepComputer fn computer =
    let
        currentComputer : Computer
        currentComputer =
            Zipper.current computer

        newComputer : Computer
        newComputer =
            fn currentComputer
    in
    computer
        |> Zipper.mapAfter (\future -> newComputer :: future)
        |> Zipper.next
        |> Maybe.withDefault computer
        |> Zipper.mapBefore
            (\past ->
                let
                    n =
                        List.length past
                in
                List.drop (n - 10) past
            )


step : Set Int -> Zipper Computer -> Zipper Computer
step pressedKeys =
    stepComputer
        (\currentComputer ->
            case Memory.getInstruction currentComputer.pc currentComputer.memory of
                Err err ->
                    { currentComputer | state = Halted err }

                Ok instruction ->
                    if instruction == Jump currentComputer.pc then
                        { currentComputer | state = Halted (InfiniteLoop currentComputer.pc) }

                    else
                        currentComputer
                            |> runInstruction pressedKeys instruction
                            |> incrementPCIfNeeded instruction
        )


incrementPCIfNeeded : Instruction -> Computer -> Computer
incrementPCIfNeeded instruction computer =
    if shouldIncrementPC instruction && (isRunning computer.state || computer.state == Paused) then
        incrementPC computer

    else
        computer


incrementPC : Computer -> Computer
incrementPC computer =
    let
        (Address pc) =
            computer.pc
    in
    { computer | pc = Address (pc + 2) }


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

        ShiftRightBy1 _ ->
            True

        SubReverseRegReg _ ->
            True

        ShiftLeftBy1 _ ->
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

        MagicFn _ ->
            True


todo : Instruction -> Computer -> Computer
todo instruction c =
    { c | state = Halted (UnimplementedInstruction instruction) }


runInstruction : Set Int -> Instruction -> Computer -> Computer
runInstruction pressedKeys instruction computer =
    case instruction of
        Clear ->
            { computer | memory = Memory.clearDisplay computer.memory }

        Return ->
            case computer.callStack of
                addr :: rest ->
                    { computer
                        | pc = addr
                        , callStack = rest
                    }

                _ ->
                    { computer | state = Halted ReturningWithEmptyCallStack }

        Jump addr ->
            { computer | pc = addr }

        Call addr ->
            let
                (Address pc) =
                    computer.pc
            in
            { computer
                | pc = addr
                , callStack = Address (pc + 2) :: computer.callStack
            }

        DoIfNeq reg (Byte byte) ->
            if Registers.get reg computer.registers /= byte then
                computer

            else
                incrementPC computer

        DoIfEq reg (Byte byte) ->
            if Registers.get reg computer.registers == byte then
                computer

            else
                incrementPC computer

        DoIfNeqReg reg1 reg2 ->
            if Registers.get reg1 computer.registers /= Registers.get reg2 computer.registers then
                computer

            else
                incrementPC computer

        SetRegConst reg (Byte byte) ->
            { computer | registers = Registers.set reg byte computer.registers }

        AddRegConst reg (Byte byte) ->
            { computer
                | registers =
                    Registers.map
                        reg
                        (\regValue -> (regValue + byte) |> modBy 0x0100)
                        computer.registers
            }

        SetRegReg { from, to } ->
            { computer | registers = Registers.set to (Registers.get from computer.registers) computer.registers }

        OrRegReg { from, to } ->
            { computer
                | registers =
                    Registers.map to
                        (\oldTo -> Bitwise.or oldTo (Registers.get from computer.registers))
                        computer.registers
            }

        AndRegReg { from, to } ->
            { computer
                | registers =
                    Registers.map to
                        (\oldTo -> Bitwise.and oldTo (Registers.get from computer.registers))
                        computer.registers
            }

        XorRegReg { from, to } ->
            { computer
                | registers =
                    Registers.map to
                        (\oldTo -> Bitwise.xor oldTo (Registers.get from computer.registers))
                        computer.registers
            }

        AddRegReg { from, to } ->
            let
                oldTo : Int
                oldTo =
                    Registers.get to computer.registers

                from_ : Int
                from_ =
                    Registers.get from computer.registers

                rawNewTo : Int
                rawNewTo =
                    oldTo + from_

                ( newTo, newVF ) =
                    if rawNewTo >= 0x0100 then
                        ( rawNewTo - 0x0100, 1 )

                    else
                        ( rawNewTo, 0 )
            in
            { computer
                | registers =
                    computer.registers
                        |> Registers.set to newTo
                        |> Registers.set VF newVF
            }

        SubRegReg { from, to } ->
            let
                oldTo : Int
                oldTo =
                    Registers.get to computer.registers

                from_ : Int
                from_ =
                    Registers.get from computer.registers

                rawNewTo : Int
                rawNewTo =
                    oldTo - from_

                ( newTo, newVF ) =
                    if rawNewTo < 0 then
                        ( rawNewTo + 0x0100, 0 )

                    else
                        ( rawNewTo, 1 )
            in
            { computer
                | registers =
                    computer.registers
                        |> Registers.set to newTo
                        |> Registers.set VF newVF
            }

        ShiftRightBy1 { from, to } ->
            let
                from_ : Int
                from_ =
                    Registers.get from computer.registers

                newValue : Int
                newValue =
                    from_
                        -- TODO should this be zero-fill?
                        |> Bitwise.shiftRightZfBy 1

                newVF : Int
                newVF =
                    Bitwise.and 0x01 from_
            in
            { computer
                | registers =
                    computer.registers
                        |> Registers.set to newValue
                        |> Registers.set VF newVF
            }

        SubReverseRegReg { from, to } ->
            let
                oldTo : Int
                oldTo =
                    Registers.get to computer.registers

                from_ : Int
                from_ =
                    Registers.get from computer.registers

                rawNewTo : Int
                rawNewTo =
                    from_ - oldTo

                ( newTo, newVF ) =
                    if rawNewTo < 0 then
                        ( rawNewTo + 0x0100, 0 )

                    else
                        ( rawNewTo, 1 )
            in
            { computer
                | registers =
                    computer.registers
                        |> Registers.set to newTo
                        |> Registers.set VF newVF
            }

        ShiftLeftBy1 { from, to } ->
            let
                from_ : Int
                from_ =
                    Registers.get from computer.registers

                newValue : Int
                newValue =
                    from_
                        |> Bitwise.shiftLeftBy 1
                        |> modBy 0x0100

                newVF : Int
                newVF =
                    if from_ > 0x7F then
                        1

                    else
                        0
            in
            { computer
                | registers =
                    computer.registers
                        |> Registers.set to newValue
                        |> Registers.set VF newVF
            }

        DoIfEqReg reg1 reg2 ->
            if Registers.get reg1 computer.registers == Registers.get reg2 computer.registers then
                computer

            else
                incrementPC computer

        SetI addr ->
            { computer | i = addr }

        JumpPlusV0 (Address addr) ->
            {- TODO is this supposed to wrap around? ie. addr 0xFFF + (v0 = 0x05)
               -> should it crash out of bounds?
               -> or should it wrap around to something like 0x004
            -}
            { computer | pc = Address (addr + Registers.get V0 computer.registers) }

        SetRandomAnd register (Byte mask) ->
            let
                ( randomByte, newSeed ) =
                    Random.step byteGenerator computer.randomSeed

                maskedByte : Int
                maskedByte =
                    Bitwise.and mask randomByte
            in
            { computer
                | randomSeed = newSeed
                , registers = Registers.set register maskedByte computer.registers
            }

        DrawSprite { vx, vy, height } ->
            let
                x : Int
                x =
                    Registers.get vx computer.registers

                y : Int
                y =
                    Registers.get vy computer.registers

                (Address i) =
                    computer.i

                spriteRows : Result Error (List Byte)
                spriteRows =
                    List.range i (i + height - 1)
                        |> Result.combineMap (\addr -> Memory.get (Address addr) computer.memory)

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
                            (Ok ( computer.memory, { hadCollision = False } ))
            in
            case spriteRows |> Result.andThen xorBits of
                Err err ->
                    { computer | state = Halted err }

                Ok ( newMemory, { hadCollision } ) ->
                    let
                        newVF : Int
                        newVF =
                            if hadCollision then
                                1

                            else
                                0
                    in
                    { computer
                        | memory = newMemory
                        , registers = Registers.set VF newVF computer.registers
                    }

        DoIfKeyNotPressed reg ->
            if not <| Set.member (Registers.get reg computer.registers) pressedKeys then
                computer

            else
                incrementPC computer

        DoIfKeyPressed reg ->
            if Set.member (Registers.get reg computer.registers) pressedKeys then
                computer

            else
                incrementPC computer

        GetDelayTimer reg ->
            { computer | registers = Registers.set reg computer.delayTimer computer.registers }

        SetPressedKey reg ->
            case Set.toList pressedKeys of
                [] ->
                    { computer | state = WaitingForKey reg }

                key :: rest ->
                    setPressedKey reg key computer

        SetDelayTimer reg ->
            { computer | delayTimer = Registers.get reg computer.registers }

        SetAudioTimer _ ->
            todo instruction computer

        AddI reg ->
            let
                (Address i) =
                    computer.i
            in
            { computer | i = Address ((i + Registers.get reg computer.registers) |> modBy 0x1000) }

        SetIToFontAddr reg ->
            let
                hexDigit : Int
                hexDigit =
                    Registers.get reg computer.registers
                        |> modBy 0x10

                address : Address
                address =
                    Address (hexDigit * 5)
            in
            { computer | i = address }

        BcdDecode reg ->
            let
                value : Int
                value =
                    Registers.get reg computer.registers

                decimalDigits : List Int
                decimalDigits =
                    value
                        |> RadixInt.fromInt (RadixInt.Base 10)
                        |> RadixInt.toList
                        |> List.reverse
                        |> Util.zeroPadLeft 3

                (Address i) =
                    computer.i

                addressesAndDigits : List ( Int, Int )
                addressesAndDigits =
                    List.map2 Tuple.pair
                        (List.range i (i + 2))
                        decimalDigits

                savedToMemory : Result Error Memory
                savedToMemory =
                    List.foldl
                        (\( addr, digit ) accMemory ->
                            accMemory
                                |> Result.andThen (\mem -> Memory.set (Address addr) digit mem)
                        )
                        (Ok computer.memory)
                        addressesAndDigits
            in
            case savedToMemory of
                Err err ->
                    { computer | state = Halted err }

                Ok newMemory ->
                    { computer | memory = newMemory }

        SaveRegsUpTo reg ->
            let
                regs : List Register
                regs =
                    Registers.upTo reg

                (Address i) =
                    computer.i

                bytesToSave : List Int
                bytesToSave =
                    List.map (\reg_ -> Registers.get reg_ computer.registers) regs

                savedToMemory : Result Error Memory
                savedToMemory =
                    bytesToSave
                        |> List.indexedMap Tuple.pair
                        |> List.foldl
                            (\( di, byte ) accMemory ->
                                accMemory
                                    |> Result.andThen (\mem -> Memory.set (Address (i + di)) byte mem)
                            )
                            (Ok computer.memory)
            in
            case savedToMemory of
                Err err ->
                    { computer | state = Halted err }

                Ok newMemory ->
                    { computer | memory = newMemory }

        LoadRegsUpTo reg ->
            let
                n : Int
                n =
                    Registers.index reg

                (Address i) =
                    computer.i

                loadedBytes : Result Error (List Byte)
                loadedBytes =
                    List.range i (i + n)
                        |> Result.combineMap (\addr -> Memory.get (Address addr) computer.memory)
            in
            case loadedBytes of
                Err err ->
                    { computer | state = Halted err }

                Ok bytes ->
                    { computer
                        | registers =
                            List.foldl
                                (\( reg_, Byte byte ) accRegisters -> Registers.set reg_ byte accRegisters)
                                computer.registers
                                (List.map2 Tuple.pair (Registers.upTo reg) bytes)
                    }

        MagicFn _ ->
            todo instruction computer


setPressedKey : Register -> Int -> Computer -> Computer
setPressedKey reg key computer =
    { computer
        | registers = Registers.set reg key computer.registers
        , state = Running
    }


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
