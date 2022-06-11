module Instruction exposing
    ( Address(..)
    , Byte(..)
    , Instruction(..)
    , code
    , toString
    )

import Registers exposing (Register(..))
import Util


type Address
    = -- 0 .. 4095
      -- 0x000 .. 0xFFF
      Address Int


type Byte
    = -- 0 .. 255
      -- 0x00 .. 0xFF
      Byte Int


type Instruction
    = Clear
    | Return
    | Jump Address
    | Call Address
    | DoIfNeq Register Byte
    | DoIfEq Register Byte
    | DoIfNeqReg Register Register
    | SetRegConst Register Byte
    | AddRegConst Register Byte
    | SetRegReg { from : Register, to : Register }
    | OrRegReg { from : Register, to : Register }
    | AndRegReg { from : Register, to : Register }
    | XorRegReg { from : Register, to : Register }
    | AddRegReg { from : Register, to : Register }
    | SubRegReg { from : Register, to : Register }
    | ShiftRightBy1 { from : Register, to : Register }
    | SubReverseRegReg { from : Register, to : Register }
    | ShiftLeftBy1 { from : Register, to : Register }
    | DoIfEqReg Register Register
    | SetI Address
    | JumpPlusV0 Address
    | SetRandomAnd Register Byte
    | DrawSprite { vx : Register, vy : Register, height : Int }
    | DoIfKeyNotPressed Register
    | DoIfKeyPressed Register
    | GetDelayTimer Register
    | SetPressedKey Register
    | SetDelayTimer Register
    | SetAudioTimer Register
    | AddI Register
    | SetIToFontAddr Register
    | BcdDecode Register
    | SaveRegsUpTo Register
    | LoadRegsUpTo Register


code : Instruction -> String
code instruction =
    let
        fn : String -> List String -> String
        fn name args =
            name ++ "(" ++ String.join ", " args ++ ")"

        setReg : Register -> String -> String
        setReg reg value =
            Registers.name reg ++ " = " ++ value
    in
    case instruction of
        Clear ->
            fn "clear_screen" []

        Return ->
            "return"

        Jump (Address addr) ->
            fn "jmp" [ Util.hex addr ]

        Call (Address addr) ->
            fn "call" [ Util.hex addr ]

        DoIfNeq reg (Byte byte) ->
            "if (" ++ Registers.name reg ++ " != " ++ Util.hex byte ++ ")"

        DoIfEq reg (Byte byte) ->
            "if (" ++ Registers.name reg ++ " == " ++ Util.hex byte ++ ")"

        DoIfNeqReg r1 r2 ->
            "if (" ++ Registers.name r1 ++ " != " ++ Registers.name r2 ++ ")"

        SetRegConst reg (Byte byte) ->
            setReg reg (Util.hex byte)

        AddRegConst reg (Byte byte) ->
            setReg reg (Registers.name reg ++ " + " ++ Util.hex byte)

        SetRegReg { from, to } ->
            setReg to (Registers.name from)

        OrRegReg { from, to } ->
            setReg to (Registers.name to ++ " | " ++ Registers.name from)

        AndRegReg { from, to } ->
            setReg to (Registers.name to ++ " & " ++ Registers.name from)

        XorRegReg { from, to } ->
            setReg to (Registers.name to ++ " ^ " ++ Registers.name from)

        AddRegReg { from, to } ->
            setReg to (Registers.name to ++ " + " ++ Registers.name from)

        SubRegReg { from, to } ->
            setReg to (Registers.name to ++ " - " ++ Registers.name from)

        ShiftRightBy1 { from, to } ->
            setReg to (Registers.name from ++ " >>> 1")

        SubReverseRegReg { from, to } ->
            setReg to (Registers.name from ++ " - " ++ Registers.name to)

        ShiftLeftBy1 { from, to } ->
            setReg to (Registers.name from ++ " << 1")

        DoIfEqReg r1 r2 ->
            "if (" ++ Registers.name r1 ++ " == " ++ Registers.name r2 ++ ")"

        SetI (Address addr) ->
            "i = " ++ Util.hex addr

        JumpPlusV0 (Address addr) ->
            fn "jmp" [ Util.hex addr ++ " + " ++ Registers.name V0 ]

        SetRandomAnd reg (Byte byte) ->
            setReg reg ("rand() && " ++ Util.hex byte)

        DrawSprite { vx, vy, height } ->
            fn "draw"
                [ "x=" ++ Registers.name vx
                , "y=" ++ Registers.name vy
                , "h=" ++ String.fromInt height
                ]

        DoIfKeyNotPressed _ ->
            "TODO"

        DoIfKeyPressed _ ->
            "TODO"

        GetDelayTimer _ ->
            "TODO"

        SetPressedKey _ ->
            "TODO"

        SetDelayTimer reg ->
            "delay_t = " ++ Registers.name reg

        SetAudioTimer reg ->
            "audio_t = " ++ Registers.name reg

        AddI reg ->
            "i = i + " ++ Registers.name reg

        SetIToFontAddr reg ->
            "i = font_address(" ++ Registers.name reg ++ ")"

        BcdDecode reg ->
            fn "bcd_decode" [ Registers.name reg ]

        SaveRegsUpTo reg ->
            fn "save_regs" [ Registers.name V0 ++ ".." ++ Registers.name reg ]

        LoadRegsUpTo reg ->
            fn "load_regs" [ Registers.name V0 ++ ".." ++ Registers.name reg ]


toString : Instruction -> String
toString instruction =
    case instruction of
        Clear ->
            "Clear"

        Return ->
            "Return"

        Jump _ ->
            "Jump"

        Call _ ->
            "Call"

        DoIfNeq _ _ ->
            "DoIfNeq"

        DoIfEq _ _ ->
            "DoIfEq"

        DoIfNeqReg _ _ ->
            "DoIfNeqReg"

        SetRegConst _ _ ->
            "SetRegConst"

        AddRegConst _ _ ->
            "AddRegConst"

        SetRegReg _ ->
            "SetRegReg"

        OrRegReg _ ->
            "OrRegReg"

        AndRegReg _ ->
            "AndRegReg"

        XorRegReg _ ->
            "XorRegReg"

        AddRegReg _ ->
            "AddRegReg"

        SubRegReg _ ->
            "SubRegReg"

        ShiftRightBy1 _ ->
            "ShiftRightBy1"

        SubReverseRegReg _ ->
            "SubReverseRegReg"

        ShiftLeftBy1 _ ->
            "ShiftLeftBy1"

        DoIfEqReg _ _ ->
            "DoIfEqReg"

        SetI _ ->
            "SetI"

        JumpPlusV0 _ ->
            "JumpPlusV0"

        SetRandomAnd _ _ ->
            "SetRandomAnd"

        DrawSprite _ ->
            "DrawSprite"

        DoIfKeyNotPressed _ ->
            "DoIfKeyNotPressed"

        DoIfKeyPressed _ ->
            "DoIfKeyPressed"

        GetDelayTimer _ ->
            "GetDelayTimer"

        SetPressedKey _ ->
            "SetPressedKey"

        SetDelayTimer _ ->
            "SetDelayTimer"

        SetAudioTimer _ ->
            "SetAudioTimer"

        AddI _ ->
            "AddI"

        SetIToFontAddr _ ->
            "SetIToFontAddr"

        BcdDecode _ ->
            "BcdDecode"

        SaveRegsUpTo _ ->
            "SaveRegsUpTo"

        LoadRegsUpTo _ ->
            "LoadRegsUpTo"
