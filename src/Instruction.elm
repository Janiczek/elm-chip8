module Instruction exposing
    ( Address(..)
    , Byte(..)
    , Instruction(..)
    , arguments
    , toString
    )

import Bitwise
import Registers exposing (Register)
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
    | ShiftRightRegReg { from : Register, to : Register }
    | SubReverseRegReg { from : Register, to : Register }
    | ShiftLeftRegReg { from : Register, to : Register }
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

        ShiftRightRegReg _ ->
            "ShiftRightRegReg"

        SubReverseRegReg _ ->
            "SubReverseRegReg"

        ShiftLeftRegReg _ ->
            "ShiftLeftRegReg"

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


arguments : Instruction -> String
arguments instruction =
    let
        twoRegs : Register -> Register -> String
        twoRegs r1 r2 =
            Registers.name r1 ++ ", " ++ Registers.name r2

        fromTo : Register -> Register -> String
        fromTo from to =
            Registers.name from ++ " -> " ++ Registers.name to
    in
    case instruction of
        Clear ->
            ""

        Return ->
            ""

        Jump (Address addr) ->
            Util.hex addr

        Call (Address addr) ->
            Util.hex addr

        DoIfNeq reg (Byte byte) ->
            Registers.name reg ++ ", " ++ Util.hex byte

        DoIfEq reg (Byte byte) ->
            Registers.name reg ++ ", " ++ Util.hex byte

        DoIfNeqReg reg1 reg2 ->
            twoRegs reg1 reg2

        SetRegConst reg (Byte byte) ->
            Registers.name reg ++ ", " ++ Util.hex byte

        AddRegConst reg (Byte byte) ->
            Registers.name reg ++ ", " ++ Util.hex byte

        SetRegReg { from, to } ->
            fromTo from to

        OrRegReg { from, to } ->
            fromTo from to

        AndRegReg { from, to } ->
            fromTo from to

        XorRegReg { from, to } ->
            fromTo from to

        AddRegReg { from, to } ->
            fromTo from to

        SubRegReg { from, to } ->
            fromTo from to

        ShiftRightRegReg { from, to } ->
            fromTo from to

        SubReverseRegReg { from, to } ->
            fromTo from to

        ShiftLeftRegReg { from, to } ->
            fromTo from to

        DoIfEqReg reg1 reg2 ->
            twoRegs reg1 reg2

        SetI (Address addr) ->
            Util.hex addr

        JumpPlusV0 (Address addr) ->
            Util.hex addr

        SetRandomAnd reg (Byte byte) ->
            Registers.name reg ++ ", " ++ Util.hex byte

        DrawSprite { vx, vy, height } ->
            Registers.name vx ++ ", " ++ Registers.name vy ++ ", " ++ String.fromInt height

        DoIfKeyNotPressed reg ->
            Registers.name reg

        DoIfKeyPressed reg ->
            Registers.name reg

        GetDelayTimer reg ->
            Registers.name reg

        SetPressedKey reg ->
            Registers.name reg

        SetDelayTimer reg ->
            Registers.name reg

        SetAudioTimer reg ->
            Registers.name reg

        AddI reg ->
            Registers.name reg

        SetIToFontAddr reg ->
            Registers.name reg

        BcdDecode reg ->
            Registers.name reg

        SaveRegsUpTo reg ->
            Registers.name reg

        LoadRegsUpTo reg ->
            Registers.name reg
