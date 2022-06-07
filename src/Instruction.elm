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
            Debug.toString reg ++ ", " ++ Util.hex byte

        DoIfEq reg (Byte byte) ->
            Debug.toString reg ++ ", " ++ Util.hex byte

        DoIfNeqReg reg1 reg2 ->
            Debug.toString ( reg1, reg2 )

        SetRegConst reg (Byte byte) ->
            Debug.toString reg ++ ", " ++ Util.hex byte

        AddRegConst reg (Byte byte) ->
            Debug.toString reg ++ ", " ++ Util.hex byte

        SetRegReg { from, to } ->
            Debug.toString ( from, to )

        OrRegReg { from, to } ->
            Debug.toString ( from, to )

        AndRegReg { from, to } ->
            Debug.toString ( from, to )

        XorRegReg { from, to } ->
            Debug.toString ( from, to )

        AddRegReg { from, to } ->
            Debug.toString ( from, to )

        SubRegReg { from, to } ->
            Debug.toString ( from, to )

        ShiftRightRegReg { from, to } ->
            Debug.toString ( from, to )

        SubReverseRegReg { from, to } ->
            Debug.toString ( from, to )

        ShiftLeftRegReg { from, to } ->
            Debug.toString ( from, to )

        DoIfEqReg reg1 reg2 ->
            Debug.toString ( reg1, reg2 )

        SetI (Address addr) ->
            Util.hex addr

        JumpPlusV0 (Address addr) ->
            Util.hex addr

        SetRandomAnd reg (Byte byte) ->
            Debug.toString reg ++ ", " ++ Util.hex byte

        DrawSprite { vx, vy, height } ->
            Debug.toString ( vx, vy, height )

        DoIfKeyNotPressed reg ->
            Debug.toString reg

        DoIfKeyPressed reg ->
            Debug.toString reg

        GetDelayTimer reg ->
            Debug.toString reg

        SetPressedKey reg ->
            Debug.toString reg

        SetDelayTimer reg ->
            Debug.toString reg

        SetAudioTimer reg ->
            Debug.toString reg

        AddI reg ->
            Debug.toString reg

        SetIToFontAddr reg ->
            Debug.toString reg

        BcdDecode reg ->
            Debug.toString reg

        SaveRegsUpTo reg ->
            Debug.toString reg

        LoadRegsUpTo reg ->
            Debug.toString reg
