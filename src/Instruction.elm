module Instruction exposing
    ( Address(..)
    , Byte(..)
    , Instruction(..)
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
