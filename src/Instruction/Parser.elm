module Instruction.Parser exposing (parse)

import Bitwise
import Error exposing (Error(..))
import Instruction
    exposing
        ( Address(..)
        , Byte(..)
        , Instruction(..)
        )
import Registers exposing (Register)
import Util


address : ( Byte, Byte ) -> Address
address ( Byte hi, Byte lo ) =
    Util.bytePair ( hi, lo )
        |> Bitwise.and 0x0FFF
        |> Address


registerLo : Int -> Result Error Register
registerLo byte =
    let
        reg : Int
        reg =
            loNibble byte
    in
    Registers.register reg
        |> Result.fromMaybe (ParsedInvalidRegister reg)


registerHi : Int -> Result Error Register
registerHi byte =
    let
        reg : Int
        reg =
            hiNibble byte
    in
    Registers.register reg
        |> Result.fromMaybe (ParsedInvalidRegister reg)


hiNibble : Int -> Int
hiNibble byte =
    byte
        |> Bitwise.and 0xF0
        |> Bitwise.shiftRightZfBy 4


loNibble : Int -> Int
loNibble byte =
    byte
        |> Bitwise.and 0x0F


parse : ( Byte, Byte ) -> Result Error Instruction
parse (( Byte hi, (Byte lo) as lo_ ) as pair) =
    let
        err : Result Error Instruction
        err =
            Err (UnknownInstruction ( hi, lo ))

        hh : Int
        hh =
            hiNibble hi

        ll : Int
        ll =
            loNibble lo
    in
    if hi == 0x00 && lo == 0xE0 then
        Ok Clear

    else if hi == 0x00 && lo == 0xEE then
        Ok Return

    else if hh == 0x01 then
        Ok (Jump (address pair))

    else if hh == 0x02 then
        Ok (Call (address pair))

    else if hh == 0x03 then
        registerLo hi
            |> Result.map (\reg -> DoIfNeq reg lo_)

    else if hh == 0x04 then
        registerLo hi
            |> Result.map (\reg -> DoIfEq reg lo_)

    else if hh == 0x05 then
        Result.map2 DoIfNeqReg
            (registerLo hi)
            (registerHi lo)

    else if hh == 0x06 then
        registerLo hi
            |> Result.map (\reg -> SetRegConst reg lo_)

    else if hh == 0x07 then
        registerLo hi
            |> Result.map (\reg -> AddRegConst reg lo_)

    else if hh == 0x08 && ll == 0x00 then
        Result.map2 (\vx vy -> SetRegReg { from = vy, to = vx })
            (registerLo hi)
            (registerHi lo)

    else if hh == 0x08 && ll == 0x02 then
        Result.map2 (\vx vy -> AndRegReg { from = vy, to = vx })
            (registerLo hi)
            (registerHi lo)

    else if hh == 0x08 && ll == 0x04 then
        Result.map2 (\vx vy -> AddRegReg { from = vy, to = vx })
            (registerLo hi)
            (registerHi lo)

    else if hh == 0x08 && ll == 0x0E then
        registerLo hi
            |> Result.map ShiftLeftBy1Reg

    else if hh == 0x0A then
        Ok (SetI (address pair))

    else if hh == 0x0C then
        registerLo hi
            |> Result.map (\reg -> SetRandomAnd reg lo_)

    else if hh == 0x0D then
        Result.map2
            (\x y ->
                DrawSprite
                    { vx = x
                    , vy = y
                    , height = loNibble lo
                    }
            )
            (registerLo hi)
            (registerHi lo)

    else if hh == 0x0F && lo == 0x07 then
        registerLo hi
            |> Result.map (\reg -> GetDelayTimer reg)

    else if hh == 0x0F && lo == 0x15 then
        registerLo hi
            |> Result.map (\reg -> SetDelayTimer reg)

    else if hh == 0x0F && lo == 0x1E then
        registerLo hi
            |> Result.map (\reg -> AddI reg)

    else if hh == 0x0F && lo == 0x55 then
        registerLo hi
            |> Result.map (\reg -> SaveRegsUpTo reg)

    else if hh == 0x0F && lo == 0x65 then
        registerLo hi
            |> Result.map (\reg -> LoadRegsUpTo reg)

    else
        err
