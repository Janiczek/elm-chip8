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
        hh : Int
        hh =
            hiNibble hi
    in
    if hh == 0x01 then
        Ok (Jump (address pair))

    else if hh == 0x03 then
        registerLo hi
            |> Result.map (\reg -> DoIfNeq reg lo_)

    else if hh == 0x06 then
        registerLo hi
            |> Result.map (\reg -> SetRegConst reg lo_)

    else if hh == 0x07 then
        registerLo hi
            |> Result.map (\reg -> AddRegConst reg lo_)

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

    else
        Err (UnknownInstruction ( hi, lo ))
