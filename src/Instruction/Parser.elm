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
            Bitwise.and 0x0F byte
    in
    Registers.register reg
        |> Result.fromMaybe (ParsedInvalidRegister reg)


parse : ( Byte, Byte ) -> Result Error Instruction
parse (( Byte hi, (Byte lo) as lo_ ) as pair) =
    let
        hh : Int
        hh =
            hi
                |> Bitwise.and 0xF0
                |> Bitwise.shiftRightZfBy 4
    in
    if hh == 0x03 then
        registerLo hi
            |> Result.map (\reg -> DoIfNeq reg lo_)

    else if hh == 0x0A then
        Ok (SetI (address pair))

    else if hh == 0x0C then
        registerLo hi
            |> Result.map (\reg -> SetRandomAnd reg lo_)

    else
        Err (UnknownInstruction ( hi, lo ))
