module Error exposing (Error(..), toString)

import Instruction exposing (Address(..), Instruction)
import Util


type Error
    = UnknownInstruction ( Int, Int )
    | UnimplementedInstruction Instruction
    | TriedToExecuteNonexecutableMemory Address
    | MemoryOverflow Address
    | ParsedInvalidRegister Int
    | ReturningWithEmptyCallStack


toString : Error -> String
toString err =
    case err of
        UnknownInstruction ( hi, lo ) ->
            "Tried to run an unknown instruction: " ++ Util.hexBytePair ( hi, lo )

        UnimplementedInstruction instruction ->
            "Tried to run an unimplemented instruction: " ++ Instruction.toString instruction

        TriedToExecuteNonexecutableMemory (Address addr) ->
            "Tried to execute non-executable memory at address " ++ Util.hex addr

        MemoryOverflow (Address addr) ->
            "Tried to access memory out of bounds at address " ++ Util.hex addr

        ParsedInvalidRegister reg ->
            "Parsed invalid register: " ++ String.fromInt reg

        ReturningWithEmptyCallStack ->
            "Returning with empty call stack"
