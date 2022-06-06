module Error exposing (Error(..), toString)

import Util


type Error
    = UnknownInstruction ( Int, Int )
    | TriedToExecuteNonexecutableMemory { addr : Int }
    | MemoryOverflow { addr : Int }


toString : Error -> String
toString err =
    case err of
        UnknownInstruction ( hi, lo ) ->
            "Tried to run an unknown instruction: " ++ Util.hexBytePair ( hi, lo )

        TriedToExecuteNonexecutableMemory { addr } ->
            "Tried to execute non-executable memory at address " ++ Util.hex addr

        MemoryOverflow { addr } ->
            "Tried to access memory out of bounds at address " ++ Util.hex addr
