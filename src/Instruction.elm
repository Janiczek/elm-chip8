module Instruction exposing (Instruction(..), fromBytePair)

import Error exposing (Error(..))


type Instruction
    = TodoInstruction


fromBytePair : ( Int, Int ) -> Result Error Instruction
fromBytePair ( hi, lo ) =
    Err (UnknownInstruction ( hi, lo ))
