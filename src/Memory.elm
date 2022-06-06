module Memory exposing
    ( Memory
    , getInstruction
    , init
    , loadProgram
    , programStart
    , size
    , toList
    , view
    )

import Array exposing (Array)
import Error exposing (Error(..))
import Instruction exposing (Address(..), Byte(..), Instruction)
import Instruction.Parser
import List.Extra as List
import Playground as P
import Util


type alias Memory =
    Array Int


size : Int
size =
    0x1000


programStart : Int
programStart =
    0x0200


programEnd : Int
programEnd =
    0x0E9F


init : Memory
init =
    Array.repeat size 0


toList : Memory -> List Int
toList memory =
    Array.toList memory


loadProgram : List Int -> Memory -> Memory
loadProgram program memory =
    List.indexedFoldl
        (\i byte accMem -> Array.set (programStart + i) byte accMem)
        memory
        program


get : Address -> Memory -> Result Error Byte
get ((Address rawAddr) as addr) memory =
    case Array.get rawAddr memory of
        Nothing ->
            Err (MemoryOverflow addr)

        Just byte ->
            Ok (Byte byte)


isExecutable : Int -> Bool
isExecutable addr =
    addr >= programStart && (addr + 1) <= programEnd


getInstruction : Address -> Memory -> Result Error Instruction
getInstruction ((Address rawAddr) as addr) memory =
    -- instructions are 2 bytes long
    if isExecutable rawAddr then
        Result.map2 Tuple.pair
            (get addr memory)
            (get (Address (rawAddr + 1)) memory)
            |> Result.andThen Instruction.Parser.parse

    else
        Err (TriedToExecuteNonexecutableMemory addr)



-- VIEW


displayedWidth : Int
displayedWidth =
    64


view : Address -> Memory -> List P.Shape
view (Address pc) memory =
    memory
        |> toList
        |> List.indexedMap
            (\i byte ->
                let
                    x : Int
                    x =
                        i |> modBy displayedWidth

                    y : Int
                    y =
                        i // displayedWidth

                    b : Float
                    b =
                        toFloat byte

                    color : P.Color
                    color =
                        if pc == i then
                            P.red

                        else
                            P.rgb b b b
                in
                Util.viewPixel color x y
                    |> P.move (Util.px (Util.screenWidth + 8)) (Util.px 32)
            )
