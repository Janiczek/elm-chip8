module Memory exposing (Memory, init, loadProgram, programStart, size, toList)

import Array exposing (Array)
import List.Extra as List


type alias Memory =
    Array Int


size : Int
size =
    0x1000


programStart : Int
programStart =
    0x0200


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
