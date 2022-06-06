module Memory exposing
    ( Memory
    , displayStart
    , get
    , getDisplayActivePixels
    , getDisplayByte
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
import RadixInt
import Set exposing (Set)
import Util


type Memory
    = Memory (Array Int)


size : Int
size =
    0x1000


programStart : Int
programStart =
    0x0200


programEnd : Int
programEnd =
    0x0E9F


displayStart : Int
displayStart =
    0x0F00


displaySizeBytes : Int
displaySizeBytes =
    0x0100


init : Memory
init =
    Memory (Array.repeat size 0)


toList : Memory -> List Int
toList (Memory memory) =
    Array.toList memory


loadProgram : List Int -> Memory -> Memory
loadProgram program (Memory memory) =
    List.indexedFoldl
        (\i byte accMem -> Array.set (programStart + i) byte accMem)
        memory
        program
        |> Memory


get : Address -> Memory -> Result Error Byte
get ((Address rawAddr) as addr) (Memory memory) =
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


getDisplayByte : ( Int, Int ) -> Memory -> Result Error Byte
getDisplayByte ( x, y ) memory =
    let
        addr : Address
        addr =
            Address (displayStart + x + Util.screenWidth * y)
    in
    get addr memory


getDisplayActivePixels : Memory -> List ( Int, Int )
getDisplayActivePixels memory =
    toList memory
        |> List.drop displayStart
        |> List.indexedMap Tuple.pair
        |> List.concatMap
            (\( byteIndex, byte ) ->
                let
                    pxStart : Int
                    pxStart =
                        byteIndex * 8

                    bits : List Int
                    bits =
                        byte
                            |> RadixInt.fromInt (RadixInt.Base 2)
                            |> RadixInt.toList
                            |> List.reverse
                            |> zeroPadLeft 8
                in
                bits
                    |> List.indexedMap Tuple.pair
                    |> List.filterMap
                        (\( i, bit ) ->
                            if bit == 1 then
                                let
                                    position : Int
                                    position =
                                        pxStart + i
                                in
                                Just
                                    ( position |> modBy Util.screenWidth
                                    , position // Util.screenWidth
                                    )

                            else
                                Nothing
                        )
            )


zeroPadLeft : Int -> List Int -> List Int
zeroPadLeft length list =
    let
        toAdd : Int
        toAdd =
            max 0 (length - List.length list)
    in
    List.repeat toAdd 0 ++ list



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
