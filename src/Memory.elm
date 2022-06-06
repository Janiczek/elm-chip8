module Memory exposing
    ( Memory
    , displayStart
    , get
    , getDisplayActivePixels
    , getInstruction
    , init
    , loadProgram
    , programStart
    , size
    , toList
    , view
    , xorDisplayBit
    )

import Array exposing (Array)
import Bitwise
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


set : Address -> Int -> Memory -> Result Error Memory
set ((Address rawAddr) as addr) value (Memory memory) =
    memory
        |> Array.set rawAddr value
        |> Memory
        |> Ok


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


xorDisplayBit : ( Int, Int ) -> Int -> Memory -> Result Error ( Memory, { hadCollision : Bool } )
xorDisplayBit ( x, y ) xorBit memory =
    let
        bitPosition : Int
        bitPosition =
            x + Util.screenWidth * y

        bytePosition : Int
        bytePosition =
            bitPosition // 8

        bitIndexInsideByte : Int
        bitIndexInsideByte =
            bitPosition |> modBy 8

        addr : Address
        addr =
            Address (displayStart + bytePosition)
    in
    get addr memory
        |> Result.andThen
            (\(Byte byte) ->
                let
                    currentBit : Int
                    currentBit =
                        bit bitIndexInsideByte byte

                    resultBit : Int
                    resultBit =
                        Bitwise.xor currentBit xorBit

                    hadCollision : Bool
                    hadCollision =
                        currentBit == 1 && resultBit == 0

                    mask : Int
                    mask =
                        Bitwise.shiftLeftBy (7 - bitIndexInsideByte) 1

                    resultByte : Int
                    resultByte =
                        -- https://graphics.stanford.edu/~seander/bithacks.html#MaskedMerge
                        -- Merge bits from two values according to a mask
                        -- Normal: (a & ~mask) | (b & mask)
                        -- Optimized: a ^ ((a ^ b) & mask)
                        Bitwise.or
                            (Bitwise.and byte (Bitwise.complement mask))
                            (Bitwise.shiftLeftBy (7 - bitIndexInsideByte) resultBit)
                in
                set addr resultByte memory
                    |> Result.map (\newMemory -> ( newMemory, { hadCollision = hadCollision } ))
            )


bit : Int -> Int -> Int
bit bitIndex byte =
    byte
        |> Bitwise.shiftRightZfBy (7 - bitIndex)
        |> Bitwise.and 0x01


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
                        Util.toBitList byte
                in
                bits
                    |> List.indexedMap Tuple.pair
                    |> List.filterMap
                        (\( i, bit_ ) ->
                            if bit_ == 1 then
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
