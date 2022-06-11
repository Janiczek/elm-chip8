module Memory exposing
    ( Memory
    , clearDisplay
    , get
    , getDisplayActivePixels
    , getInstruction
    , init
    , loadProgram
    , programStart
    , set
    , view
    , xorDisplayBit
    )

import Array exposing (Array)
import Bitwise
import Error exposing (Error(..))
import Html exposing (Html)
import Html.Attributes as Attrs
import Instruction exposing (Address(..), Byte(..), Instruction)
import Instruction.Parser
import List.Extra as List
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


init : Memory
init =
    let
        allZeroes : Array Int
        allZeroes =
            Array.repeat size 0

        withFonts : Array Int
        withFonts =
            fontData
                |> List.concat
                |> List.indexedFoldl Array.set allZeroes
    in
    Memory withFonts


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
set ((Address rawAddr) as addr) value memory =
    if isWritable rawAddr then
        setUnsafe addr value memory

    else
        Err (MemoryOverflow addr)


setUnsafe : Address -> Int -> Memory -> Result Error Memory
setUnsafe (Address rawAddr) value (Memory memory) =
    memory
        |> Array.set rawAddr value
        |> Memory
        |> Ok


isExecutable : Int -> Bool
isExecutable addr =
    addr >= programStart && (addr + 1) <= programEnd


isWritable : Int -> Bool
isWritable addr =
    -- same thing
    isExecutable addr


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

        addr : Address
        addr =
            Address (displayStart + bytePosition)
    in
    get addr memory
        |> Result.andThen
            (\(Byte byte) ->
                let
                    bitIndexInsideByte : Int
                    bitIndexInsideByte =
                        bitPosition |> modBy 8

                    currentBit : Int
                    currentBit =
                        bit bitIndexInsideByte byte

                    resultBit : Int
                    resultBit =
                        Bitwise.xor currentBit xorBit

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
                setUnsafe addr resultByte memory
                    |> Result.map
                        (\newMemory ->
                            let
                                hadCollision : Bool
                                hadCollision =
                                    currentBit == 1 && resultBit == 0
                            in
                            ( newMemory, { hadCollision = hadCollision } )
                        )
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


displayedHeight : Int
displayedHeight =
    size // displayedWidth


view : { pc : Address, i : Address } -> Memory -> Html msg
view { pc, i } memory =
    let
        (Address pc_) =
            pc

        (Address i_) =
            i
    in
    Html.div []
        [ Html.h2 [] [ Html.text "Memory" ]
        , Html.div
            [ Attrs.style "position" "relative"
            , Attrs.style "width" (Util.px displayedWidth)
            , Attrs.style "height" (Util.px displayedHeight)
            ]
            (memory
                |> toList
                |> List.indexedMap
                    (\index byte ->
                        let
                            x : Int
                            x =
                                index |> modBy displayedWidth

                            y : Int
                            y =
                                index // displayedWidth

                            color : String
                            color =
                                if index == pc_ then
                                    "red"

                                else if index == i_ then
                                    "yellow"

                                else
                                    let
                                        b : String
                                        b =
                                            String.fromInt byte
                                    in
                                    "rgb(" ++ b ++ "," ++ b ++ "," ++ b ++ ")"
                        in
                        Util.viewPixel color x y
                    )
            )
        ]


clearDisplay : Memory -> Memory
clearDisplay (Memory memory) =
    Memory
        (Array.indexedMap
            (\i val ->
                if i >= displayStart then
                    0

                else
                    val
            )
            memory
        )


{-| going 0..F
-}
fontData : List (List Int)
fontData =
    [ [ 0xF0, 0x90, 0x90, 0x90, 0xF0 ]
    , [ 0x20, 0x60, 0x20, 0x20, 0x70 ]
    , [ 0xF0, 0x10, 0xF0, 0x80, 0xF0 ]
    , [ 0xF0, 0x10, 0xF0, 0x10, 0xF0 ]
    , [ 0x90, 0x90, 0xF0, 0x10, 0x10 ]
    , [ 0xF0, 0x80, 0xF0, 0x10, 0xF0 ]
    , [ 0xF0, 0x80, 0xF0, 0x90, 0xF0 ]
    , [ 0xF0, 0x10, 0x20, 0x40, 0x40 ]
    , [ 0xF0, 0x90, 0xF0, 0x90, 0xF0 ]
    , [ 0xF0, 0x90, 0xF0, 0x10, 0xF0 ]
    , [ 0xF0, 0x90, 0xF0, 0x90, 0x90 ]
    , [ 0xE0, 0x90, 0xE0, 0x90, 0xE0 ]
    , [ 0xF0, 0x80, 0x80, 0x80, 0xF0 ]
    , [ 0xE0, 0x90, 0x90, 0x90, 0xE0 ]
    , [ 0xF0, 0x80, 0xF0, 0x80, 0xF0 ]
    , [ 0xF0, 0x80, 0xF0, 0x80, 0x80 ]
    ]
