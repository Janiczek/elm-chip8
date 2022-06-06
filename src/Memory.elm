module Memory exposing
    ( Memory
    , init
    , loadProgram
    , programStart
    , size
    , toList
    , view
    )

import Array exposing (Array)
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


displayedWidth : Int
displayedWidth =
    64


view : Int -> Memory -> List P.Shape
view pc memory =
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
