module Registers exposing
    ( Register(..)
    , Registers
    , all
    , get
    , init
    , map
    , name
    , register
    , set
    )


type Registers
    = Registers
        { v0 : Int
        , v1 : Int
        , v2 : Int
        , v3 : Int
        , v4 : Int
        , v5 : Int
        , v6 : Int
        , v7 : Int
        , v8 : Int
        , v9 : Int
        , vA : Int
        , vB : Int
        , vC : Int
        , vD : Int
        , vE : Int
        , vF : Int
        }


init : Registers
init =
    Registers
        { v0 = 0
        , v1 = 0
        , v2 = 0
        , v3 = 0
        , v4 = 0
        , v5 = 0
        , v6 = 0
        , v7 = 0
        , v8 = 0
        , v9 = 0
        , vA = 0
        , vB = 0
        , vC = 0
        , vD = 0
        , vE = 0
        , vF = 0
        }


type Register
    = V0
    | V1
    | V2
    | V3
    | V4
    | V5
    | V6
    | V7
    | V8
    | V9
    | VA
    | VB
    | VC
    | VD
    | VE
    | VF


all : List Register
all =
    [ V0, V1, V2, V3, V4, V5, V6, V7, V8, V9, VA, VB, VC, VD, VE, VF ]


set : Register -> Int -> Registers -> Registers
set reg value (Registers registers) =
    case reg of
        V0 ->
            Registers { registers | v0 = value }

        V1 ->
            Registers { registers | v1 = value }

        V2 ->
            Registers { registers | v2 = value }

        V3 ->
            Registers { registers | v3 = value }

        V4 ->
            Registers { registers | v4 = value }

        V5 ->
            Registers { registers | v5 = value }

        V6 ->
            Registers { registers | v6 = value }

        V7 ->
            Registers { registers | v7 = value }

        V8 ->
            Registers { registers | v8 = value }

        V9 ->
            Registers { registers | v9 = value }

        VA ->
            Registers { registers | vA = value }

        VB ->
            Registers { registers | vB = value }

        VC ->
            Registers { registers | vC = value }

        VD ->
            Registers { registers | vD = value }

        VE ->
            Registers { registers | vE = value }

        VF ->
            Registers { registers | vF = value }


get : Register -> Registers -> Int
get reg (Registers registers) =
    case reg of
        V0 ->
            registers.v0

        V1 ->
            registers.v1

        V2 ->
            registers.v2

        V3 ->
            registers.v3

        V4 ->
            registers.v4

        V5 ->
            registers.v5

        V6 ->
            registers.v6

        V7 ->
            registers.v7

        V8 ->
            registers.v8

        V9 ->
            registers.v9

        VA ->
            registers.vA

        VB ->
            registers.vB

        VC ->
            registers.vC

        VD ->
            registers.vD

        VE ->
            registers.vE

        VF ->
            registers.vF


register : Int -> Maybe Register
register n =
    case n of
        0 ->
            Just V0

        1 ->
            Just V1

        2 ->
            Just V2

        3 ->
            Just V3

        4 ->
            Just V4

        5 ->
            Just V5

        6 ->
            Just V6

        7 ->
            Just V7

        8 ->
            Just V8

        9 ->
            Just V9

        10 ->
            Just VA

        11 ->
            Just VB

        12 ->
            Just VC

        13 ->
            Just VD

        14 ->
            Just VE

        15 ->
            Just VF

        _ ->
            Nothing


map : Register -> (Int -> Int) -> Registers -> Registers
map reg fn registers =
    let
        value : Int
        value =
            get reg registers

        newValue : Int
        newValue =
            fn value
    in
    set reg newValue registers


name : Register -> String
name reg =
    case reg of
        V0 ->
            "V0"

        V1 ->
            "V1"

        V2 ->
            "V2"

        V3 ->
            "V3"

        V4 ->
            "V4"

        V5 ->
            "V5"

        V6 ->
            "V6"

        V7 ->
            "V7"

        V8 ->
            "V8"

        V9 ->
            "V9"

        VA ->
            "VA"

        VB ->
            "VB"

        VC ->
            "VC"

        VD ->
            "VD"

        VE ->
            "VE"

        VF ->
            "VF"
