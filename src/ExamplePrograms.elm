module ExamplePrograms exposing (ProgramROM(..), all, name, program)

-- It's helpful to use `hexdump -v -e '16/1 "0x%02X, "' -e '"\n"' game.ch8` to generate these


type ProgramROM
    = Maze
    | MazeAlt
    | Trip8
    | Particle
    | Sierpinski
    | Stars
    | Zero


all : List ProgramROM
all =
    [ Maze, MazeAlt, Trip8, Particle, Sierpinski, Stars, Zero ]


name : ProgramROM -> String
name p =
    case p of
        Maze ->
            "Maze"

        MazeAlt ->
            "Maze (alt)"

        Trip8 ->
            "Trip8"

        Particle ->
            "Particle"

        Sierpinski ->
            "Sierpinski"

        Stars ->
            "Stars"

        Zero ->
            "Zero"


program : ProgramROM -> List Int
program p =
    case p of
        Maze ->
            maze

        MazeAlt ->
            mazeAlt

        Trip8 ->
            trip8

        Particle ->
            particle

        Sierpinski ->
            sierpinski

        Stars ->
            stars

        Zero ->
            zero


maze : List Int
maze =
    [ 0xA2, 0x1E, 0xC2, 0x01, 0x32, 0x01, 0xA2, 0x1A, 0xD0, 0x14, 0x70, 0x04, 0x30, 0x40, 0x12, 0x00, 0x60, 0x00, 0x71, 0x04, 0x31, 0x20, 0x12, 0x00, 0x12, 0x18, 0x80, 0x40, 0x20, 0x10, 0x20, 0x40, 0x80, 0x10 ]


mazeAlt : List Int
mazeAlt =
    [ 0x60, 0x00, 0x61, 0x00, 0xA2, 0x22, 0xC2, 0x01, 0x32, 0x01, 0xA2, 0x1E, 0xD0, 0x14, 0x70, 0x04, 0x30, 0x40, 0x12, 0x04, 0x60, 0x00, 0x71, 0x04, 0x31, 0x20, 0x12, 0x04, 0x12, 0x1C, 0x80, 0x40, 0x20, 0x10, 0x20, 0x40, 0x80, 0x10 ]


trip8 : List Int
trip8 =
    [ 0x12, 0x14, 0x52, 0x45, 0x56, 0x49, 0x56, 0x41, 0x4C, 0x53, 0x54, 0x55, 0x44, 0x49, 0x4F, 0x53, 0x32, 0x30, 0x30, 0x38, 0x00, 0xE0, 0x6D, 0x20, 0xFD, 0x15, 0x23, 0xBE, 0x23, 0xC6, 0x6D, 0x40, 0xFD, 0x15, 0x23, 0xBE, 0x23, 0xC6, 0x6D, 0x20, 0xFD, 0x15, 0x23, 0xBE, 0xA4, 0x83, 0x24, 0x48, 0x6D, 0x80, 0xFD, 0x15, 0x23, 0xBE, 0xA4, 0x83, 0x24, 0x48, 0xA5, 0x83, 0x24, 0x48, 0x6D, 0x00, 0x6B, 0x00, 0x22, 0xC6, 0x4B, 0x00, 0x22, 0xE4, 0x4B, 0x01, 0x23, 0x86, 0x4B, 0x02, 0x22, 0xEC, 0x4B, 0x03, 0x23, 0x86, 0x4B, 0x04, 0x22, 0xF4, 0x4B, 0x05, 0x23, 0x86, 0x60, 0x01, 0xF0, 0x15, 0x23, 0xBE, 0x7D, 0x01, 0x60, 0x3F, 0x8C, 0xD0, 0x8C, 0x02, 0x4C, 0x00, 0x22, 0x70, 0x12, 0x44, 0x4B, 0x00, 0x22, 0x90, 0x4B, 0x01, 0x22, 0xCC, 0x4B, 0x02, 0x22, 0xA2, 0x4B, 0x03, 0x22, 0xD4, 0x4B, 0x04, 0x22, 0xB4, 0x4B, 0x05, 0x22, 0xDC, 0x7B, 0x01, 0x4B, 0x06, 0x6B, 0x00, 0x00, 0xEE, 0x23, 0x08, 0xC9, 0x03, 0x89, 0x94, 0x89, 0x94, 0x89, 0x94, 0x89, 0x94, 0x89, 0x94, 0x23, 0x66, 0x00, 0xEE, 0x22, 0xFC, 0xC9, 0x03, 0x89, 0x94, 0x89, 0x94, 0x89, 0x94, 0x89, 0x94, 0x89, 0x94, 0x23, 0x66, 0x00, 0xEE, 0x23, 0x18, 0xC9, 0x03, 0x89, 0x94, 0x89, 0x94, 0x89, 0x94, 0x89, 0x94, 0x89, 0x94, 0x23, 0x66, 0x00, 0xEE, 0x6E, 0x00, 0x23, 0x08, 0x00, 0xEE, 0x23, 0x66, 0x6E, 0x00, 0x22, 0xFC, 0x00, 0xEE, 0x23, 0x66, 0x6E, 0x00, 0x23, 0x18, 0x00, 0xEE, 0x23, 0x66, 0x6E, 0x00, 0x23, 0x08, 0x00, 0xEE, 0x23, 0x08, 0x7E, 0x03, 0x23, 0x08, 0x00, 0xEE, 0x22, 0xFC, 0x7E, 0x02, 0x22, 0xFC, 0x00, 0xEE, 0x23, 0x18, 0x7E, 0x02, 0x23, 0x18, 0x00, 0xEE, 0x6C, 0x00, 0x23, 0x3A, 0x23, 0x3A, 0x23, 0x3A, 0x23, 0x3A, 0x00, 0xEE, 0x6C, 0x00, 0x23, 0x24, 0x23, 0x24, 0x23, 0x24, 0x23, 0x24, 0x23, 0x24, 0x23, 0x24, 0x00, 0xEE, 0x6C, 0x00, 0x23, 0x50, 0x23, 0x50, 0x23, 0x50, 0x23, 0x50, 0x00, 0xEE, 0xA6, 0x83, 0xFE, 0x1E, 0xFE, 0x1E, 0xFE, 0x1E, 0xFE, 0x1E, 0xFC, 0x1E, 0xF1, 0x65, 0xA4, 0x7C, 0xD0, 0x14, 0x7C, 0x02, 0x00, 0xEE, 0xA9, 0x83, 0xFE, 0x1E, 0xFE, 0x1E, 0xFE, 0x1E, 0xFE, 0x1E, 0xFC, 0x1E, 0xF1, 0x65, 0xA4, 0x7C, 0xD0, 0x14, 0x7C, 0x02, 0x00, 0xEE, 0xAB, 0x83, 0xFE, 0x1E, 0xFE, 0x1E, 0xFE, 0x1E, 0xFE, 0x1E, 0xFC, 0x1E, 0xF1, 0x65, 0xA4, 0x7C, 0xD0, 0x14, 0x7C, 0x02, 0x00, 0xEE, 0x6C, 0x00, 0x60, 0x1F, 0x8A, 0xD0, 0x8A, 0xC4, 0x8A, 0x02, 0x8A, 0x94, 0xAD, 0x83, 0xFA, 0x1E, 0xFA, 0x1E, 0xF1, 0x65, 0xA4, 0x80, 0xD0, 0x13, 0x7C, 0x01, 0x3C, 0x08, 0x13, 0x68, 0x00, 0xEE, 0x60, 0x1F, 0x8A, 0xD0, 0x8A, 0x02, 0x8A, 0x94, 0xAD, 0x83, 0xFA, 0x1E, 0xFA, 0x1E, 0xF1, 0x65, 0xA4, 0x80, 0xD0, 0x13, 0x60, 0x1F, 0x8A, 0xD0, 0x7A, 0x08, 0x8A, 0x02, 0x8A, 0x94, 0xAD, 0x83, 0xFA, 0x1E, 0xFA, 0x1E, 0xF1, 0x65, 0xA4, 0x80, 0xD0, 0x13, 0x00, 0xEE, 0xA6, 0x83, 0xFD, 0x1E, 0xF0, 0x65, 0x30, 0x00, 0xF0, 0x18, 0x00, 0xEE, 0xF0, 0x07, 0x30, 0x00, 0x13, 0xBE, 0x00, 0xEE, 0x6D, 0x04, 0x61, 0x0C, 0x60, 0x1C, 0x62, 0x12, 0xA4, 0x1E, 0xF2, 0x1E, 0xD0, 0x16, 0xFD, 0x15, 0x23, 0xBE, 0x60, 0x14, 0x62, 0x0C, 0xA4, 0x1E, 0xF2, 0x1E, 0xD0, 0x16, 0x60, 0x24, 0x62, 0x18, 0xA4, 0x1E, 0xF2, 0x1E, 0xD0, 0x16, 0xFD, 0x15, 0x23, 0xBE, 0x60, 0x0C, 0x62, 0x06, 0xA4, 0x1E, 0xF2, 0x1E, 0xD0, 0x16, 0x60, 0x2C, 0x62, 0x1E, 0xA4, 0x1E, 0xF2, 0x1E, 0xD0, 0x16, 0xFD, 0x15, 0x23, 0xBE, 0xA4, 0x1E, 0x60, 0x04, 0xD0, 0x16, 0x60, 0x34, 0x62, 0x24, 0xA4, 0x1E, 0xF2, 0x1E, 0xD0, 0x16, 0xFD, 0x15, 0x23, 0xBE, 0x00, 0xEE, 0x00, 0x00, 0x0C, 0x11, 0x11, 0x10, 0x00, 0x00, 0x95, 0x55, 0x95, 0xCD, 0x00, 0x00, 0x53, 0x55, 0x55, 0x33, 0x40, 0x40, 0x44, 0x42, 0x41, 0x46, 0x00, 0x40, 0x6A, 0x4A, 0x4A, 0x46, 0x00, 0x20, 0x69, 0xAA, 0xAA, 0x69, 0x00, 0x00, 0x20, 0x90, 0x88, 0x30, 0x64, 0x01, 0x65, 0x07, 0x62, 0x00, 0x63, 0x00, 0x60, 0x00, 0x81, 0x30, 0xD0, 0x11, 0x71, 0x08, 0xF4, 0x1E, 0xD0, 0x11, 0x71, 0x08, 0xF4, 0x1E, 0xD0, 0x11, 0x71, 0x08, 0xF4, 0x1E, 0xD0, 0x11, 0xF4, 0x1E, 0x70, 0x08, 0x30, 0x40, 0x14, 0x52, 0x73, 0x03, 0x83, 0x52, 0x72, 0x01, 0x32, 0x08, 0x14, 0x50, 0x00, 0xEE, 0x60, 0xB0, 0xF0, 0x60, 0x40, 0xA0, 0x40, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x06, 0x00, 0x00, 0x00, 0xC6, 0x00, 0x00, 0x00, 0xDB, 0x00, 0x00, 0x00, 0x30, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x5F, 0x06, 0x00, 0x00, 0xFE, 0xC6, 0x00, 0x00, 0xD3, 0xFB, 0x00, 0x00, 0xF0, 0xF0, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x06, 0x00, 0x00, 0x00, 0xF6, 0x00, 0x00, 0x00, 0xFB, 0xE0, 0x00, 0x00, 0xF0, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x06, 0x06, 0x00, 0x00, 0x00, 0xC6, 0x00, 0x00, 0x00, 0xDB, 0x00, 0x00, 0x00, 0x30, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x06, 0x06, 0x00, 0x00, 0x00, 0xC6, 0x00, 0x00, 0x03, 0xF1, 0x00, 0x00, 0x30, 0xE0, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x06, 0x00, 0x00, 0x00, 0xC6, 0x00, 0x00, 0x00, 0xD9, 0x00, 0x00, 0x00, 0xE0, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x2F, 0x06, 0x00, 0x00, 0xFF, 0xC6, 0x00, 0x00, 0x69, 0xDB, 0x00, 0x00, 0xE0, 0x30, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x06, 0x00, 0x00, 0x00, 0x76, 0x00, 0x00, 0x00, 0xF3, 0xE0, 0x00, 0x00, 0x30, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x1F, 0x07, 0x0F, 0x00, 0xFF, 0xFE, 0xFC, 0x7E, 0x00, 0x00, 0x3E, 0x7C, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x2F, 0x1B, 0x07, 0x00, 0xFF, 0xF0, 0xFB, 0x1F, 0x00, 0x00, 0xFE, 0xB0, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x17, 0x0F, 0x00, 0x00, 0xFF, 0xF8, 0x7E, 0x0F, 0x00, 0x0C, 0x14, 0x38, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x2F, 0x0B, 0x0F, 0x00, 0xFE, 0xE0, 0xFC, 0x3F, 0x00, 0x00, 0x7E, 0xFC, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x17, 0x1F, 0x03, 0x00, 0xFF, 0xF0, 0xFF, 0x1F, 0x80, 0x00, 0xFE, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x0B, 0x0F, 0x00, 0x00, 0xFE, 0xF8, 0x7E, 0x0F, 0x00, 0x1C, 0x3E, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x17, 0x17, 0x0F, 0x00, 0xFE, 0xC0, 0xF8, 0x3F, 0x00, 0x00, 0xFE, 0xFC, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x2B, 0x1F, 0x00, 0x00, 0xFF, 0xE0, 0x7F, 0x1F, 0x80, 0x04, 0x1C, 0x3C, 0x04, 0x05, 0x1B, 0x05, 0x1B, 0x17, 0x04, 0x17, 0x07, 0x08, 0x17, 0x08, 0x1C, 0x08, 0x15, 0x1A, 0x06, 0x03, 0x00, 0x14, 0x1B, 0x0A, 0x16, 0x16, 0x1A, 0x0A, 0x0E, 0x1A, 0x08, 0x02, 0x00, 0x10, 0x1E, 0x0C, 0x15, 0x17, 0x18, 0x0B, 0x08, 0x19, 0x0B, 0x01, 0x00, 0x0D, 0x20, 0x0F, 0x13, 0x19, 0x15, 0x0A, 0x03, 0x15, 0x20, 0x12, 0x0E, 0x01, 0x10, 0x1A, 0x01, 0x0B, 0x14, 0x08, 0x00, 0x11, 0x1F, 0x14, 0x0D, 0x1B, 0x12, 0x01, 0x04, 0x09, 0x15, 0x06, 0x00, 0x0D, 0x1D, 0x16, 0x0A, 0x1B, 0x15, 0x03, 0x06, 0x08, 0x18, 0x05, 0x01, 0x09, 0x1C, 0x16, 0x06, 0x19, 0x17, 0x05, 0x07, 0x08, 0x1B, 0x05, 0x04, 0x05, 0x1B, 0x17, 0x04, 0x17, 0x17, 0x08, 0x08, 0x08, 0x04, 0x14, 0x09, 0x02, 0x1A, 0x18, 0x1E, 0x07, 0x05, 0x13, 0x08, 0x07, 0x05, 0x12, 0x0E, 0x01, 0x19, 0x19, 0x1F, 0x0A, 0x02, 0x12, 0x08, 0x06, 0x08, 0x12, 0x14, 0x02, 0x18, 0x1A, 0x20, 0x0C, 0x00, 0x10, 0x09, 0x05, 0x0A, 0x12, 0x19, 0x04, 0x15, 0x1B, 0x00, 0x0E, 0x1F, 0x0E, 0x0B, 0x03, 0x0C, 0x14, 0x1D, 0x08, 0x00, 0x0B, 0x0E, 0x01, 0x11, 0x1B, 0x1D, 0x11, 0x0B, 0x17, 0x1F, 0x0D, 0x00, 0x08, 0x12, 0x01, 0x0D, 0x1A, 0x1B, 0x12, 0x08, 0x18, 0x1E, 0x12, 0x02, 0x07, 0x17, 0x02, 0x0A, 0x17, 0x19, 0x13, 0x1B, 0x17, 0x1B, 0x05, 0x04, 0x17, 0x04, 0x05, 0x17, 0x08, 0x17, 0x14, 0x1D, 0x0A, 0x06, 0x04, 0x15, 0x1A, 0x01, 0x15, 0x1B, 0x09, 0x0B, 0x05, 0x1B, 0x0E, 0x08, 0x03, 0x0E, 0x1B, 0x00, 0x11, 0x1E, 0x0C, 0x10, 0x04, 0x17, 0x12, 0x0A, 0x02, 0x08, 0x1A, 0x1F, 0x0F, 0x00, 0x0D, 0x16, 0x04, 0x10, 0x14, 0x0D, 0x02, 0x1E, 0x14, 0x03, 0x16, 0x1B, 0x06, 0x02, 0x09, 0x09, 0x13, 0x10, 0x01, 0x1A, 0x18, 0x1F, 0x0A, 0x01, 0x12, 0x07, 0x06, 0x04, 0x0F, 0x14, 0x02, 0x13, 0x1B, 0x20, 0x0E, 0x01, 0x0E, 0x0D, 0x05, 0x02, 0x0A, 0x18, 0x03, 0x0B, 0x1B, 0x1F, 0x13, 0x04, 0x0A, 0x12, 0x05, 0x04, 0x17, 0x04, 0x05, 0x1B, 0x17, 0x1B, 0x05, 0x08, 0x14, 0x08, 0x08, 0x15, 0x19, 0x1E, 0x08, 0x00, 0x11, 0x09, 0x02, 0x14, 0x17, 0x1B, 0x0C, 0x0E, 0x17, 0x1F, 0x0B, 0x00, 0x0B, 0x0F, 0x01, 0x10, 0x1A, 0x1C, 0x10, 0x09, 0x13, 0x1F, 0x0E, 0x02, 0x06, 0x15, 0x02, 0x0A, 0x1A, 0x1A, 0x15, 0x08, 0x0E, 0x1F, 0x0F, 0x08, 0x02, 0x05, 0x18, 0x1A, 0x05, 0x17, 0x19, 0x0A, 0x09, 0x1E, 0x12, 0x00, 0x14, 0x0E, 0x01, 0x11, 0x1B, 0x1C, 0x09, 0x0F, 0x05, 0x1D, 0x14, 0x00, 0x0E, 0x0C, 0x1B, 0x13, 0x02, 0x1D, 0x0E, 0x15, 0x04, 0x1C, 0x15, 0x00, 0x0A, 0x07, 0x1A, 0x16, 0x05, 0x1B, 0x11, 0x1B, 0x05, 0x04, 0x05, 0x04, 0x17, 0x1B, 0x17, 0x18, 0x08, 0x08, 0x08, 0x03, 0x14, 0x0A, 0x02, 0x19, 0x19, 0x1F, 0x08, 0x04, 0x12, 0x09, 0x06, 0x05, 0x12, 0x11, 0x02, 0x17, 0x1A, 0x20, 0x0C, 0x01, 0x10, 0x0A, 0x05, 0x07, 0x11, 0x17, 0x03, 0x14, 0x1B, 0x00, 0x0D, 0x20, 0x0F, 0x0C, 0x03, 0x0A, 0x12, 0x1C, 0x07, 0x00, 0x0A, 0x11, 0x1B, 0x0F, 0x02, 0x1E, 0x11, 0x0B, 0x14, 0x1F, 0x0B, 0x00, 0x08, 0x12, 0x01, 0x0D, 0x1B, 0x1B, 0x13, 0x0A, 0x16, 0x1F, 0x0F, 0x02, 0x06, 0x15, 0x01, 0x0A, 0x19, 0x19, 0x14, 0x07, 0x17, 0x1E, 0x13, 0x03, 0x06, 0x19, 0x03, 0x08, 0x17, 0x18, 0x14, 0x1B, 0x05, 0x04, 0x05, 0x1B, 0x17, 0x04, 0x17, 0x17, 0x08, 0x08, 0x08, 0x1B, 0x08, 0x16, 0x1A, 0x05, 0x04, 0x01, 0x15, 0x1A, 0x09, 0x17, 0x15, 0x1A, 0x0A, 0x11, 0x1B, 0x06, 0x03, 0x00, 0x12, 0x1D, 0x0A, 0x17, 0x16, 0x17, 0x0A, 0x0B, 0x1A, 0x07, 0x02, 0x00, 0x10, 0x1F, 0x0C, 0x16, 0x17, 0x15, 0x0A, 0x06, 0x18, 0x0A, 0x01, 0x20, 0x0E, 0x00, 0x0E, 0x14, 0x19, 0x13, 0x08, 0x02, 0x14, 0x20, 0x11, 0x0E, 0x01, 0x11, 0x1B, 0x02, 0x0B, 0x14, 0x05, 0x00, 0x0F, 0x1F, 0x14, 0x0D, 0x1B, 0x12, 0x02, 0x04, 0x0A, 0x17, 0x04, 0x01, 0x0A, 0x1D, 0x15, 0x08, 0x1A, 0x15, 0x05, 0x06, 0x09, 0x1B, 0x17, 0x1B, 0x05, 0x04, 0x17, 0x04, 0x05, 0x18, 0x08, 0x18, 0x14, 0x02, 0x12, 0x19, 0x18, 0x0A, 0x02, 0x1E, 0x07, 0x04, 0x13, 0x14, 0x17, 0x04, 0x0E, 0x17, 0x19, 0x11, 0x01, 0x20, 0x0B, 0x01, 0x10, 0x0F, 0x18, 0x08, 0x0A, 0x15, 0x1A, 0x17, 0x02, 0x20, 0x0F, 0x00, 0x0D, 0x09, 0x18, 0x0F, 0x08, 0x12, 0x1A, 0x01, 0x08, 0x1C, 0x06, 0x04, 0x16, 0x1D, 0x13, 0x16, 0x09, 0x0F, 0x1B, 0x05, 0x04, 0x00, 0x12, 0x1E, 0x0A, 0x18, 0x16, 0x1B, 0x0D, 0x0B, 0x1A, 0x0C, 0x01, 0x00, 0x0E, 0x1E, 0x0E, 0x12, 0x17, 0x1D, 0x12, 0x07, 0x19, 0x14, 0x01, 0x00, 0x09, 0x1B, 0x12, 0x0D, 0x17, 0x04, 0x17, 0x04, 0x05, 0x1B, 0x17, 0x1B, 0x05, 0x08, 0x14, 0x08, 0x08, 0x0A, 0x03, 0x01, 0x14, 0x1F, 0x0B, 0x16, 0x1A, 0x0B, 0x05, 0x04, 0x10, 0x11, 0x05, 0x00, 0x11, 0x20, 0x11, 0x10, 0x1B, 0x0F, 0x02, 0x03, 0x0C, 0x16, 0x09, 0x00, 0x0E, 0x1D, 0x16, 0x0A, 0x1A, 0x15, 0x02, 0x05, 0x07, 0x17, 0x0E, 0x00, 0x0D, 0x17, 0x1A, 0x1A, 0x04, 0x05, 0x17, 0x08, 0x03, 0x15, 0x13, 0x01, 0x0A, 0x1F, 0x08, 0x0E, 0x01, 0x11, 0x1B, 0x03, 0x13, 0x10, 0x17, 0x02, 0x08, 0x20, 0x0E, 0x13, 0x01, 0x0C, 0x1A, 0x02, 0x0E, 0x0A, 0x18, 0x03, 0x07, 0x1F, 0x12, 0x18, 0x02, 0x09, 0x17, 0x04, 0x0B, 0x04, 0x05, 0x1B, 0x05, 0x1B, 0x17, 0x04, 0x17, 0x1C, 0x08, 0x15, 0x1A, 0x06, 0x03, 0x00, 0x14, 0x1A, 0x0A, 0x0E, 0x1A, 0x08, 0x02, 0x00, 0x10, 0x18, 0x0B, 0x08, 0x19, 0x0B, 0x01, 0x00, 0x0D, 0x15, 0x0A, 0x03, 0x15, 0x0E, 0x01, 0x01, 0x0B, 0x14, 0x08, 0x00, 0x11, 0x12, 0x01, 0x14, 0x12, 0x15, 0x06, 0x00, 0x0D, 0x13, 0x14, 0x15, 0x03, 0x18, 0x05, 0x01, 0x09, 0x11, 0x15, 0x17, 0x05, 0x1B, 0x05, 0x04, 0x05, 0x10, 0x15, 0x17, 0x08, 0x09, 0x02, 0x1E, 0x07, 0x0E, 0x15, 0x08, 0x07, 0x0E, 0x01, 0x1F, 0x0A, 0x0C, 0x15, 0x08, 0x06, 0x14, 0x02, 0x0B, 0x14, 0x20, 0x0C, 0x09, 0x05, 0x19, 0x04, 0x0A, 0x14, 0x1F, 0x0E, 0x0B, 0x03, 0x1D, 0x08, 0x08, 0x13, 0x0E, 0x01, 0x1D, 0x11, 0x1F, 0x0D, 0x12, 0x01, 0x07, 0x11, 0x1B, 0x12, 0x1E, 0x12, 0x17, 0x02, 0x06, 0x10, 0x19, 0x13, 0x1B, 0x17, 0x1B, 0x05, 0x06, 0x0E, 0x17, 0x08, 0x1D, 0x0A, 0x15, 0x1A, 0x07, 0x0C, 0x1B, 0x09, 0x1B, 0x0E, 0x0E, 0x1B, 0x08, 0x0A, 0x1E, 0x0C, 0x17, 0x12, 0x08, 0x1A, 0x1F, 0x0F, 0x0B, 0x08, 0x10, 0x14, 0x1E, 0x14, 0x03, 0x16, 0x0F, 0x07, 0x09, 0x13, 0x1A, 0x18, 0x01, 0x12, 0x12, 0x08, 0x04, 0x0F, 0x13, 0x1B, 0x15, 0x09, 0x01, 0x0E, 0x02, 0x0A, 0x0B, 0x1B, 0x18, 0x0B, 0x04, 0x0A, 0x04, 0x17, 0x04, 0x05, 0x19, 0x0E, 0x08, 0x14, 0x00, 0x11, 0x09, 0x02, 0x18, 0x10, 0x06, 0x12, 0x00, 0x0B, 0x0F, 0x01, 0x16, 0x13, 0x05, 0x10, 0x02, 0x06, 0x14, 0x14, 0x15, 0x02, 0x05, 0x0E, 0x11, 0x14, 0x08, 0x02, 0x1A, 0x05, 0x05, 0x0D, 0x0E, 0x13, 0x0E, 0x01, 0x1C, 0x09, 0x06, 0x0C, 0x0D, 0x11, 0x13, 0x02, 0x1D, 0x0E, 0x06, 0x0A, 0x0E, 0x0F, 0x16, 0x05, 0x1B, 0x11, 0x07, 0x09, 0x10, 0x0E, 0x18, 0x08, 0x08, 0x08, 0x08, 0x14, 0x12, 0x0E, 0x04, 0x12, 0x09, 0x06, 0x13, 0x16, 0x14, 0x0E, 0x01, 0x10, 0x0A, 0x05, 0x0F, 0x16, 0x16, 0x10, 0x00, 0x0D, 0x0C, 0x03, 0x0A, 0x15, 0x16, 0x12, 0x00, 0x0A, 0x0F, 0x02, 0x07, 0x13, 0x00, 0x08, 0x15, 0x13, 0x12, 0x01, 0x05, 0x10, 0x02, 0x06, 0x15, 0x01, 0x13, 0x15, 0x05, 0x0E, 0x03, 0x06, 0x19, 0x03, 0x11, 0x15, 0x05, 0x0B, 0x1B, 0x05, 0x04, 0x05, 0x0F, 0x15, 0x17, 0x08, 0x1B, 0x08, 0x05, 0x04, 0x0E, 0x15, 0x1A, 0x09, 0x1A, 0x0A, 0x06, 0x03, 0x0C, 0x14, 0x1D, 0x0A, 0x17, 0x0A, 0x07, 0x02, 0x1F, 0x0C, 0x0B, 0x14, 0x15, 0x0A, 0x0A, 0x01, 0x20, 0x0E, 0x0A, 0x13, 0x13, 0x08, 0x20, 0x11, 0x0E, 0x01, 0x09, 0x12, 0x14, 0x05, 0x1F, 0x14, 0x08, 0x11, 0x12, 0x02, 0x17, 0x04, 0x1D, 0x15, 0x07, 0x10, 0x15, 0x05, 0x1B, 0x17, 0x1B, 0x05, 0x06, 0x0E, 0x18, 0x08, 0x19, 0x18, 0x1E, 0x07, 0x07, 0x0C, 0x14, 0x17, 0x17, 0x19, 0x20, 0x0B, 0x08, 0x0A, 0x0F, 0x18, 0x15, 0x1A, 0x0B, 0x08, 0x20, 0x0F, 0x09, 0x18, 0x12, 0x1A, 0x0E, 0x07, 0x04, 0x16, 0x1D, 0x13, 0x0F, 0x1B, 0x12, 0x07, 0x00, 0x12, 0x18, 0x16, 0x0B, 0x1A, 0x00, 0x0E, 0x16, 0x09, 0x12, 0x17, 0x07, 0x19, 0x00, 0x09, 0x18, 0x0B, 0x0D, 0x17, 0x04, 0x17, 0x04, 0x05, 0x19, 0x0E, 0x08, 0x14, 0x0A, 0x03, 0x01, 0x14, 0x18, 0x10, 0x0B, 0x05, 0x11, 0x05, 0x00, 0x11, 0x16, 0x12, 0x0F, 0x02, 0x16, 0x09, 0x00, 0x0E, 0x15, 0x02, 0x13, 0x13, 0x17, 0x0E, 0x00, 0x0D, 0x1A, 0x04, 0x08, 0x03, 0x15, 0x13, 0x01, 0x0A, 0x1F, 0x08, 0x0E, 0x01, 0x10, 0x17, 0x02, 0x08, 0x20, 0x0E, 0x13, 0x01, 0x0A, 0x18, 0x03, 0x07, 0x1F, 0x12, 0x18, 0x02, 0x10, 0x0E, 0x06, 0x07, 0x19, 0x07, 0x19, 0x15, 0x0D, 0x0E, 0x1B, 0x09, 0x16, 0x17, 0x09, 0x05, 0x0B, 0x0E, 0x1C, 0x0C, 0x12, 0x18, 0x0D, 0x04, 0x1C, 0x0E, 0x09, 0x0C, 0x0E, 0x19, 0x11, 0x04, 0x1B, 0x0E, 0x09, 0x0A, 0x0B, 0x18, 0x14, 0x05, 0x1A, 0x0E, 0x07, 0x16, 0x0A, 0x09, 0x14, 0x12, 0x1A, 0x0E, 0x05, 0x14, 0x0C, 0x07, 0x13, 0x14, 0x1A, 0x0E, 0x04, 0x11, 0x0E, 0x07, 0x11, 0x15, 0x1B, 0x0E, 0x04, 0x0E, 0x10, 0x15, 0x10, 0x07, 0x06, 0x0C, 0x1C, 0x0F, 0x0E, 0x15, 0x11, 0x07, 0x0A, 0x0A, 0x1D, 0x10, 0x0C, 0x15, 0x13, 0x08, 0x0E, 0x0A, 0x1C, 0x12, 0x0B, 0x14, 0x14, 0x08, 0x12, 0x0B, 0x0A, 0x14, 0x1A, 0x14, 0x05, 0x08, 0x15, 0x0E, 0x07, 0x06, 0x08, 0x13, 0x17, 0x16, 0x15, 0x11, 0x0A, 0x04, 0x07, 0x11, 0x17, 0x0B, 0x13, 0x15, 0x0D, 0x04, 0x06, 0x10, 0x18, 0x0C, 0x10, 0x17, 0x10, 0x05, 0x06, 0x0E, 0x19, 0x0E, 0x11, 0x07, 0x0B, 0x17, 0x18, 0x10, 0x07, 0x0C, 0x11, 0x09, 0x06, 0x16, 0x17, 0x12, 0x08, 0x0A, 0x10, 0x0A, 0x03, 0x13, 0x14, 0x14, 0x1A, 0x0A, 0x0E, 0x0B, 0x11, 0x15, 0x1D, 0x0D, 0x03, 0x0F, 0x0D, 0x0A, 0x1D, 0x10, 0x0D, 0x15, 0x12, 0x08, 0x0C, 0x08, 0x1A, 0x14, 0x09, 0x13, 0x15, 0x09, 0x0D, 0x06, 0x15, 0x17, 0x07, 0x11, 0x18, 0x0B, 0x10, 0x17, 0x10, 0x05, 0x19, 0x0E, 0x06, 0x0E, 0x0A, 0x15, 0x13, 0x05, 0x18, 0x10, 0x07, 0x0C, 0x06, 0x10, 0x17, 0x05, 0x16, 0x13, 0x09, 0x0A, 0x05, 0x0C, 0x1A, 0x07, 0x14, 0x14, 0x0C, 0x09, 0x08, 0x07, 0x11, 0x14, 0x1C, 0x0A, 0x05, 0x11, 0x0C, 0x04, 0x0E, 0x13, 0x1D, 0x0D, 0x04, 0x0F, 0x0D, 0x11, 0x11, 0x03, 0x1D, 0x10, 0x03, 0x0D, 0x0E, 0x0F, 0x16, 0x04, 0x1B, 0x13, 0x04, 0x09, 0x10, 0x0E, 0x19, 0x07, 0x06, 0x07, 0x06, 0x15, 0x12, 0x0E, 0x04, 0x13, 0x09, 0x05, 0x16, 0x17, 0x14, 0x0E, 0x03, 0x10, 0x0D, 0x04, 0x12, 0x18, 0x03, 0x0E, 0x16, 0x10, 0x11, 0x03, 0x0E, 0x18, 0x04, 0x0E, 0x16, 0x12, 0x14, 0x04, 0x0B, 0x17, 0x05, 0x0E, 0x18, 0x06, 0x15, 0x13, 0x0B, 0x0A, 0x05, 0x0E, 0x1A, 0x08, 0x13, 0x15, 0x0C, 0x08, 0x05, 0x0E, 0x1B, 0x0B, 0x11, 0x15, 0x0E, 0x07, 0x1B, 0x0E, 0x04, 0x0E, 0x0F, 0x15, 0x0F, 0x07, 0x19, 0x10, 0x03, 0x0D, 0x11, 0x07, 0x0E, 0x15, 0x15, 0x12, 0x02, 0x0C, 0x13, 0x07, 0x0C, 0x14, 0x11, 0x12, 0x03, 0x0A, 0x14, 0x08, 0x0B, 0x14, 0x0D, 0x11, 0x15, 0x08, 0x05, 0x08, 0x1A, 0x14, 0x0A, 0x0E, 0x18, 0x16, 0x17, 0x09, 0x08, 0x06, 0x0A, 0x0B, 0x15, 0x18, 0x18, 0x0B, 0x08, 0x11, 0x0C, 0x07, 0x12, 0x18, 0x19, 0x0C, 0x07, 0x10, 0x10, 0x17, 0x10, 0x05, 0x06, 0x0E, 0x19, 0x0E, 0x0E, 0x15, 0x14, 0x05, 0x07, 0x0C, 0x18, 0x10, 0x0E, 0x13, 0x19, 0x06, 0x08, 0x0A, 0x17, 0x12, 0x0F, 0x12, 0x1C, 0x09, 0x0B, 0x08, 0x05, 0x12, 0x11, 0x11, 0x0E, 0x07, 0x02, 0x0F, 0x1C, 0x0D, 0x12, 0x12, 0x02, 0x0C, 0x12, 0x07, 0x0D, 0x14, 0x13, 0x14, 0x05, 0x08, 0x16, 0x09, 0x0A, 0x13, 0x12, 0x16, 0x0A, 0x05, 0x18, 0x0B, 0x07, 0x11, 0x0F, 0x17, 0x10, 0x05, 0x19, 0x0E, 0x06, 0x0E, 0x15, 0x07, 0x0C, 0x17, 0x07, 0x0C, 0x18, 0x10, 0x19, 0x0C, 0x08, 0x17, 0x09, 0x09, 0x16, 0x12, 0x1A, 0x10, 0x05, 0x15, 0x0B, 0x08, 0x13, 0x13, 0x17, 0x15, 0x0E, 0x08, 0x03, 0x12, 0x1A, 0x0B, 0x13, 0x18, 0x11, 0x09, 0x02, 0x0F, 0x1B, 0x0D, 0x12, 0x0B, 0x0E, 0x19, 0x02, 0x0C, 0x1C, 0x0F, 0x11, 0x0D, 0x09, 0x18, 0x04, 0x09, 0x1B, 0x13, 0x10, 0x10, 0x0C, 0x14, 0x07, 0x17, 0x04, 0x1A, 0x03, 0x1C, 0x03, 0x1D, 0x05, 0x1D, 0x08, 0x1B, 0x0C, 0x19, 0x10, 0x16, 0x14, 0x13, 0x17, 0x10, 0x1A, 0x0D, 0x1B, 0x0A, 0x1B, 0x08, 0x19, 0x06, 0x17, 0x05, 0x13, 0x06, 0x10, 0x07, 0x0D, 0x08, 0x0A, 0x0B, 0x07, 0x0E, 0x06, 0x10, 0x06, 0x12, 0x08, 0x15, 0x0A, 0x17, 0x0D, 0x18, 0x10, 0x19, 0x13, 0x19, 0x16, 0x18, 0x18, 0x17, 0x19, 0x14, 0x10, 0x19, 0x0F, 0x17, 0x0D, 0x17, 0x0A, 0x19, 0x06, 0x1A, 0x06, 0x16, 0x09, 0x13, 0x09, 0x11, 0x07, 0x10, 0x03, 0x0E, 0x03, 0x0B, 0x08, 0x0B, 0x0B, 0x0B, 0x0C, 0x0A, 0x0C, 0x06, 0x0E, 0x02, 0x10, 0x04, 0x11, 0x08, 0x12, 0x0A, 0x14, 0x09, 0x19, 0x07, 0x1B, 0x09, 0x19, 0x0D, 0x17, 0x0F, 0x17, 0x10, 0x1A, 0x12, 0x1D, 0x15, 0x1A, 0x16, 0x15, 0x15, 0x14, 0x16, 0x13, 0x18, 0x12, 0x1D, 0x10, 0x19, 0x0E, 0x17, 0x0D, 0x17, 0x0B, 0x17, 0x08, 0x17, 0x05, 0x19, 0x02, 0x19, 0x03, 0x17, 0x08, 0x14, 0x0B, 0x12, 0x0E, 0x11, 0x10, 0x10, 0x11, 0x10, 0x14, 0x0E, 0x19, 0x0B, 0x1C, 0x09, 0x1C, 0x09, 0x19, 0x09, 0x17, 0x0A, 0x16, 0x0A, 0x14, 0x09, 0x13, 0x07, 0x11, 0x04, 0x0F, 0x02, 0x0B, 0x03, 0x09, 0x06, 0x09, 0x09, 0x09, 0x0A, 0x09, 0x0B, 0x08, 0x0B, 0x07, 0x0B, 0x07, 0x0B, 0x10, 0x19, 0x13, 0x18, 0x15, 0x17, 0x16, 0x16, 0x18, 0x15, 0x18, 0x15, 0x18, 0x14, 0x15, 0x12, 0x10, 0x10, 0x0B, 0x0E, 0x09, 0x0C, 0x08, 0x0C, 0x09, 0x0B, 0x0A, 0x0A, 0x0B, 0x09, 0x0D, 0x08, 0x10, 0x06, 0x14, 0x04, 0x1A, 0x03, 0x1D, 0x04, 0x1C, 0x08, 0x19, 0x0B, 0x15, 0x0D, 0x12, 0x0F, 0x10, 0x10, 0x0D, 0x11, 0x0A, 0x13, 0x06, 0x16, 0x03, 0x19, 0x03, 0x1C, 0x07, 0x1C, 0x0C, 0x1A ]


particle : List Int
particle =
    [ 0xA3, 0x21, 0x60, 0x00, 0x61, 0x00, 0x62, 0x08, 0xD0, 0x15, 0xF2, 0x1E, 0x80, 0x24, 0xD0, 0x15, 0xF2, 0x1E, 0x80, 0x24, 0xD0, 0x15, 0xF2, 0x1E, 0x80, 0x24, 0xD0, 0x15, 0xF2, 0x1E, 0x80, 0x24, 0xD0, 0x15, 0xF2, 0x1E, 0x80, 0x24, 0xD0, 0x15, 0xF2, 0x1E, 0x80, 0x24, 0xD0, 0x15, 0xF2, 0x1E, 0x80, 0x24, 0xD0, 0x15, 0x66, 0x05, 0x67, 0x02, 0x6A, 0x00, 0x12, 0xB8, 0x6B, 0x00, 0x6C, 0x00, 0xA2, 0xD8, 0xFB, 0x1E, 0xF3, 0x65, 0x22, 0xCE, 0x22, 0x5C, 0x12, 0x62, 0x22, 0xCE, 0x22, 0x5C, 0x7B, 0x04, 0x7C, 0x01, 0x5C, 0x60, 0x12, 0x40, 0x12, 0x3C, 0x12, 0x00, 0xA3, 0x20, 0xDE, 0xD1, 0x00, 0xEE, 0xA2, 0xD8, 0xFB, 0x1E, 0xF3, 0x65, 0x80, 0x24, 0x81, 0x34, 0x8E, 0x00, 0x8D, 0x10, 0x8E, 0xE6, 0x8D, 0xD6, 0x84, 0xE0, 0x65, 0xC2, 0x84, 0x54, 0x4F, 0x01, 0x12, 0x92, 0x4D, 0x00, 0x63, 0x01, 0x84, 0xD0, 0x65, 0xE1, 0x84, 0x54, 0x4F, 0x01, 0x12, 0x92, 0x33, 0x02, 0x73, 0x01, 0x12, 0x94, 0x22, 0x9C, 0xA2, 0xD8, 0xFB, 0x1E, 0xF3, 0x55, 0x12, 0x4C, 0xA3, 0x00, 0xFA, 0x1E, 0xF0, 0x65, 0x82, 0x00, 0x7A, 0x01, 0x64, 0x1F, 0x8A, 0x42, 0x60, 0x20, 0x61, 0x1E, 0x80, 0x0E, 0x81, 0x1E, 0xC3, 0x03, 0x73, 0xF8, 0x00, 0xEE, 0x6B, 0x00, 0x6C, 0x00, 0x22, 0x9C, 0xA2, 0xD8, 0xFB, 0x1E, 0xF3, 0x55, 0x7B, 0x04, 0x7C, 0x01, 0x5C, 0x60, 0x12, 0xBC, 0x12, 0x3C, 0x8E, 0x00, 0x8D, 0x10, 0x8E, 0xE6, 0x8D, 0xD6, 0x00, 0xEE, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xF8, 0xFA, 0xF9, 0xFE, 0xFB, 0xFC, 0xFD, 0xFF, 0x02, 0x01, 0x03, 0x05, 0x04, 0x06, 0x07, 0x08, 0x06, 0x07, 0x04, 0x05, 0x03, 0x01, 0x02, 0xFE, 0xFF, 0xFC, 0xFB, 0xFD, 0xFA, 0xF9, 0xF8, 0xFA, 0x80, 0xF7, 0x06, 0x77, 0x06, 0x36, 0x00, 0x00, 0x00, 0xC7, 0x6C, 0xCF, 0x0C, 0x0C, 0x00, 0x00, 0x00, 0x9F, 0xD9, 0xDF, 0xD9, 0xD9, 0x00, 0x00, 0x00, 0x3F, 0x8C, 0x0C, 0x8C, 0x8C, 0x00, 0x00, 0x00, 0x67, 0x6C, 0x6C, 0x6C, 0x67, 0x00, 0x00, 0x00, 0xB0, 0x30, 0x30, 0x30, 0xBE, 0x00, 0x00, 0x00, 0xF9, 0xC3, 0xF1, 0xC0, 0xFB, 0x00, 0x00, 0x00, 0xEF, 0x00, 0xCE, 0x60, 0xCC, 0x00, 0x00, 0x00 ]


sierpinski : List Int
sierpinski =
    [ 0x12, 0x05, 0x43, 0x38, 0x50, 0x60, 0x00, 0x85, 0x00, 0x60, 0x01, 0x81, 0x50, 0xA3, 0xE6, 0xF1, 0x1E, 0xF0, 0x55, 0x60, 0x1F, 0x8A, 0x00, 0x60, 0x00, 0x8B, 0x00, 0xA3, 0xC2, 0xF0, 0x65, 0xA3, 0xC2, 0xDA, 0xB1, 0x60, 0x01, 0xA3, 0xC3, 0xF0, 0x55, 0x60, 0x1F, 0xA4, 0x06, 0xF0, 0x55, 0x60, 0x01, 0xA3, 0xC4, 0xF0, 0x55, 0xA3, 0xC3, 0xF0, 0x65, 0x85, 0x00, 0x60, 0x01, 0x81, 0x00, 0x80, 0x50, 0x80, 0x14, 0xA4, 0x07, 0xF0, 0x55, 0xA3, 0xC4, 0xF0, 0x65, 0x85, 0x00, 0x60, 0x01, 0x81, 0x00, 0x80, 0x50, 0x80, 0x15, 0xA3, 0xC5, 0xF0, 0x55, 0xA3, 0xC4, 0xF0, 0x65, 0x85, 0x00, 0xA3, 0xC5, 0xF0, 0x65, 0xA3, 0xE6, 0xF0, 0x1E, 0xF0, 0x65, 0x86, 0x00, 0xA3, 0xC4, 0xF0, 0x65, 0x87, 0x00, 0x60, 0x01, 0x81, 0x00, 0x80, 0x70, 0x80, 0x14, 0xA3, 0xE6, 0xF0, 0x1E, 0xF0, 0x65, 0x81, 0x00, 0x80, 0x60, 0x80, 0x13, 0x81, 0x50, 0xA3, 0xC6, 0xF1, 0x1E, 0xF0, 0x55, 0xA3, 0xC5, 0xF0, 0x65, 0x85, 0x00, 0xA3, 0xC5, 0xF0, 0x65, 0xA3, 0xC6, 0xF0, 0x1E, 0xF0, 0x65, 0x81, 0x50, 0xA3, 0xE6, 0xF1, 0x1E, 0xF0, 0x55, 0xA3, 0xC4, 0xF0, 0x65, 0xA3, 0xC6, 0xF0, 0x1E, 0xF0, 0x65, 0x85, 0x00, 0x60, 0x01, 0x81, 0x50, 0x50, 0x10, 0x6F, 0x01, 0x3F, 0x00, 0x12, 0xF9, 0xA3, 0xC4, 0xF0, 0x65, 0x85, 0x00, 0x60, 0x1F, 0x81, 0x00, 0x80, 0x50, 0x80, 0x14, 0x8A, 0x00, 0xA3, 0xC3, 0xF0, 0x65, 0x8B, 0x00, 0xA3, 0xC2, 0xF0, 0x65, 0xA3, 0xC2, 0xDA, 0xB1, 0x60, 0x1F, 0x85, 0x00, 0xA3, 0xC4, 0xF0, 0x65, 0x81, 0x00, 0x80, 0x50, 0x80, 0x15, 0x8A, 0x00, 0xA3, 0xC3, 0xF0, 0x65, 0x8B, 0x00, 0xA3, 0xC2, 0xF0, 0x65, 0xA3, 0xC2, 0xDA, 0xB1, 0xA3, 0xC4, 0xF0, 0x65, 0x85, 0x00, 0xA4, 0x07, 0xF0, 0x65, 0x81, 0x00, 0x80, 0x50, 0x82, 0x10, 0x81, 0x05, 0x81, 0x20, 0x90, 0x10, 0x6F, 0x00, 0x3F, 0x01, 0x13, 0x21, 0xA3, 0xC4, 0xF0, 0x65, 0x70, 0x01, 0xA3, 0xC4, 0xF0, 0x55, 0x12, 0x47, 0xA3, 0xC3, 0xF0, 0x65, 0x85, 0x00, 0xA4, 0x06, 0xF0, 0x65, 0x81, 0x00, 0x80, 0x50, 0x82, 0x10, 0x81, 0x05, 0x81, 0x20, 0x90, 0x10, 0x6F, 0x00, 0x3F, 0x01, 0x13, 0x49, 0xA3, 0xC3, 0xF0, 0x65, 0x70, 0x01, 0xA3, 0xC3, 0xF0, 0x55, 0x12, 0x2F, 0x13, 0x49, 0x81, 0x00, 0xA4, 0x08, 0x62, 0x01, 0x8E, 0x25, 0xFE, 0x1E, 0xF0, 0x65, 0x00, 0xEE, 0x62, 0x01, 0x63, 0x00, 0x83, 0x04, 0x81, 0x25, 0x31, 0x00, 0x13, 0x5D, 0x80, 0x30, 0x00, 0xEE, 0xA4, 0x08, 0xFE, 0x1E, 0xF6, 0x55, 0x66, 0x00, 0x82, 0x00, 0x82, 0x15, 0x3F, 0x01, 0x13, 0x95, 0x83, 0x00, 0x83, 0x06, 0x84, 0x10, 0x65, 0x01, 0x82, 0x30, 0x82, 0x45, 0x3F, 0x01, 0x13, 0x8F, 0x84, 0x0E, 0x85, 0x0E, 0x13, 0x81, 0x80, 0x45, 0x86, 0x54, 0x13, 0x71, 0xF5, 0x65, 0x80, 0x60, 0x00, 0xEE, 0x82, 0x00, 0x80, 0x15, 0x3F, 0x00, 0x13, 0x9B, 0x80, 0x20, 0x00, 0xEE, 0xA3, 0xBF, 0xF0, 0x33, 0xF2, 0x65, 0xF0, 0x29, 0xD3, 0x45, 0x73, 0x06, 0xF1, 0x29, 0xD3, 0x45, 0x73, 0x06, 0xF2, 0x29, 0xD3, 0x45, 0x00, 0xEE, 0x28, 0x63, 0x29, 0x80, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00 ]


stars : List Int
stars =
    [ 0x12, 0x05, 0x43, 0x38, 0x50, 0x60, 0x00, 0x85, 0x00, 0xC0, 0x38, 0x81, 0x50, 0xA5, 0xB0, 0xF1, 0x1E, 0xF0, 0x55, 0x60, 0x00, 0x85, 0x00, 0xC0, 0x18, 0x81, 0x50, 0xA5, 0xB8, 0xF1, 0x1E, 0xF0, 0x55, 0x60, 0x00, 0xA5, 0xB0, 0xF0, 0x1E, 0xF0, 0x65, 0x8A, 0x00, 0x60, 0x00, 0xA5, 0xB8, 0xF0, 0x1E, 0xF0, 0x65, 0x8B, 0x00, 0x60, 0x00, 0xA5, 0x80, 0xF0, 0x1E, 0xDA, 0xB8, 0x60, 0x01, 0xA5, 0xC2, 0xF0, 0x55, 0x60, 0x07, 0xA5, 0xC4, 0xF0, 0x55, 0xA5, 0xC2, 0xF0, 0x65, 0x85, 0x00, 0xC0, 0x38, 0x81, 0x50, 0xA5, 0xB0, 0xF1, 0x1E, 0xF0, 0x55, 0x60, 0x00, 0xA5, 0xC1, 0xF0, 0x55, 0xA5, 0xC2, 0xF0, 0x65, 0x85, 0x00, 0x60, 0x01, 0x81, 0x00, 0x80, 0x50, 0x80, 0x15, 0xA5, 0xC6, 0xF0, 0x55, 0xA5, 0xC2, 0xF0, 0x65, 0xA5, 0xB0, 0xF0, 0x1E, 0xF0, 0x65, 0x85, 0x00, 0xA5, 0xC1, 0xF0, 0x65, 0xA5, 0xB0, 0xF0, 0x1E, 0xF0, 0x65, 0x86, 0x00, 0x60, 0x08, 0x81, 0x00, 0x80, 0x60, 0x80, 0x14, 0x81, 0x00, 0x80, 0x50, 0x82, 0x00, 0x80, 0x15, 0x80, 0x20, 0x3F, 0x00, 0x12, 0xDB, 0xA5, 0xC2, 0xF0, 0x65, 0xA5, 0xB0, 0xF0, 0x1E, 0xF0, 0x65, 0x85, 0x00, 0xA5, 0xC1, 0xF0, 0x65, 0xA5, 0xB0, 0xF0, 0x1E, 0xF0, 0x65, 0x86, 0x00, 0x60, 0x08, 0x81, 0x00, 0x80, 0x60, 0x80, 0x15, 0x81, 0x00, 0x80, 0x50, 0x82, 0x10, 0x81, 0x05, 0x81, 0x20, 0x3F, 0x00, 0x12, 0xD5, 0x60, 0x01, 0xA5, 0xC3, 0xF0, 0x55, 0x12, 0xDB, 0x60, 0x00, 0xA5, 0xC3, 0xF0, 0x55, 0xA5, 0xC1, 0xF0, 0x65, 0x85, 0x00, 0xA5, 0xC6, 0xF0, 0x65, 0x81, 0x00, 0x80, 0x50, 0x82, 0x10, 0x81, 0x05, 0x81, 0x20, 0x90, 0x10, 0x6F, 0x00, 0x3F, 0x01, 0x13, 0x01, 0xA5, 0xC1, 0xF0, 0x65, 0x70, 0x01, 0xF0, 0x55, 0x12, 0x71, 0xA5, 0xC3, 0xF0, 0x65, 0x85, 0x00, 0x60, 0x01, 0x81, 0x50, 0x50, 0x10, 0x6F, 0x01, 0x3F, 0x00, 0x13, 0x23, 0xA5, 0xC2, 0xF0, 0x65, 0x85, 0x00, 0xC0, 0x78, 0x81, 0x50, 0xA5, 0xB0, 0xF1, 0x1E, 0xF0, 0x55, 0xA5, 0xC3, 0xF0, 0x65, 0x85, 0x00, 0x60, 0x00, 0x81, 0x50, 0x50, 0x10, 0x6F, 0x01, 0x90, 0x10, 0x6F, 0x00, 0x3F, 0x00, 0x12, 0x59, 0xA5, 0xC2, 0xF0, 0x65, 0x85, 0x00, 0xC0, 0x18, 0x81, 0x50, 0xA5, 0xB8, 0xF1, 0x1E, 0xF0, 0x55, 0x60, 0x00, 0xA5, 0xC1, 0xF0, 0x55, 0xA5, 0xC2, 0xF0, 0x65, 0x85, 0x00, 0x60, 0x01, 0x81, 0x00, 0x80, 0x50, 0x80, 0x15, 0xA5, 0xC6, 0xF0, 0x55, 0xA5, 0xC2, 0xF0, 0x65, 0xA5, 0xB8, 0xF0, 0x1E, 0xF0, 0x65, 0x85, 0x00, 0xA5, 0xC1, 0xF0, 0x65, 0xA5, 0xB8, 0xF0, 0x1E, 0xF0, 0x65, 0x86, 0x00, 0x60, 0x08, 0x81, 0x00, 0x80, 0x60, 0x80, 0x14, 0x81, 0x00, 0x80, 0x50, 0x82, 0x00, 0x80, 0x15, 0x80, 0x20, 0x3F, 0x00, 0x13, 0xCB, 0xA5, 0xC2, 0xF0, 0x65, 0xA5, 0xB8, 0xF0, 0x1E, 0xF0, 0x65, 0x85, 0x00, 0xA5, 0xC1, 0xF0, 0x65, 0xA5, 0xB8, 0xF0, 0x1E, 0xF0, 0x65, 0x86, 0x00, 0x60, 0x08, 0x81, 0x00, 0x80, 0x60, 0x80, 0x15, 0x81, 0x00, 0x80, 0x50, 0x82, 0x10, 0x81, 0x05, 0x81, 0x20, 0x3F, 0x00, 0x13, 0xC5, 0x60, 0x01, 0xA5, 0xC3, 0xF0, 0x55, 0x13, 0xCB, 0x60, 0x00, 0xA5, 0xC3, 0xF0, 0x55, 0xA5, 0xC1, 0xF0, 0x65, 0x85, 0x00, 0xA5, 0xC6, 0xF0, 0x65, 0x81, 0x00, 0x80, 0x50, 0x82, 0x10, 0x81, 0x05, 0x81, 0x20, 0x90, 0x10, 0x6F, 0x00, 0x3F, 0x01, 0x13, 0xF1, 0xA5, 0xC1, 0xF0, 0x65, 0x70, 0x01, 0xF0, 0x55, 0x13, 0x61, 0xA5, 0xC3, 0xF0, 0x65, 0x85, 0x00, 0x60, 0x01, 0x81, 0x50, 0x50, 0x10, 0x6F, 0x01, 0x3F, 0x00, 0x14, 0x13, 0xA5, 0xC2, 0xF0, 0x65, 0x85, 0x00, 0xC0, 0x18, 0x81, 0x50, 0xA5, 0xB8, 0xF1, 0x1E, 0xF0, 0x55, 0xA5, 0xC3, 0xF0, 0x65, 0x85, 0x00, 0x60, 0x00, 0x81, 0x50, 0x50, 0x10, 0x6F, 0x01, 0x90, 0x10, 0x6F, 0x00, 0x3F, 0x00, 0x13, 0x49, 0xA5, 0xC2, 0xF0, 0x65, 0xA5, 0xB0, 0xF0, 0x1E, 0xF0, 0x65, 0x8A, 0x00, 0xA5, 0xC2, 0xF0, 0x65, 0xA5, 0xB8, 0xF0, 0x1E, 0xF0, 0x65, 0x8B, 0x00, 0x60, 0x00, 0xA5, 0x80, 0xF0, 0x1E, 0xDA, 0xB8, 0xA5, 0xC2, 0xF0, 0x65, 0x85, 0x00, 0xA5, 0xC4, 0xF0, 0x65, 0x81, 0x00, 0x80, 0x50, 0x82, 0x10, 0x81, 0x05, 0x81, 0x20, 0x90, 0x10, 0x6F, 0x00, 0x3F, 0x01, 0x14, 0x6F, 0xA5, 0xC2, 0xF0, 0x65, 0x70, 0x01, 0xF0, 0x55, 0x12, 0x49, 0xC0, 0x07, 0xA5, 0xC0, 0xF0, 0x55, 0xA5, 0xC0, 0xF0, 0x65, 0xA5, 0xB0, 0xF0, 0x1E, 0xF0, 0x65, 0x8A, 0x00, 0xA5, 0xC0, 0xF0, 0x65, 0xA5, 0xB8, 0xF0, 0x1E, 0xF0, 0x65, 0x8B, 0x00, 0xA5, 0xC0, 0xF0, 0x65, 0xA5, 0xA8, 0xF0, 0x1E, 0xF0, 0x65, 0xA5, 0x80, 0xF0, 0x1E, 0xF0, 0x65, 0xDA, 0xB8, 0x60, 0x0A, 0xF0, 0x15, 0xA5, 0xC0, 0xF0, 0x65, 0x85, 0x00, 0xA5, 0xC0, 0xF0, 0x65, 0xA5, 0xA8, 0xF0, 0x1E, 0xF0, 0x65, 0x86, 0x00, 0x60, 0x08, 0x81, 0x00, 0x80, 0x60, 0x80, 0x14, 0x81, 0x50, 0xA5, 0xA8, 0xF1, 0x1E, 0xF0, 0x55, 0xA5, 0xC0, 0xF0, 0x65, 0xA5, 0xA8, 0xF0, 0x1E, 0xF0, 0x65, 0x85, 0x00, 0x60, 0x20, 0x81, 0x00, 0x80, 0x50, 0x82, 0x10, 0x81, 0x05, 0x81, 0x20, 0x3F, 0x00, 0x14, 0xF1, 0xA5, 0xC0, 0xF0, 0x65, 0x85, 0x00, 0x60, 0x00, 0x81, 0x50, 0xA5, 0xA8, 0xF1, 0x1E, 0xF0, 0x55, 0xA5, 0xC0, 0xF0, 0x65, 0x85, 0x00, 0x60, 0x32, 0x81, 0x50, 0x50, 0x10, 0x6F, 0x01, 0x90, 0x10, 0x6F, 0x00, 0x3F, 0x00, 0x14, 0x6F, 0x15, 0x07, 0x81, 0x00, 0xA5, 0xC7, 0x62, 0x01, 0x8E, 0x25, 0xFE, 0x1E, 0xF0, 0x65, 0x00, 0xEE, 0x62, 0x01, 0x63, 0x00, 0x83, 0x04, 0x81, 0x25, 0x31, 0x00, 0x15, 0x1B, 0x80, 0x30, 0x00, 0xEE, 0xA5, 0xC7, 0xFE, 0x1E, 0xF6, 0x55, 0x66, 0x00, 0x82, 0x00, 0x82, 0x15, 0x3F, 0x01, 0x15, 0x53, 0x83, 0x00, 0x83, 0x06, 0x84, 0x10, 0x65, 0x01, 0x82, 0x30, 0x82, 0x45, 0x3F, 0x01, 0x15, 0x4D, 0x84, 0x0E, 0x85, 0x0E, 0x15, 0x3F, 0x80, 0x45, 0x86, 0x54, 0x15, 0x2F, 0xF5, 0x65, 0x80, 0x60, 0x00, 0xEE, 0x82, 0x00, 0x80, 0x15, 0x3F, 0x00, 0x15, 0x59, 0x80, 0x20, 0x00, 0xEE, 0xA5, 0x7D, 0xF0, 0x33, 0xF2, 0x65, 0xF0, 0x29, 0xD3, 0x45, 0x73, 0x06, 0xF1, 0x29, 0xD3, 0x45, 0x73, 0x06, 0xF2, 0x29, 0xD3, 0x45, 0x00, 0xEE, 0x28, 0x63, 0x29, 0x00, 0x00, 0x00, 0x10, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x38, 0x28, 0x38, 0x00, 0x00, 0x00, 0x00, 0x54, 0x00, 0x44, 0x00, 0x54, 0x00, 0x00, 0x92, 0x00, 0x00, 0x82, 0x00, 0x00, 0x92, 0x00, 0x92, 0x54, 0x38, 0xFE, 0x38, 0x54, 0x92, 0x00, 0x08, 0x08, 0x08, 0x08, 0x08, 0x08, 0x08, 0x08, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00 ]


zero : List Int
zero =
    [ 0x60, 0x0A, 0x65, 0x05, 0x66, 0x0A, 0x67, 0x0F, 0x68, 0x14, 0x61, 0x01, 0x62, 0x01, 0x63, 0x01, 0x64, 0x01, 0x60, 0x0A, 0xA2, 0x78, 0xD0, 0x56, 0x70, 0x0A, 0xA2, 0x7E, 0xD0, 0x66, 0x70, 0x0A, 0xA2, 0x84, 0xD0, 0x76, 0x70, 0x0A, 0xA2, 0x8A, 0xD0, 0x86, 0x6A, 0x03, 0xFA, 0x15, 0x60, 0x0A, 0xA2, 0x78, 0xD0, 0x56, 0x45, 0x14, 0x61, 0xFF, 0x45, 0x01, 0x61, 0x01, 0x85, 0x14, 0xD0, 0x56, 0x70, 0x0A, 0xA2, 0x7E, 0xD0, 0x66, 0x46, 0x14, 0x62, 0xFF, 0x46, 0x01, 0x62, 0x01, 0x86, 0x24, 0xD0, 0x66, 0x70, 0x0A, 0xA2, 0x84, 0xD0, 0x76, 0x47, 0x14, 0x63, 0xFF, 0x47, 0x01, 0x63, 0x01, 0x87, 0x34, 0xD0, 0x76, 0x70, 0x0A, 0xA2, 0x8A, 0xD0, 0x86, 0x48, 0x14, 0x64, 0xFF, 0x48, 0x01, 0x64, 0x01, 0x88, 0x44, 0xD0, 0x86, 0x12, 0x2A, 0xFF, 0x03, 0x0C, 0x30, 0xC0, 0xFF, 0xFF, 0xC0, 0xC0, 0xFC, 0xC0, 0xFF, 0xF0, 0xCC, 0xCC, 0xF0, 0xCC, 0xC3, 0x3C, 0xC3, 0xC3, 0xC3, 0xC3, 0x3C ]
