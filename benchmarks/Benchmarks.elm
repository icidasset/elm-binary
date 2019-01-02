module Main exposing (main)

import Benchmark exposing (..)
import Benchmark.Runner
import Binary


main : Benchmark.Runner.BenchmarkProgram
main =
    Benchmark.Runner.program suite


suite : Benchmark
suite =
    describe "Binary"
        [ let
            hexString =
                String.repeat 8 "F"
          in
          describe "Converters"
            [ benchmark "fromHex" <|
                \_ -> Binary.fromHex hexString
            ]

        -----------------------------------------
        -- BITWISE OPERATORS
        -----------------------------------------
        , let
            bitsA =
                Binary.fromBooleans (List.repeat 8 False)

            bitsB =
                Binary.fromBooleans (List.repeat 8 True)
          in
          describe "Bitwise Operators"
            [ benchmark "and" <| \_ -> Binary.and bitsA bitsB
            , benchmark "or" <| \_ -> Binary.or bitsA bitsB
            , benchmark "xor" <| \_ -> Binary.xor bitsA bitsB
            , benchmark "not" <| \_ -> Binary.not bitsA
            ]

        -----------------------------------------
        -- BIT SHIFTING
        -----------------------------------------
        , let
            bits =
                Binary.fromIntegers [ 1, 0, 0, 1, 0, 1, 1, 1 ]
          in
          describe "Bit Shifting"
            [ benchmark "shiftLeftBy" <| \_ -> Binary.shiftLeftBy 5 bits
            , benchmark "shiftRightBy" <| \_ -> Binary.shiftRightBy 5 bits
            , benchmark "shiftRightZfBy" <| \_ -> Binary.shiftRightZfBy 5 bits
            , benchmark "rotateLeftBy" <| \_ -> Binary.rotateLeftBy 5 bits
            , benchmark "rotateRightBy" <| \_ -> Binary.rotateRightBy 5 bits
            ]

        -----------------------------------------
        -- MATHEMATICAL OPERATORS
        -----------------------------------------
        , let
            bitsA =
                Binary.fromBooleans (List.repeat 8 False)

            bitsB =
                Binary.fromBooleans (List.repeat 8 True)
          in
          describe "Mathematical Operators"
            [ benchmark "add" <|
                \_ -> Binary.add bitsA bitsB
            ]
        ]
