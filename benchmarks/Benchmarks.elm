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
        [ describe "Converters"
            [ benchmark "fromHex" <|
                \_ -> Binary.fromHex (String.repeat 512 "F")
            ]
        ]
