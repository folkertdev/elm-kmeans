module KMeansBenchmark exposing (main)

import Array
import Benchmark exposing (..)
import Benchmark.Runner exposing (BenchmarkProgram, program)
import Dict
import KMeans
import Random


main : BenchmarkProgram
main =
    program suite


seed =
    Random.initialSeed 5


suite : Benchmark
suite =
    let
        generateTuple2d =
            Random.map2 Tuple.pair (Random.float 0 500) (Random.float 0 500)

        points =
            Random.step (Random.list 50 generateTuple2d) seed
                |> Tuple.first
    in
    describe "Array"
        [ -- nest as many descriptions as you like
          describe "slice"
            [ benchmark "from the end" <|
                \_ -> KMeans.tuple2d 4 points
            ]
        ]


gatherBy : (a -> comparable) -> List a -> List (List a)
gatherBy toKey items =
    let
        folder item =
            Dict.update (toKey item) (updater item)

        updater item maybeValue =
            case maybeValue of
                Nothing ->
                    Just [ item ]

                Just value ->
                    Just (item :: value)
    in
    List.foldl folder Dict.empty items
        |> Dict.values
