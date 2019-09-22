module TestKMeans exposing (suite)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import KMeans
import Test exposing (..)


fuzzPoint2d : Fuzzer ( Float, Float )
fuzzPoint2d =
    Fuzz.map2 Tuple.pair (Fuzz.floatRange -1000 1000) (Fuzz.floatRange -1000 1000)


suite : Test
suite =
    describe "KMeans"
        [ describe "singletons"
            [ test "tuple2d" <|
                \_ ->
                    KMeans.tuple2d 5 [ ( 1, 2 ) ]
                        |> Expect.equal [ [ ( 1, 2 ) ] ]
            , test "tuple3d" <|
                \_ ->
                    KMeans.tuple3d 5 [ ( 1, 2, 3 ) ]
                        |> Expect.equal [ [ ( 1, 2, 3 ) ] ]
            , test "record2d" <|
                \_ ->
                    KMeans.record2d 5 [ { x = 1, y = 2 } ]
                        |> Expect.equal [ [ { x = 1, y = 2 } ] ]
            , test "record3d" <|
                \_ ->
                    KMeans.record3d 5 [ { x = 1, y = 2, z = 3 } ]
                        |> Expect.equal [ [ { x = 1, y = 2, z = 3 } ] ]
            ]
        , fuzz (Fuzz.map List.sort (Fuzz.list fuzzPoint2d)) "all points are in the output" <|
            \points ->
                KMeans.tuple2d 3 points
                    |> List.concat
                    |> List.sort
                    |> Expect.equal points
        ]
