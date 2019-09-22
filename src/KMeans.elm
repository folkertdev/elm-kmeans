module KMeans exposing
    ( cluster
    , clusterBy
    , clusterExactlyBy
    )

{-| K-means is a method for partitioning data points into at most `k` clusters.

@docs cluster
@docs clusterBy
@docs clusterExactlyBy

-}

import Dict
import List.Extra


{-| Partition a list of 2d points into at most `k` clusters.
-}
cluster : Int -> List ( Float, Float ) -> List (List ( Float, Float ))
cluster k items =
    (clusterBy (\( x, y ) -> [ x, y ]) k items).clusters


{-| Partition a list of points into at most `k` clusters.

This function offers more flexibility in the type of coordinate you have: Just turn it into a list of float values, e.g.

    tuple2d : ( Float, Float ) -> List Float
    tuple2d ( x, y ) =
        [ x, y ]

    myTuples : List (Float, Float)

    clusterBy tuple2d 4 myTuples

Or using `ianmackenzie/elm-geometry` and `ianmackenzie/elm-units`

    point2d : Point2d Pixels Float -> List Float
    point2d point =
        let
            ( a, b ) =
                Point2d.toTuple Pixels.inPixels point
        in
        [ a, b ]

    myPoint2ds : List (Point2d Pixels Float)

    clusterBy point2d 4 myPoint2ds

This function also works with 1, 3 or `n` dimensions.
Additionally, you get back not only the clustered values, but also the centroid (median of all points in the cluster) of each cluster.

-}
clusterBy : (a -> List Float) -> Int -> List a -> { centroids : List (List Float), clusters : List (List a) }
clusterBy toVector k items =
    let
        wrapped =
            List.map (\v -> { vector = toVector v, value = v }) items

        result =
            kmeansHelp k wrapped
    in
    { centroids = result.centroids, clusters = List.map (List.map .value) result.clusters }


{-| Try to find a clustering with exactly `k` clusters

The K-means algorithm initially groups the data randomly into clusters. In some cases, this can cause a cluster to be "bumped out" and become empty.
Therefore, normally you get at most `k` clusters. This function will retry clustering when fewer than `k` clusters are found, by shuffling the input points.

It will retry at most `n` times (where `n` is the number of input points).

-}
clusterExactlyBy : (a -> List Float) -> Int -> List a -> { centroids : List (List Float), clusters : List (List a) }
clusterExactlyBy toVector k items =
    clusterExactlyByHelp (List.length items) toVector k items


clusterExactlyByHelp : Int -> (a -> List Float) -> Int -> List a -> { centroids : List (List Float), clusters : List (List a) }
clusterExactlyByHelp remaining toVector k items =
    if remaining <= 1 then
        clusterBy toVector k items

    else
        let
            result =
                clusterBy toVector k items
        in
        if List.length result.clusters == k then
            result

        else
            case items of
                first :: rest ->
                    clusterExactlyByHelp (remaining - 1) toVector k (rest ++ [ first ])

                [] ->
                    { centroids = [], clusters = [] }


{-| Apply K-means clustering to a set of points with a custom conversion to cartesian coordinates

    myValues : List (Float, Float)

    KMeans.tuple2d 5 myValues

Will partition `myValues` into at most 5 clusters.

-}
generic : (a -> List Float) -> Int -> List a -> List (List a)
generic toVector k points =
    points
        |> List.map (\x -> { vector = toVector x, value = x })
        |> kmeansHelp k
        |> .clusters
        |> List.map (List.map .value)


tuple2d : Int -> List ( Float, Float ) -> List (List ( Float, Float ))
tuple2d =
    generic (\( x, y ) -> [ x, y ])


tuple3d : Int -> List ( Float, Float, Float ) -> List (List ( Float, Float, Float ))
tuple3d =
    generic (\( x, y, z ) -> [ x, y, z ])


record2d : Int -> List { x : Float, y : Float } -> List (List { x : Float, y : Float })
record2d =
    generic (\{ x, y } -> [ x, y ])


record3d : Int -> List { x : Float, y : Float, z : Float } -> List (List { x : Float, y : Float, z : Float })
record3d =
    generic (\{ x, y, z } -> [ x, y, z ])


type alias Vector =
    List Float


type alias Wrapper a =
    { vector : Vector
    , value : a
    }


distanceSquared : Vector -> Vector -> Float
distanceSquared a b =
    distanceSquaredHelp a b 0


distanceSquaredHelp : Vector -> Vector -> Float -> Float
distanceSquaredHelp xs ys accum =
    case xs of
        [] ->
            accum

        x :: xss ->
            case ys of
                [] ->
                    accum

                y :: yss ->
                    let
                        delta =
                            x - y
                    in
                    distanceSquaredHelp xss yss (accum + delta * delta)


foldl2 : (a -> b -> c -> c) -> List a -> List b -> c -> c
foldl2 f xs ys accum =
    case xs of
        [] ->
            accum

        x :: xss ->
            case ys of
                [] ->
                    accum

                y :: yss ->
                    foldl2 f xss yss (f x y accum)


centroid : (a -> Vector) -> List a -> Vector
centroid toPoint items =
    let
        length =
            toFloat (List.length items)

        folder element accum =
            List.map2 (+) (toPoint element) accum
    in
    case items of
        [] ->
            []

        first :: rest ->
            let
                result =
                    List.foldl folder (toPoint first) rest
                        |> List.map (\x -> x / length)
            in
            result


closest : List Vector -> Vector -> Maybe Vector
closest points point =
    List.Extra.minimumBy (distanceSquared point) points


{-| Group items by their closest centroid
-}
reclusterHelp : List Vector -> List (Wrapper a) -> List (List (Wrapper a))
reclusterHelp centroids points =
    gatherBy (Maybe.withDefault [] << closest centroids << .vector) points


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


findFixpoint : Float -> List Vector -> List (List (Wrapper a)) -> { centroids : List Vector, clusters : List (List (Wrapper a)) }
findFixpoint previousError centroids clusters =
    let
        newCentroids =
            List.map (centroid .vector) clusters

        newClusters =
            reclusterHelp newCentroids (List.concat <| clusters)

        errorVector : Vector -> Vector -> Float
        errorVector a b =
            let
                sum =
                    foldl2 (\x y accum -> accum + (x - y)) a b 0
            in
            sum * sum

        errorCentroids : List Vector -> List Vector -> Float
        errorCentroids a b =
            let
                sum =
                    foldl2 (\x y accum -> accum + errorVector x y) a b 0
            in
            sum * sum

        error =
            errorCentroids newCentroids centroids
    in
    -- an arbitrary stopping criterion: make sure we make at least some progress every iteration.
    if (previousError - error) < 1.0e-12 || error < 1.0e-12 then
        -- clusters are ordered by centroid because they are stored in a dict
        -- so here sort the centroids to have their orders match up
        { centroids = List.sort newCentroids, clusters = newClusters }

    else
        findFixpoint error newCentroids newClusters


kmeansHelp : Int -> List (Wrapper a) -> { centroids : List Vector, clusters : List (List (Wrapper a)) }
kmeansHelp k points =
    let
        l =
            (List.length points + k - 1) // k

        groups =
            -- greedyGroupsOfHelp l points [] 0 []
            List.Extra.greedyGroupsOf l points
    in
    case points of
        [] ->
            { centroids = [], clusters = [] }

        first :: _ ->
            let
                centroidVec =
                    List.repeat (List.length first.vector) 0

                centroids =
                    List.repeat k centroidVec

                infinity =
                    1 / 0
            in
            findFixpoint infinity centroids groups


greedyGroupsOfHelp n items stack itemsOnStack accum =
    case items of
        [] ->
            if itemsOnStack > 0 then
                stack :: accum

            else
                accum

        first :: rest ->
            if itemsOnStack < n then
                greedyGroupsOfHelp n rest (first :: stack) (itemsOnStack + 1) accum

            else
                greedyGroupsOfHelp n rest [ first ] 1 (stack :: accum)

