module KMeans exposing
    ( cluster
    , clusterBy
    , clusterExactlyBy
    , associate
    )

{-| K-means is a method for partitioning data points into `k` clusters.

The standard method only guarantees at most `k` clusters: sometimes there are fewer.
In many cases an exact `k`-clustering is desired and this usecase is also supported.


# Cluster

@docs cluster
@docs clusterBy
@docs clusterExactlyBy


# Helpers

@docs associate


# Shuffling

K-means is sensitive to the initial guess of the centroids.
If two centroid points are too close, one of them often becomes empty during the clustering process, and we don't have `k` clusters any more.
This is especially likely when the input data is sorted.

The `clusterExactlyBy` function tries to solve this issue by trying `n` permutations of the list (it moves items from the front to the back, then tries clustering to see if `k` clusters emerge).
Another method that helps is shuffling the input list. On average, the initial clusters will be distributed more evenly.

In elm shuffling a list is easiest with the `elm-community/random-extra` package, that exposes a `Random.List.shuffle` function.

    import KMeans
    import Random
    import Random.List

    shuffleAndClusterBy :
        (a -> List Float)
        -> Int
        -> List a
        -> Random.Generator { centroids : List (List Float), clusters : List (List a) }
    shuffleAndClusterBy toVector k items =
        Random.List.shuffle items
            |> Random.map (KMeans.clusterBy toVector k)

The [guide](https://guide.elm-lang.org/effects/random.html) explains how to work with randomness and `Random.Generator`.

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

    point2d : Point2d Pixels coordinates -> List Float
    point2d point =
        let
            { x, y } =
                Point2d.toPixels point
        in
        [ x, y ]

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
Therefore, normally you get at most, but not always exactly, `k` clusters.

This function will retry clustering when fewer than `k` clusters are found, by moving data points from the front to the back of the input.
It will retry at most `n` times (where `n` is the number of input points).

For big sorted inputs an initial full random shuffle can be helpful to decrease computation time. See the advice on shuffling below.

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



-- GENERATORS


{-| Associate a centroid with its points.
-}
associate : { centroids : List (List Float), clusters : List (List a) } -> List { centroid : List Float, points : List a }
associate items =
    List.map2 (\center points -> { centroid = center, points = points }) items.centroids items.clusters


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
