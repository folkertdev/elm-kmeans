module Main exposing (main)

import Browser
import Circle2d
import Geometry.Svg
import Html exposing (Html, div)
import Iris exposing (Iris)
import KMeans
import Length
import Pixels
import Point2d
import Random
import Random.List
import Svg
import Svg.Attributes as Attributes



-- VIEW


irisToList : Iris -> List Float
irisToList { sepal, petal } =
    [ Length.inCentimeters sepal.length
    , Length.inCentimeters sepal.width
    , Length.inCentimeters petal.length
    , Length.inCentimeters petal.width
    ]


view : List Iris -> Html Msg
view points =
    div []
        [ Svg.svg [ Attributes.width "600", Attributes.height "600" ] <|
            let
                clustered =
                    -- we want exactly 3 clusters here, so use
                    -- `clusterExactlyBy` to guarantee that
                    KMeans.clusterExactlyBy irisToList 3 points
            in
            viewPoints clustered.clusters
        ]


{-| css colors for the different clusters
-}
colors : List String
colors =
    [ "red", "green", "purple" ]


{-| Convert an iris to a point2d

The choice of attributes here is arbitrary. This just is one that
clearly shows one of the clusters to be linearly seperable.

-}
irisToPoint : Iris -> Point2d.Point2d Pixels.Pixels a
irisToPoint iris =
    Point2d.pixels
        (Length.inCentimeters iris.petal.length)
        (Length.inCentimeters iris.sepal.width)


{-| Scale up the points so they are nicely visible
-}
scale : Point2d.Point2d unit number -> Point2d.Point2d unit number
scale =
    Point2d.scaleAbout Point2d.origin 80


viewPoints : List (List Iris) -> List (Svg.Svg msg)
viewPoints clusters =
    let
        viewPoint opacity color iris =
            Geometry.Svg.circle2d
                [ Attributes.opacity (String.fromFloat opacity)
                , Attributes.fill color
                ]
                (Circle2d.withRadius (Pixels.pixels 4) (scale (irisToPoint iris)))
    in
    List.map2 (\color coordinates -> List.map (viewPoint 0.8 color) coordinates) colors clusters
        |> List.concat



-- SHUFFLE POINTS
--
-- K-means is sensitive to the initial cluster positions.
-- shuffling the data usually gives better starting positions
--
-- In the case of iris the data is sorted in such a way that the initial centroid
-- would be perfect, which is not desirable for an example.


shuffleData : Cmd Msg
shuffleData =
    Random.generate GeneratePoints (Random.List.shuffle Iris.data)



-- TEA BOILERPLATE


type alias Model =
    List Iris


init : () -> ( Model, Cmd Msg )
init _ =
    ( [], shuffleData )


type Msg
    = GeneratePoints (List Iris)


update : Msg -> Model -> Model
update (GeneratePoints newPoints) _ =
    newPoints


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = \model -> { title = "k-means", body = [ view model ] }
        , update = \msg model -> ( update msg model, Cmd.none )
        , subscriptions = \_ -> Sub.none
        }
