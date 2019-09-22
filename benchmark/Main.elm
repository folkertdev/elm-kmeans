module Main exposing (main)

import Browser
import Circle2d
import Geometry.Svg
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import KMeans
import Pixels
import Point2d
import Random
import Svg
import Svg.Attributes as Attributes



-- GENERATE POINTS
--
-- The input points for clustering are randomly generated. Because randomness is a side-effect, we need to use `Cmd`s in elm.
-- And to use `Cmd`s `Browser.document` is required which requires some setup


type alias Point2d =
    Point2d.Point2d Pixels.Pixels Float


generatePoint2d : Random.Generator Point2d
generatePoint2d =
    Random.map2 (\a b -> Point2d.fromTuple Pixels.pixels ( a, b ))
        (Random.float 0 500)
        (Random.float 0 500)


generatePoints : Cmd Msg
generatePoints =
    Random.list 100 generatePoint2d
        |> Random.generate GeneratePoints


type alias Model =
    List Point2d


initialModel : () -> ( Model, Cmd Msg )
initialModel _ =
    ( [], generatePoints )


type Msg
    = GeneratePoints (List Point2d)


update : Msg -> Model -> Model
update msg _ =
    case msg of
        GeneratePoints newPoints ->
            newPoints



-- VIEW


{-| css colors for the different clusters
-}
colors : List String
colors =
    [ "red", "green", "cyan", "lightgreen", "orange", "purple", "steelblue", "pink", "grey" ]


point2d : Point2d -> List Float
point2d point =
    let
        ( a, b ) =
            Point2d.toTuple Pixels.inPixels point
    in
    [ a, b ]


view : Model -> Html Msg
view points =
    div []
        [ Svg.svg [ Attributes.width "500", Attributes.height "500" ] <|
            let
                clustered =
                    KMeans.clusterBy point2d 8 points
            in
            viewPoints clustered ++ viewCentroids clustered
        ]


viewPoints clustered =
    let
        viewPoint opacity color point =
            Geometry.Svg.circle2d
                [ Attributes.opacity (String.fromFloat opacity)
                , Attributes.fill color
                ]
                (Circle2d.withRadius (Pixels.pixels 4) point)
    in
    List.map2 (\color coordinates -> List.map (viewPoint 0.8 color) coordinates) colors clustered.clusters
        |> List.concat


viewCentroids clustered =
    let
        viewCentroid color point =
            Geometry.Svg.circle2d
                [ Attributes.fill color
                , Attributes.stroke "black"
                , Attributes.strokeWidth "2"
                ]
                (Circle2d.withRadius (Pixels.pixels 4) point)

        toPoint2d : List Float -> Point2d
        toPoint2d centroid =
            case centroid of
                [ x, y ] ->
                    Point2d.fromTuple Pixels.pixels ( x, y )

                _ ->
                    Point2d.origin
    in
    List.map2 viewCentroid colors (List.map toPoint2d clustered.centroids)


{-| We need the full elm architecture for randomness, but otherwise don't really use any of it.
-}
main : Program () Model Msg
main =
    Browser.document
        { init = initialModel
        , view = \model -> { title = "k-means", body = [ view model ] }
        , update = \msg model -> ( update msg model, Cmd.none )
        , subscriptions = \_ -> Sub.none
        }
