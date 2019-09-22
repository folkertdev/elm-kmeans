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
                    KMeans.clusterExactlyBy irisToList 3 points
            in
            viewPoints clustered.clusters
        ]


{-| css colors for the different clusters
-}
colors : List String
colors =
    [ "red", "green", "purple" ]


irisToPoint : Iris -> Point2d.Point2d Pixels.Pixels a
irisToPoint iris =
    Point2d.pixels
        (Length.inCentimeters iris.petal.length)
        (Length.inCentimeters iris.sepal.width)


viewPoints : List (List Iris) -> List (Svg.Svg msg)
viewPoints clusters =
    let
        viewPoint opacity color iris =
            Geometry.Svg.circle2d
                [ Attributes.opacity (String.fromFloat opacity)
                , Attributes.fill color
                ]
                (Circle2d.withRadius (Pixels.pixels 4) (irisToPoint iris |> Point2d.scaleAbout Point2d.origin 80))
    in
    List.map2 (\color coordinates -> List.map (viewPoint 0.8 color) coordinates) colors clusters
        |> List.concat



-- GENERATE POINTS
--
-- The input points for clustering are randomly generated. Because randomness is a side-effect, we need to use `Cmd`s in elm.
-- And to use `Cmd`s `Browser.document` is required which requires some setup


shuffleData : Cmd Msg
shuffleData =
    Random.generate GeneratePoints (Random.List.shuffle Iris.data)


type alias Model =
    List Iris


initialModel : () -> ( Model, Cmd Msg )
initialModel _ =
    ( [], shuffleData )


type Msg
    = GeneratePoints (List Iris)


update : Msg -> Model -> Model
update (GeneratePoints newPoints) _ =
    newPoints


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
