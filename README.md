# K-means 

![kmeans clustering example](https://github.com/folkertdev/elm-kmeans/raw/master/kmeans.png)

[K-means clustering](https://en.wikipedia.org/wiki/K-means_clustering) in elm. This algorithm partitions data into clusters, and is often used in data analysis and machine learning. 

## Example

```elm
import Iris exposing (Iris)
import KMeans
import Length
import Svg
import Svg.Attributes as Attributes

irisToList : Iris -> List Float
irisToList { sepal, petal } =
    [ Length.inCentimeters sepal.length
    , Length.inCentimeters sepal.width
    , Length.inCentimeters petal.length
    , Length.inCentimeters petal.width
    ]


view : List Iris -> Html Msg
view points =
    let
        clustered =
            KMeans.clusterExactlyBy irisToList 3 points
    in
    div []
        [ Svg.svg 
            [ Attributes.width "600"
            , Attributes.height "600" 
            ] 
            viewPoints clustered.clusters
        ]
```
[Full example](link)
