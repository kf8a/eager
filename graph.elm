module Graph exposing (..)

import Html
import Html exposing (program, Html)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events exposing (..)
import List.Extra as LE


type alias Point =
    { x : Float
    , y : Float
    , active : Bool
    , id : Int
    }


type alias Incubation =
    { co2 : List Point
    , ch4 : List Point
    , n2o : List Point
    }


type alias Axis =
    { max_extent : Float
    , max_value : Float
    , min_value : Float
    }


max_y : List Point -> Float
max_y points =
    case LE.maximumBy .y points of
        Just point ->
            point.y

        Nothing ->
            120


max_x : List Point -> Float
max_x points =
    case LE.maximumBy .x points of
        Just point ->
            point.x

        Nothing ->
            120


axisTransform : Axis -> Float -> Float
axisTransform axis value =
    (axis.max_extent / (axis.max_value - axis.min_value) * value)


pointColor : Bool -> String
pointColor active =
    case active of
        True ->
            "black"

        False ->
            "grey"


translateCoords : Axis -> String
translateCoords axis =
    String.concat [ "translate(0,", toString axis.max_extent, ") scale(1,-1)" ]
