module Main exposing (..)

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


type alias Model =
    { incubation : Incubation
    , x_axis : Axis
    , y_axis : Axis
    }


type alias Axis =
    { max_extent : Float
    , max_value : Float
    , min_value : Float
    }


type Msg
    = SwitchPoint Point
    | None


initialModel : Model
initialModel =
    { incubation =
        { co2 =
            [ { x = 10, y = 10, active = True, id = 1 }
            , { x = 20, y = 20, active = False, id = 2 }
            , { x = 30, y = 40, active = True, id = 3 }
            ]
        , ch4 =
            [ { x = 10, y = 10, active = True, id = 1 }
            , { x = 20, y = 20, active = False, id = 2 }
            , { x = 30, y = 40, active = True, id = 3 }
            ]
        , n2o =
            [ { x = 10, y = 10, active = True, id = 1 }
            , { x = 20, y = 20, active = False, id = 2 }
            , { x = 30, y = 40, active = True, id = 3 }
            ]
        }
    , x_axis = Axis 220 40 0
    , y_axis = Axis 120 50 0
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
    -- ((axis.max_value - axis.min_value)/(axis.max_extent - axis.min_extent) / value) * axis.max_extent
    -- (axis.max_extent/(axis.max_value - axis.min_value) / value ) * axis.max_extent
    (axis.max_extent / (axis.max_value - axis.min_value) * value)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        None ->
            (model ! [])

        SwitchPoint point ->
            let
                incubation =
                    model.incubation

                old_list =
                    List.filter (\x -> x.id /= point.id) incubation.co2

                new_point =
                    { point | active = not point.active }

                new_co2 =
                    old_list ++ [ new_point ]

                new_incubation =
                    { incubation | co2 = new_co2 }
            in
                ({ model | incubation = new_incubation } ! [])

        UpdateData ->
            (model ! [])


viewBox_ : Model -> String
viewBox_ model =
    String.concat [ "0 0 ", (toString model.x_axis.max_extent), " ", (toString model.y_axis.max_extent) ]


translateCoords : Model -> String
translateCoords model =
    String.concat [ "translate(0,", toString model.y_axis.max_extent, ") scale(1,-1)" ]


dots : Model -> Point -> Svg Msg
dots model point =
    let
        color =
            case point.active of
                True ->
                    "black"

                False ->
                    "grey"

        x_axis_transform =
            axisTransform model.x_axis

        y_axis_transform =
            axisTransform model.y_axis
    in
        circle
            [ cx (toString (x_axis_transform point.x))
            , cy (toString (y_axis_transform point.y))
            , r "5"
            , stroke color
            , fill color
            , onClick (SwitchPoint point)
            ]
            []


view : Model -> Html Msg
view model =
    let
        dots_transform =
            dots model
    in
        svg
            [ width (toString model.x_axis.max_extent), height (toString model.y_axis.max_extent), viewBox (viewBox_ model) ]
            [ g [ transform (translateCoords model) ]
                (List.map dots_transform model.incubation.co2)
            ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


init : ( Model, Cmd Msg )
init =
    ( initialModel, Cmd.none )


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
