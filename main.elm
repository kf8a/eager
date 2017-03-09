module Main exposing (..)

import Graph exposing (..)
import Html
import Html exposing (..)
import Html exposing (program, Html)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events exposing (..)
import List.Extra as LE


type alias Model =
    { incubation : Incubation
    , x_axis : Axis
    , y_axis : Axis
    }


type Msg
    = SwitchPoint Point
    | SwitchCO2 Point
    | SwitchCH4 Point
    | NextIncubation
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


update_point : Point -> List Point -> List Point
update_point point incubation =
    let
        old_list =
            List.filter (\x -> x.id /= point.id) incubation

        new_point =
            { point | active = not point.active }
    in
        old_list ++ [ new_point ]


update_incubation_co2 : Incubation -> Point -> Incubation
update_incubation_co2 incubation point =
    let
        new_points =
            update_point point incubation.co2
    in
        { incubation | co2 = new_points }


update_incubation_ch4 : Incubation -> Point -> Incubation
update_incubation_ch4 incubation point =
    let
        new_points =
            update_point point incubation.ch4
    in
        { incubation | ch4 = new_points }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        None ->
            (model ! [])

        SwitchCO2 point ->
            let
                new_incubation =
                    update_incubation_co2 model.incubation point
            in
                ({ model | incubation = new_incubation } ! [])

        SwitchCH4 point ->
            let
                new_incubation =
                    update_incubation_ch4 model.incubation point
            in
                ({ model | incubation = new_incubation } ! [])

        SwitchPoint point ->
            let
                incubation =
                    model.incubation

                old_list =
                    List.filter (\x -> x.id /= point.id) incubation.ch4

                new_point =
                    { point | active = not point.active }

                new_co2 =
                    old_list ++ [ new_point ]

                new_incubation =
                    { incubation | co2 = new_co2 }
            in
                ({ model | incubation = new_incubation } ! [])

        NextIncubation ->
            (model ! [])


viewBox_ : Axis -> Axis -> String
viewBox_ x_axis y_axis =
    String.concat [ "0 0 ", (toString x_axis.max_extent), " ", (toString y_axis.max_extent) ]


dots : Axis -> Axis -> Msg -> Point -> Svg Msg
dots x_axis y_axis msg point =
    let
        color =
            pointColor point.active

        x_axis_transform =
            axisTransform x_axis

        y_axis_transform =
            axisTransform y_axis
    in
        circle
            [ cx (toString (x_axis_transform point.x))
            , cy (toString (y_axis_transform point.y))
            , r "5"
            , stroke color
            , fill color
            , onClick msg
            ]
            []


draw_graph_co2 : Axis -> Axis -> List Point -> Svg Msg
draw_graph_co2 x_axis y_axis points =
    let
        dots_transform =
            dots x_axis y_axis
    in
        svg
            [ width (toString x_axis.max_extent)
            , height (toString y_axis.max_extent)
            , viewBox (viewBox_ x_axis y_axis)
            ]
            [ g [ transform (translateCoords y_axis) ]
                (List.map (\x -> dots_transform (SwitchCO2 x) x) points)
            ]


draw_graph_ch4 : Axis -> Axis -> List Point -> Svg Msg
draw_graph_ch4 x_axis y_axis points =
    let
        dots_transform =
            dots x_axis y_axis
    in
        svg
            [ width (toString x_axis.max_extent)
            , height (toString y_axis.max_extent)
            , viewBox (viewBox_ x_axis y_axis)
            ]
            [ g [ transform (translateCoords y_axis) ]
                (List.map (\x -> dots_transform (SwitchCH4 x) x) points)
            ]


view : Model -> Html Msg
view model =
    div []
        [ draw_graph_co2 model.x_axis model.y_axis model.incubation.co2
        , draw_graph_ch4 model.x_axis model.y_axis model.incubation.ch4
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
