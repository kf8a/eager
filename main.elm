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
    = SwitchCO2 Point
    | SwitchCH4 Point
    | SwitchN2O Point
    | FluxGood Incubation
    | FluxMaybeGood Incubation
    | FluxBad Incubation
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
            [ { x = 10, y = 30, active = True, id = 1 }
            , { x = 20, y = 20, active = False, id = 2 }
            , { x = 30, y = 10, active = True, id = 3 }
            ]
        , n2o =
            [ { x = 10, y = 10, active = True, id = 1 }
            , { x = 15, y = 30, active = False, id = 2 }
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


update_incubation_n2o : Incubation -> Point -> Incubation
update_incubation_n2o incubation point =
    let
        new_points =
            update_point point incubation.n2o
    in
        { incubation | n2o = new_points }


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

        SwitchN2O point ->
            let
                new_incubation =
                    update_incubation_n2o model.incubation point
            in
                ({ model | incubation = new_incubation } ! [])

        NextIncubation ->
            (model ! [])

        FluxGood incubation ->
            (model ! [])

        FluxMaybeGood incubation ->
            (model ! [])

        FluxBad incubation ->
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


draw_graph : List (Svg Msg) -> Axis -> Axis -> List Point -> Svg Msg
draw_graph drawing_func x_axis y_axis points =
    svg
        [ width (toString x_axis.max_extent)
        , height (toString y_axis.max_extent)
        , viewBox (viewBox_ x_axis y_axis)
        ]
        [ g [ transform (translateCoords y_axis) ]
            drawing_func
        ]


co2_dots : Axis -> Axis -> List Point -> List (Svg Msg)
co2_dots x_axis y_axis points =
    let
        dots_transform =
            dots x_axis y_axis
    in
        List.map (\x -> dots_transform (SwitchCO2 x) x) points


ch4_dots : Axis -> Axis -> List Point -> List (Svg Msg)
ch4_dots x_axis y_axis points =
    let
        dots_transform =
            dots x_axis y_axis
    in
        List.map (\x -> dots_transform (SwitchCH4 x) x) points


n2o_dots : Axis -> Axis -> List Point -> List (Svg Msg)
n2o_dots x_axis y_axis points =
    let
        dots_transform =
            dots x_axis y_axis
    in
        List.map (\x -> dots_transform (SwitchN2O x) x) points


view : Model -> Html Msg
view model =
    let
        dots_n2o =
            n2o_dots model.x_axis model.y_axis model.incubation.n2o

        dots_co2 =
            co2_dots model.x_axis model.y_axis model.incubation.co2

        dots_ch4 =
            ch4_dots model.x_axis model.y_axis model.incubation.ch4
    in
        div []
            [ div []
                [ draw_graph dots_co2 model.x_axis model.y_axis model.incubation.co2
                , draw_graph dots_ch4 model.x_axis model.y_axis model.incubation.ch4
                , draw_graph dots_n2o model.x_axis model.y_axis model.incubation.n2o
                ]
            , button [ onClick (FluxGood model.incubation) ] [ Html.text "Good" ]
            , button [ onClick (FluxMaybeGood model.incubation) ] [ Html.text "Maybe" ]
            , button [ onClick (FluxBad model.incubation) ] [ Html.text "Bad" ]
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
