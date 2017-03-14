module Main exposing (..)

import Graph exposing (..)
import Date exposing (..)
import Date.Extra as DE exposing (..)
import Time exposing (..)
import Json.Decode as JD exposing (..)
import Json.Decode.Pipeline exposing (decode, required, optional, hardcoded)
import Html
import Html exposing (..)
import Html exposing (program, Html)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events exposing (..)
import SampleIncubation exposing (json, nextJson)


type Status
    = Good
    | Bad
    | MaybeGood
    | NotChecked


type alias Model =
    { incubation : Incubation
    , next_incubation : Incubation
    , x_axis : Axis
    , y_axis_co2 : Axis
    , y_axis_ch4 : Axis
    , y_axis_n2o : Axis
    , status : Status
    }


type Msg
    = SwitchCO2 Point
    | SwitchCH4 Point
    | SwitchN2O Point
    | FluxGood Incubation
    | FluxMaybeGood Incubation
    | FluxBad Incubation
    | NextIncubation


initialModel : Model
initialModel =
    { incubation = toIncubation (decodeInjections SampleIncubation.json)
    , next_incubation = toIncubation (decodeInjections SampleIncubation.nextJson)
    , x_axis = Axis 220 100 0
    , y_axis_co2 = Axis 120 200 0
    , y_axis_ch4 = Axis 120 3 0
    , y_axis_n2o = Axis 120 1 0
    , status = NotChecked
    }


type alias Injection =
    { co2_ppm : Float
    , n2o_ppm : Float
    , ch4_ppm : Float
    , id : Int
    , deleted : Bool
    , datetime : Date
    }


sortedRecords : Injection -> Injection -> Order
sortedRecords a b =
    DE.compare a.datetime b.datetime


initialTime : List Injection -> Date
initialTime injections =
    let
        sorted =
            List.sortWith sortedRecords injections

        firstRecord =
            List.head sorted
    in
        case firstRecord of
            Just injection ->
                injection.datetime

            Nothing ->
                Date.fromTime (Time.inSeconds 0)


toIncubation : List Injection -> Incubation
toIncubation injections =
    let
        interval time =
            ((Date.toTime time) - Date.toTime (initialTime injections)) / 1000 / 60

        co2s =
            List.map
                (\x ->
                    Point (interval x.datetime) x.co2_ppm x.deleted x.id
                )
                injections

        ch4s =
            List.map
                (\x ->
                    Point (interval x.datetime) x.ch4_ppm True x.id
                )
                injections

        n2os =
            List.map
                (\x ->
                    Point (interval x.datetime) x.n2o_ppm True x.id
                )
                injections
    in
        Incubation co2s ch4s n2os



-- Decoders


date : Decoder Date
date =
    let
        convert : String -> Decoder Date
        convert raw =
            case Date.fromString raw of
                Ok date ->
                    succeed date

                Err msg ->
                    fail msg
    in
        JD.string |> JD.andThen convert


incubationDecoder : Decoder Injection
incubationDecoder =
    decode Injection
        |> required "co2_ppm" float
        |> required "n2o_ppm" float
        |> required "ch4_ppm" float
        |> required "id" int
        |> hardcoded False
        |> required "sampled_at" date


responseDecoder : Decoder (List Injection)
responseDecoder =
    decode identity
        |> required "data" (list incubationDecoder)


decodeInjections : String -> List Injection
decodeInjections json =
    case decodeString responseDecoder json of
        Ok listOfInjections ->
            listOfInjections

        Err msg ->
            []


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


swapIncubation : Model -> Model
swapIncubation model =
    let
        inc =
            model.next_incubation
    in
        { model
            | next_incubation = model.incubation
            , incubation = inc
        }


url : String
url =
    "http://localhost:3000/incubation"


fetchNextIncubation : Cmd Msg
fetchNextIncubation =
    Cmd.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        _ =
            Debug.log "model" model
    in
        case msg of
            SwitchCO2 point ->
                let
                    new_incubation =
                        update_incubation_co2 model.incubation point
                in
                    ( { model | incubation = new_incubation }, Cmd.none )

            SwitchCH4 point ->
                let
                    new_incubation =
                        update_incubation_ch4 model.incubation point
                in
                    ( { model | incubation = new_incubation }, Cmd.none )

            SwitchN2O point ->
                let
                    new_incubation =
                        update_incubation_n2o model.incubation point
                in
                    ( { model | incubation = new_incubation }, Cmd.none )

            NextIncubation ->
                let
                    _ =
                        Debug.log "Model" model
                in
                    ( model, Cmd.none )

            FluxGood incubation ->
                let
                    new_model =
                        swapIncubation model
                in
                    ( { new_model | status = Good }, fetchNextIncubation )

            FluxMaybeGood incubation ->
                let
                    new_model =
                        swapIncubation model
                in
                    ( { new_model | status = MaybeGood }, fetchNextIncubation )

            FluxBad incubation ->
                let
                    new_model =
                        swapIncubation model
                in
                    ( { new_model | status = Bad }, fetchNextIncubation )



-- VIEW


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
            n2o_dots model.x_axis model.y_axis_n2o model.incubation.n2o

        dots_co2 =
            co2_dots model.x_axis model.y_axis_co2 model.incubation.co2

        dots_ch4 =
            ch4_dots model.x_axis model.y_axis_ch4 model.incubation.ch4
    in
        div []
            [ div []
                [ draw_graph dots_co2 model.x_axis model.y_axis_co2 model.incubation.co2
                , draw_graph dots_ch4 model.x_axis model.y_axis_ch4 model.incubation.ch4
                , draw_graph dots_n2o model.x_axis model.y_axis_n2o model.incubation.n2o
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
