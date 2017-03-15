module Main exposing (..)

--import Graph exposing (..)

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
import List.Extra as LE


type alias Point =
    { x : Float
    , y : Float
    , deleted : Bool
    , id : Int
    }


type alias Flux =
    { points : List Point
    , xAxis : Axis
    , yAxis : Axis
    , slope : Float
    , intercept : Float
    }


type alias Incubation =
    { co2 : Flux
    , ch4 : Flux
    , n2o : Flux
    , injections : List Injection
    }



-- type alias Incubation =
--     { co2 : List Point
--     , ch4 : List Point
--     , n2o : List Point
--     , injections : List Injection
--     }


type alias Axis =
    { max_extent : Float
    , min_value : Float
    , max_value : Float
    }


type alias Model =
    { incubation : Incubation
    , next_incubation : Incubation
    , status : Status
    }


type alias Injection =
    { co2_ppm : Float
    , n2o_ppm : Float
    , ch4_ppm : Float
    , id : Int
    , deleted : Bool
    , datetime : Date
    }


type Status
    = Good
    | Bad
    | MaybeGood
    | NotChecked


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
    , status = NotChecked
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


interval : Date -> Date -> Float
interval startTime time =
    ((Date.toTime time) - Date.toTime (startTime)) / 1000 / 60


updateInjection : List Injection -> Int -> Bool -> List Injection
updateInjection injections id active =
    let
        ( incubation, rest ) =
            List.partition (\x -> x.id == id) injections

        newIncubation =
            case List.head incubation of
                Just incubation ->
                    [ { incubation | deleted = active } ]

                Nothing ->
                    []
    in
        List.concat [ rest, newIncubation ]



-- toInjection : List Incubation -> List Incubation
-- toInjection incubation =
--     List.map (\x -> updateInjection incubation.injections x.id x.active) incubation.co2
-- toFlux : List Injection  -> Flux
-- toFlux : injections =


toIncubation : List Injection -> Incubation
toIncubation injections =
    let
        startTime =
            initialTime injections

        pointInterval =
            interval startTime

        xAxis =
            Axis 200 0 200

        yAxis =
            Axis 120 0

        co2Points =
            List.map
                (\x ->
                    Point (pointInterval x.datetime) x.co2_ppm x.deleted x.id
                )
                injections

        co2s =
            Flux co2Points xAxis (yAxis (maxY co2Points)) 0 0

        ch4Points =
            List.map
                (\x ->
                    Point (pointInterval x.datetime) x.ch4_ppm x.deleted x.id
                )
                injections

        ch4s =
            Flux ch4Points xAxis (yAxis (maxY ch4Points)) 0 0

        n2oPoints =
            List.map
                (\x ->
                    Point (pointInterval x.datetime) x.n2o_ppm x.deleted x.id
                )
                injections

        n2os =
            Flux n2oPoints xAxis (yAxis (maxY n2oPoints)) 0 0
    in
        Incubation co2s ch4s n2os injections



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



--- VIEW


maxY : List Point -> Float
maxY points =
    case LE.maximumBy .y points of
        Just point ->
            point.y

        Nothing ->
            120


maxX : List Point -> Float
maxX points =
    case LE.maximumBy .x points of
        Just point ->
            point.x

        Nothing ->
            120


axisTransform : Axis -> Float -> Float
axisTransform axis value =
    (axis.max_extent / (axis.max_value - axis.min_value) * value)


pointColor : Bool -> String
pointColor deleted =
    case deleted of
        True ->
            "grey"

        False ->
            "blue"


translateCoords : Axis -> String
translateCoords axis =
    String.concat [ "translate(0,", toString axis.max_extent, ") scale(1,-1)" ]


viewBox_ : Axis -> Axis -> String
viewBox_ x_axis y_axis =
    String.concat [ "0 0 ", (toString x_axis.max_extent), " ", (toString y_axis.max_extent) ]


update_point : Point -> List Point -> List Point
update_point point incubation =
    let
        old_list =
            List.filter (\x -> x.id /= point.id) incubation

        new_point =
            { point | deleted = not point.deleted }
    in
        old_list ++ [ new_point ]


update_incubation_co2 : Incubation -> Point -> Incubation
update_incubation_co2 incubation point =
    let
        co2 =
            incubation.co2

        newPoints =
            update_point point co2.points

        newCO2 =
            { co2 | points = newPoints }
    in
        { incubation | co2 = newCO2 }


update_incubation_ch4 : Incubation -> Point -> Incubation
update_incubation_ch4 incubation point =
    let
        ch4 =
            incubation.ch4

        newPoints =
            update_point point ch4.points

        newCH4 =
            { ch4 | points = newPoints }
    in
        { incubation | ch4 = newCH4 }


update_incubation_n2o : Incubation -> Point -> Incubation
update_incubation_n2o incubation point =
    let
        n2o =
            incubation.n2o

        newPoints =
            update_point point n2o.points

        newN2O =
            { n2o | points = newPoints }
    in
        { incubation | n2o = newN2O }


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
            pointColor point.deleted

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


draw_graph : List (Svg Msg) -> Axis -> Axis -> Svg Msg
draw_graph drawing_func x_axis y_axis =
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
        n2o =
            model.incubation.n2o

        co2 =
            model.incubation.co2

        ch4 =
            model.incubation.ch4

        dots_n2o =
            n2o_dots n2o.xAxis n2o.yAxis n2o.points

        dots_co2 =
            co2_dots co2.xAxis co2.yAxis co2.points

        dots_ch4 =
            ch4_dots ch4.xAxis ch4.yAxis ch4.points
    in
        div []
            [ div []
                [ draw_graph dots_co2 co2.xAxis co2.yAxis
                , draw_graph dots_ch4 ch4.xAxis ch4.yAxis
                , draw_graph dots_n2o n2o.xAxis n2o.yAxis
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
