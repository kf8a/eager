module Graph exposing (..)

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
import List.Extra as LE
import Http
import LeastSquares exposing (..)
import SampleIncubation exposing (json, nextJson)


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
    , standards : List Standard
    , co2_calibration : Calibration
    , ch4_calibration : Calibration
    , n2o_calibration : Calibration
    }


type alias Standard =
    { n2o_ppm : Float
    , n2o_mv : Float
    , co2_ppm : Float
    , co2_mv : Float
    , ch4_ppm : Float
    , ch4_mv : Float
    , id : Int
    }


type alias Calibration =
    { slope : Float
    , intercept : Float
    , r2 : Float
    }



-- { points : List Point
-- , xAxis : Axis
-- , yAxis : Axis
-- , slope : Float
-- , intercept : Float
-- , r2 : Float
-- }


type alias Axis =
    { max_extent : Float
    , min_extent : Float
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
    | LoadIncubation (Result Http.Error (List Injection))
    | LoadStandard (Result Http.Error (List Standard))
    | SaveStandard (List Standard)


initialModel : Model
initialModel =
    { incubation =
        toIncubation (decodeInjections SampleIncubation.json)
            initialStandards
    , next_incubation =
        toIncubation (decodeInjections SampleIncubation.nextJson)
            initialStandards
    , status = NotChecked
    }


initialCalibration : Calibration
initialCalibration =
    { slope = 1.0
    , intercept = 0.0
    , r2 = 0.5
    }


initialStandard : Standard
initialStandard =
    { n2o_ppm = 0.3
    , n2o_mv = 100
    , co2_ppm = 500
    , co2_mv = 1000
    , ch4_ppm = 2.0
    , ch4_mv = 50.0
    , id = 0
    }


initialStandards : List Standard
initialStandards =
    [ initialStandard ]
    
    
initialAxis : Axis
initialAxis =
  Axis 200 0 0 200


sortedRecords : Injection -> Injection -> Order
sortedRecords a b =
    DE.compare a.datetime b.datetime



-- Translators


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


fluxWithDefault : Result String Fit -> Axis -> Axis -> List Point -> Flux
fluxWithDefault fit xAxis yAxis points =
    case fit of
        Ok fit ->
            Flux points xAxis yAxis fit.slope fit.intercept

        Err message ->
            let
                _ =
                    Debug.log "Fit Error " message
            in
                Flux points xAxis yAxis 0 0


toIncubation : List Injection -> List Standard -> Incubation
toIncubation injections standards =
    let
        startTime =
            initialTime injections

        pointInterval =
            interval startTime

        xAxis =
            Axis 200 0 0 200

        yAxis =
            Axis 120 0 0

        co2Points =
            List.map
                (\x ->
                    Point (pointInterval x.datetime) x.co2_ppm x.deleted x.id
                )
                injections

        fit =
            fitLineByLeastSquares co2Points

        maxYco2 =
            maxY co2Points

        co2s =
            fluxWithDefault fit xAxis (yAxis (maxY co2Points)) co2Points

        ch4Points =
            List.map
                (\x ->
                    Point (pointInterval x.datetime) x.ch4_ppm x.deleted x.id
                )
                injections

        fitch4 =
            fitLineByLeastSquares ch4Points

        ch4s =
            fluxWithDefault fitch4 xAxis (yAxis (maxY ch4Points)) ch4Points

        n2oPoints =
            List.map
                (\x ->
                    Point (pointInterval x.datetime) x.n2o_ppm x.deleted x.id
                )
                injections

        fitn2o =
            fitLineByLeastSquares n2oPoints

        n2os =
            fluxWithDefault fitn2o xAxis (yAxis (maxY n2oPoints)) n2oPoints
    in
        Incubation co2s
            ch4s
            n2os
            injections
            initialStandards
            initialCalibration
            initialCalibration
            initialCalibration



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


decodeStandards : String -> List Standard
decodeStandards json =
    case decodeString standardResponseDecoder json of
        Ok standards ->
            standards

        Err msg ->
            []


standardDecoder : Decoder Standard
standardDecoder =
    decode Standard
        |> required "n2o_ppm" float
        |> required "n2o_mv" float
        |> required "co2_ppm" float
        |> required "co2_mv" float
        |> required "ch4_ppm" float
        |> required "ch4_mv" float
        |> required "id" int


standardDataDecoder : Decoder (List Standard)
standardDataDecoder =
    decode identity
        |> required "standards" (list standardDecoder)


standardResponseDecoder : Decoder (List Standard)
standardResponseDecoder =
    decode identity
        |> required "data" standardDataDecoder



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

        fit =
            fitLineByLeastSquares newPoints

        newCO2 =
            case fit of
                Ok fitted ->
                    { co2
                        | points = newPoints
                        , slope = fitted.slope
                        , intercept = fitted.intercept
                    }

                _ ->
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

        fit =
            fitLineByLeastSquares newPoints

        newCH4 =
            case fit of
                Ok fitted ->
                    { ch4
                        | points = newPoints
                        , slope = fitted.slope
                        , intercept = fitted.intercept
                    }

                _ ->
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

        fit =
            fitLineByLeastSquares newPoints

        newN2O =
            case fit of
                Ok fitted ->
                    { n2o
                        | points = newPoints
                        , slope = fitted.slope
                        , intercept = fitted.intercept
                    }

                _ ->
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



-- VIEW


translateCoords : String
translateCoords =
    "translate(20,0)"


viewBox_ : Axis -> Axis -> String
viewBox_ x_axis y_axis =
    String.concat [ "0 0 ", (toString x_axis.max_extent), " ", (toString y_axis.max_extent) ]


drawXAxis : Axis -> Axis -> Svg Msg
drawXAxis xAxis yAxis =
    line
        [ x1 (toString xAxis.min_extent)
        , x2 (toString xAxis.max_extent)
        , y1 (toString yAxis.max_extent)
        , y2 (toString yAxis.max_extent)
        , stroke "black"
        , fill "black"
        ]
        []


drawYAxis : Axis -> Axis -> Svg Msg
drawYAxis xAxis yAxis =
    line
        [ x1 (toString xAxis.min_extent)
        , x2 (toString xAxis.min_extent)
        , y1 (toString yAxis.min_extent)
        , y2 (toString yAxis.max_extent)
        , stroke "black"
        , fill "black"
        ]
        []


drawRegressionLine : Flux -> Svg Msg
drawRegressionLine flux =
    let
        xAxisTransform =
            axisTransform flux.xAxis

        yAxisTransform =
            axisTransform flux.yAxis
    in
        line
            [ x1 "0"
            , x2 (toString (xAxisTransform flux.xAxis.max_value))
            , y1 (toString (flux.yAxis.max_extent - yAxisTransform flux.intercept))
            , y2 (toString (yAxisTransform flux.slope * flux.xAxis.max_value))
            , stroke "black"
            , fill "black"
            ]
            []


co2_standards : List Standard -> List Point
co2_standards standards =
    List.map (\x -> Point x.co2_ppm x.co2_mv False x.id) standards


n2o_standards : List Standard -> List Point
n2o_standards standards =
    List.map (\x -> Point x.n2o_ppm x.n2o_mv False x.id) standards


ch4_standards : List Standard -> List Point
ch4_standards standards =
    List.map (\x -> Point x.ch4_ppm x.ch4_mv False x.id) standards


draw_standards : List Standard -> Svg Msg
draw_standards standards =
    svg
        [ width "200"
        , height "100"
        , viewBox "0 0 200 100"
        ]
        [ g [] [] ]


draw_graph : List (Svg Msg) -> Flux -> Svg Msg
draw_graph drawing_func flux =
    svg
        [ width (toString (flux.xAxis.max_extent + 10))
        , height (toString (flux.yAxis.max_extent * 2))
        , viewBox (viewBox_ flux.xAxis flux.yAxis)
        ]
        [ g [ transform translateCoords ]
            [ g []
                drawing_func
            , g [] [ (drawRegressionLine flux) ]
            , g []
                [ drawXAxis flux.xAxis flux.yAxis
                , drawYAxis flux.xAxis flux.yAxis
                ]
            ]
        ]


dot : Axis -> Axis -> Msg -> Point -> Svg Msg
dot x_axis yAxis msg point =
    let
        color =
            pointColor point.deleted

        x_axis_transform =
            axisTransform x_axis

        yAxis_transform =
            axisTransform yAxis
    in
        circle
            [ cx (toString (x_axis_transform point.x))
            , cy (toString (yAxis.max_extent - yAxis_transform point.y))
            , r "5"
            , stroke color
            , fill color
            , onClick msg
            ]
            []


dots : (Point -> Msg) -> Axis -> Axis -> List Point -> List (Svg Msg)
dots msg xAxis yAxis points =
    let
        dotTransform =
            dot xAxis yAxis
    in
        List.map (\x -> dotTransform (msg x) x) points


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
            dots SwitchN2O n2o.xAxis n2o.yAxis n2o.points

        dots_co2 =
            dots SwitchCO2 co2.xAxis co2.yAxis co2.points

        dots_ch4 =
            dots SwitchCH4 ch4.xAxis ch4.yAxis ch4.points
    in
        div []
            [ div []
                [ draw_standards model.incubation.standards ]
            , div []
                [ draw_graph dots_co2 co2
                , draw_graph dots_ch4 ch4
                , draw_graph dots_n2o n2o
                ]
            , button
                [ onClick (FluxGood model.incubation) ]
                [ Html.text "Good" ]
            , button
                [ onClick (FluxMaybeGood model.incubation) ]
                [ Html.text "Maybe" ]
            , button
                [ onClick (FluxBad model.incubation) ]
                [ Html.text "Bad" ]
            ]



-- update


url : String
url =
    "http://localhost:4000/api/injections?incubation_id=35191"


standardUrl : String
standardUrl =
    "http://localhost:4000/api/standard_curves/1"


fetchStandard : Cmd Msg
fetchStandard =
    Http.get standardUrl standardResponseDecoder
        |> Http.send LoadStandard


fetchNextIncubation : Cmd Msg
fetchNextIncubation =
    Http.get url responseDecoder
        |> Http.send LoadIncubation


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
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

        LoadIncubation (Ok injections) ->
            let
                _ =
                    Debug.log "ok incubation" injections

                newModel =
                    Debug.log "new model" { model | next_incubation = (toIncubation injections initialStandards) }
            in
                ( newModel, Cmd.none )

        LoadIncubation (Err message) ->
            let
                _ =
                    Debug.log "Error" message
            in
                ( model, Cmd.none )

        LoadStandard (Err message) ->
            let
                _ =
                    Debug.log "Error" message
            in
                ( model, Cmd.none )

        LoadStandard (Ok standards) ->
            let
                _ =
                    Debug.log "ok standards" standards
            in
                ( model, Cmd.none )


init : ( Model, Cmd Msg )
init =
    ( initialModel, fetchStandard )
