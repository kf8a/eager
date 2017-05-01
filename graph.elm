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
    , slope : Float
    , intercept : Float
    , r2 : Float
    }


type alias Incubation =
    { co2 : Flux
    , ch4 : Flux
    , n2o : Flux
    , injections : List Injection
    , standards : List Standard
    , co2_calibration : Flux
    , ch4_calibration : Flux
    , n2o_calibration : Flux
    }


type alias Standard =
    { n2o_ppm : Float
    , n2o_mv : Float
    , co2_ppm : Float
    , co2_mv : Float
    , ch4_ppm : Float
    , ch4_mv : Float
    , n2o_deleted : Bool
    , co2_deleted : Bool
    , ch4_deleted : Bool
    , id : Int
    }


type alias Axis =
    { min_extent : Float
    , max_extent : Float
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


type Gas
    = CO2
    | N2O
    | CH4


type Status
    = Good
    | Bad
    | MaybeGood
    | NotChecked


type Msg
    = SwitchCO2 Point
    | SwitchCH4 Point
    | SwitchN2O Point
    | SwitchPoint Gas Point
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


initialStandard : Standard
initialStandard =
    { n2o_ppm = 0.3
    , n2o_mv = 100
    , co2_ppm = 500
    , co2_mv = 1000
    , ch4_ppm = 2.0
    , ch4_mv = 50.0
    , n2o_deleted = False
    , co2_deleted = False
    , ch4_deleted = False
    , id = 0
    }


initialStandards : List Standard
initialStandards =
    [ initialStandard ]


sortedRecords : Injection -> Injection -> Order
sortedRecords a b =
    DE.compare a.datetime b.datetime



-- Transformations


co2_standards : List Standard -> List Point
co2_standards standards =
    List.map (\x -> Point x.co2_ppm x.co2_mv x.co2_deleted x.id) standards


n2o_standards : List Standard -> List Point
n2o_standards standards =
    List.map (\x -> Point x.n2o_ppm x.n2o_mv x.n2o_deleted x.id) standards


ch4_standards : List Standard -> List Point
ch4_standards standards =
    List.map (\x -> Point x.ch4_ppm x.ch4_mv x.ch4_deleted x.id) standards


updateN2OStandard : Standard -> Point -> Standard
updateN2OStandard standard n2o =
    if n2o.id == standard.id then
        { standard | n2o_ppm = n2o.x, n2o_mv = n2o.y, n2o_deleted = n2o.deleted }
    else
        let
            -- TODO: Log this to the server side
            msg =
                String.concat [ "ERROR: ", toString n2o, " did not match any id in " ]

            _ =
                Debug.log msg standard
        in
            standard


updateCO2Standard : Standard -> Point -> Standard
updateCO2Standard standard co2 =
    if co2.id == standard.id then
        { standard | co2_ppm = co2.x, co2_mv = co2.y, co2_deleted = co2.deleted }
    else
        let
            -- TODO: Log this to the server side
            msg =
                String.concat [ "ERROR: ", toString co2, " did not match any id in " ]

            _ =
                Debug.log msg standard
        in
            standard


updateCH4Standard : Standard -> Point -> Standard
updateCH4Standard standard ch4 =
    if ch4.id == standard.id then
        { standard | ch4_ppm = ch4.x, ch4_mv = ch4.y, ch4_deleted = ch4.deleted }
    else
        let
            -- TODO: Log this to the server side
            msg =
                String.concat [ "ERROR: ", toString ch4, " did not match any id in " ]

            _ =
                Debug.log msg standard
        in
            standard


updateN2OStandards : List Standard -> Point -> List Standard
updateN2OStandards standards n2o =
    let
        ( standard, rest ) =
            List.partition (\x -> x.id == n2o.id) standards

        newStandard =
            case (List.head standard) of
                Just myStandard ->
                    [ updateN2OStandard myStandard n2o ]

                Nothing ->
                    []
    in
        rest ++ newStandard


updateCO2Standards : List Standard -> Point -> List Standard
updateCO2Standards standards co2 =
    let
        ( standard, rest ) =
            List.partition (\x -> x.id == co2.id) standards

        newStandard =
            case (List.head standard) of
                Just myStandard ->
                    [ updateCO2Standard myStandard co2 ]

                Nothing ->
                    []
    in
        rest ++ newStandard


updateCH4Standards : List Standard -> Point -> List Standard
updateCH4Standards standards ch4 =
    let
        ( standard, rest ) =
            List.partition (\x -> x.id == ch4.id) standards

        newStandard =
            case (List.head standard) of
                Just myStandard ->
                    [ updateCH4Standard myStandard ch4 ]

                Nothing ->
                    []
    in
        rest ++ newStandard



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


fluxWithDefault : Result String Fit -> List Point -> Flux
fluxWithDefault fit points =
    case fit of
        Ok fit ->
            Flux points fit.slope fit.intercept fit.r2

        Err message ->
            Flux points 0 0 0


toIncubation : List Injection -> List Standard -> Incubation
toIncubation injections standards =
    let
        startTime =
            initialTime injections

        pointInterval =
            interval startTime

        co2Points =
            List.map
                (\x ->
                    Point (pointInterval x.datetime) x.co2_ppm x.deleted x.id
                )
                injections

        fit =
            fitLineByLeastSquares co2Points

        co2s =
            fluxWithDefault fit co2Points

        ch4Points =
            List.map
                (\x ->
                    Point (pointInterval x.datetime) x.ch4_ppm x.deleted x.id
                )
                injections

        fitch4 =
            fitLineByLeastSquares ch4Points

        ch4s =
            fluxWithDefault fitch4 ch4Points

        n2oPoints =
            List.map
                (\x ->
                    Point (pointInterval x.datetime) x.n2o_ppm x.deleted x.id
                )
                injections

        fitn2o =
            fitLineByLeastSquares n2oPoints

        n2os =
            fluxWithDefault fitn2o n2oPoints
    in
        Incubation
            co2s
            ch4s
            n2os
            injections
            initialStandards
            co2s
            co2s
            co2s



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
        |> hardcoded False
        |> hardcoded False
        |> hardcoded False
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
    if value > axis.max_value then
        axis.max_extent
    else if value < axis.min_value then
        axis.min_extent
    else
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


drawRegressionLine : Axis -> Axis -> Flux -> Svg Msg
drawRegressionLine xAxis yAxis flux =
    let
        xAxisTransform =
            axisTransform xAxis

        yAxisTransform =
            axisTransform yAxis
    in
        line
            [ x1 (toString (xAxisTransform xAxis.min_value))
            , x2 (toString (xAxisTransform xAxis.max_value))
            , y1 (toString (yAxis.max_extent - (yAxisTransform flux.intercept)))
            , y2 (toString (yAxis.max_extent - (yAxisTransform flux.slope) * xAxis.max_value))
            , stroke "black"
            , fill "black"
            ]
            []


draw_standards : Gas -> List Point -> Svg Msg
draw_standards gas points =
    let
        max_x =
            maxX points

        max_y =
            maxY points

        xAxis =
            toXAxis flux

        yAxis =
            toYAxis flux

        _ =
            Debug.log "points" points

        fit =
            Debug.log "fit"
                (fitLineByLeastSquares points)

        flux =
            Debug.log "flux"
                (fluxWithDefault fit points)

        my_dots =
            dots2 gas flux
    in
        draw_graph my_dots flux


draw_graph : List (Svg Msg) -> Flux -> Svg Msg
draw_graph drawing_func flux =
    let
        xAxis =
            toXAxis flux

        yAxis =
            toYAxis flux
    in
        svg
            [ width (toString (xAxis.max_extent + 10))
            , height (toString (yAxis.max_extent * 2))
            , viewBox (viewBox_ xAxis yAxis)
            ]
            [ g [ transform translateCoords ]
                [ g []
                    drawing_func
                , g [] [ (drawRegressionLine xAxis yAxis flux) ]
                , g []
                    [ drawXAxis xAxis yAxis
                    , drawYAxis xAxis yAxis
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


toXAxis : Flux -> Axis
toXAxis flux =
    Axis 0 200 0 (maxX flux.points)


toYAxis : Flux -> Axis
toYAxis flux =
    Axis 0 200 0 (maxY flux.points)


dots2 : Gas -> Flux -> List (Svg Msg)
dots2 gas flux =
    let
        dotTransform =
            dot (toXAxis flux) (toYAxis flux)
    in
        List.map (\x -> dotTransform (SwitchPoint gas x) x) flux.points


dots : (Point -> Msg) -> Flux -> List (Svg Msg)
dots msg flux =
    let
        dotTransform =
            dot (toXAxis flux) (toYAxis flux)
    in
        List.map (\x -> dotTransform (msg x) x) flux.points


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
            dots SwitchN2O n2o

        dots_co2 =
            dots SwitchCO2 co2

        dots_ch4 =
            dots SwitchCH4 ch4
    in
        div []
            [ div []
                [ draw_standards N2O (n2o_standards model.incubation.standards)
                , draw_standards CO2 (co2_standards model.incubation.standards)
                , draw_standards CH4 (ch4_standards model.incubation.standards)
                ]
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

        SwitchPoint CO2 point ->
            let
                _ =
                    Debug.log "CO2 point" point

                incubation =
                    model.incubation
            in
                ( model, Cmd.none )

        SwitchPoint CH4 point ->
            let
                _ =
                    Debug.log "CH4 point" point

                incubation =
                    model.incubation

                standards =
                    Debug.log "standards"
                        incubation.standards
            in
                ( model, Cmd.none )

        SwitchPoint N2O point ->
            let
                _ =
                    Debug.log "N2O point" point

                incubation =
                    model.incubation

                standards =
                    incubation.standards
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
                incubation =
                    model.incubation

                newIncubation =
                    { incubation | standards = standards }
            in
                ( { model | incubation = newIncubation }, Cmd.none )

        SaveStandard standards ->
            ( model, Cmd.none )


init : ( Model, Cmd Msg )
init =
    ( initialModel, fetchStandard )
