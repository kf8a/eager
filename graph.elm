module Graph exposing (..)

import Date exposing (..)
import Date.Extra as DE exposing (..)
import Time exposing (..)
import Html
import Html exposing (..)
import Html exposing (program, Html)
import Html.Attributes as HA exposing (style)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events exposing (..)
import List.Extra as LE
import Http
import HttpBuilder exposing (..)
import LeastSquares exposing (..)
import Round exposing (..)
import SampleIncubation exposing (json, nextJson)
import Data exposing (..)


type Gas
    = CO2
    | N2O
    | CH4


type Msg
    = SwitchInjection Gas Incubation Point
    | SwitchStandard Gas Point
    | FluxGood Incubation
    | FluxMaybeGood Incubation
    | FluxBad Incubation
    | LoadIncubation (Result Http.Error (List Injection))
    | LoadRun (Result Http.Error Run)
    | SavedIncubation (Result Http.Error Incubation)
    | NoOp


initialModel : Model
initialModel =
    { incubation =
        toIncubation (decodeInjections SampleIncubation.json)
            initialStandards
    , next_incubation =
        toIncubation (decodeInjections SampleIncubation.nextJson)
            initialStandards
    , run = initialRun
    , next_run = Nothing
    , status = NotChecked
    }



-- Point extractors


co2_injections : List Injection -> List Point
co2_injections injections =
    let
        pointInterval =
            interval (initialTime injections)
    in
        List.map (\x -> Point (pointInterval x.datetime) x.co2_ppm x.co2_deleted x.id) injections


n2o_injections : List Injection -> List Point
n2o_injections injections =
    let
        pointInterval =
            interval (initialTime injections)
    in
        List.map (\x -> Point (pointInterval x.datetime) x.n2o_ppm x.n2o_deleted x.id) injections


ch4_injections : List Injection -> List Point
ch4_injections injections =
    let
        pointInterval =
            interval (initialTime injections)
    in
        List.map (\x -> Point (pointInterval x.datetime) x.ch4_ppm x.ch4_deleted x.id) injections



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


toFit : List Point -> Flux
toFit points =
    let
        fit =
            fitLineByLeastSquares points
    in
        fluxWithDefault fit


fluxWithDefault : Result String Fit -> Flux
fluxWithDefault fit =
    case fit of
        Ok fit ->
            Flux fit.slope fit.intercept fit.r2

        Err message ->
            Flux 0 0 0


calibrateInjectionCO2 : Flux -> Injection -> Injection
calibrateInjectionCO2 calibration injection =
    let
        co2_ppm =
            calibration.intercept + calibration.slope * injection.co2_mv
    in
        { injection | co2_ppm = co2_ppm }


calibrateIncubationCO2 : Flux -> Incubation -> Incubation
calibrateIncubationCO2 calibration incubation =
    let
        injections =
            incubation.injections
                |> List.map (calibrateInjectionCO2 calibration)
    in
        { incubation | injections = injections }


calibrateRunCO2 : Run -> Run
calibrateRunCO2 run =
    case run.co2_calibration of
        Just calibration ->
            let
                newIncubationList =
                    run.incubations
                        |> List.map (calibrateIncubationCO2 calibration)
            in
                { run | incubations = newIncubationList }

        Nothing ->
            run



-- Translators


toIncubation : List Injection -> List Standard -> Incubation
toIncubation injections standards =
    let
        co2s =
            toFit (co2_injections injections)

        ch4s =
            toFit (ch4_injections injections)

        n2os =
            toFit (n2o_injections injections)
    in
        Incubation
            injections
            1
            (Just co2s)
            (Just ch4s)
            (Just n2os)
            Nothing
            Nothing
            Nothing



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


x_offset : Float
x_offset =
    30.0


y_offset : Float
y_offset =
    10.0


translateCoords : String
translateCoords =
    String.concat
        [ "translate("
        , toString x_offset
        , " "
        , toString y_offset
        , ")"
        ]


viewBox_ : Axis -> Axis -> String
viewBox_ x_axis y_axis =
    String.concat
        [ "0 0 "
        , (toString (x_axis.max_extent + x_offset))
        , " "
        , (toString (y_axis.max_extent + y_offset))
        ]


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
            , y2
                (toString
                    (yAxis.max_extent
                        - (yAxisTransform (flux.intercept + flux.slope * xAxis.max_value))
                    )
                )
            , stroke "black"
            , fill "black"
            ]
            []


draw_standards : Gas -> List Point -> Svg Msg
draw_standards gas points =
    let
        fit =
            fitLineByLeastSquares points

        flux =
            fluxWithDefault fit

        my_dots =
            standardDots gas points
    in
        draw_graph my_dots (toString gas) points flux


draw_injections : Gas -> Incubation -> List Point -> Svg Msg
draw_injections gas incubation points =
    let
        fit =
            fitLineByLeastSquares points

        flux =
            fluxWithDefault fit

        my_dots =
            injectionDots gas incubation points
    in
        draw_graph my_dots (toString gas) points flux


draw_graph : List (Svg Msg) -> String -> List Point -> Flux -> Svg Msg
draw_graph drawing_func label points flux =
    let
        xAxis =
            toXAxis points

        yAxis =
            toYAxis points

        eq =
            String.concat
                [ "y = "
                , Round.round 2 flux.intercept
                , " + "
                , Round.round 4 flux.slope
                , " x"
                ]

        r =
            String.concat
                [ "r = "
                , Round.round 3 flux.r2
                ]
    in
        svg
            [ width (toString (xAxis.max_extent + x_offset + 50))
            , height (toString (yAxis.max_extent + y_offset + 50))

            -- , viewBox (viewBox_ xAxis yAxis)
            ]
            [ g [ transform translateCoords ]
                [ g []
                    drawing_func
                , g [] [ (drawRegressionLine xAxis yAxis flux) ]
                , g []
                    [ drawXAxis xAxis yAxis
                    , drawYAxis xAxis yAxis
                    ]
                , g []
                    [ text_ [ x "10", y "20" ] [ Svg.text label ]
                    , text_ [ x "10", y "40" ] [ Svg.text eq ]
                    , text_ [ x "10", y "60" ] [ Svg.text r ]
                    ]
                ]
            ]


dot : Axis -> Axis -> Msg -> Point -> Svg Msg
dot xAxis yAxis msg point =
    let
        color =
            pointColor point.deleted

        xAxis_transform =
            axisTransform xAxis

        yAxis_transform =
            axisTransform yAxis
    in
        case point.deleted of
            False ->
                circle
                    [ cx (toString (xAxis_transform point.x))
                    , cy (toString (yAxis.max_extent - yAxis_transform point.y))
                    , r "5"
                    , stroke color
                    , fill color
                    , onClick msg
                    ]
                    []

            True ->
                g []
                    [ circle
                        [ cx (toString (xAxis_transform point.x))
                        , cy (toString (yAxis.max_extent - yAxis_transform point.y))
                        , r "5"
                        , stroke color
                        , fill "none"
                        ]
                        []
                    , circle
                        [ cx (toString (xAxis.max_extent))
                        , cy (toString (yAxis.max_extent - yAxis_transform point.y))
                        , r "5"
                        , stroke color
                        , fill color
                        , onClick msg
                        ]
                        []
                    ]


toXAxis : List Point -> Axis
toXAxis points =
    Axis 0 200 0 (maxX points)


toYAxis : List Point -> Axis
toYAxis points =
    Axis 0 200 0 (maxY points)


standardDots : Gas -> List Point -> List (Svg Msg)
standardDots gas points =
    let
        dotTransform =
            dot (toXAxis points) (toYAxis points)
    in
        List.map (\x -> dotTransform (SwitchStandard gas x) x) points


injectionDots : Gas -> Incubation -> List Point -> List (Svg Msg)
injectionDots gas incubation points =
    let
        dotTransform =
            dot (toXAxis points) (toYAxis points)
    in
        List.map (\x -> dotTransform (SwitchInjection gas incubation x) x) points


renderListElement : Point -> Html Msg
renderListElement point =
    let
        msg =
            String.concat
                [ toString point.x
                , ", "
                , toString point.y
                ]
    in
        li [] [ Html.text msg ]


renderList : List Point -> Html Msg
renderList points =
    ul [ HA.style [ ( "float", "right" ) ] ]
        (List.map (\x -> renderListElement x) points)


renderIncubation : Incubation -> Html Msg
renderIncubation incubation =
    div []
        [ draw_injections N2O incubation (n2o_injections incubation.injections)
        , draw_injections CO2 incubation (co2_injections incubation.injections)
        , draw_injections CH4 incubation (ch4_injections incubation.injections)
        ]


orderedIncubation : Incubation -> Incubation -> Order
orderedIncubation a b =
    Basics.compare a.id b.id


view : Model -> Html Msg
view model =
    div []
        [ div []
            [ draw_standards N2O (n2o_standards model.run.standards)
            , draw_standards CO2 (co2_standards model.run.standards)
            , draw_standards CH4 (ch4_standards model.run.standards)
            ]
        , div []
            (model.run.incubations
                |> List.sortWith orderedIncubation
                |> List.map renderIncubation
            )
        , button
            [ onClick (FluxGood model.incubation) ]
            [ Html.text "Save" ]
        , button
            [ onClick (FluxMaybeGood model.incubation) ]
            [ Html.text "Cancel" ]
        ]



-- UPDATE


base_url : String
base_url =
    "http://localhost:4000/api/"


url : String
url =
    base_url ++ "injections?incubation_id=35191"


standardUrl : String
standardUrl =
    base_url ++ "standard_curves/1"


runUrl : String
runUrl =
    base_url ++ "runs/2"


fetchRun : String -> Cmd Msg
fetchRun url =
    Http.get url runResponseDecoder
        |> Http.send LoadRun


fetchNextIncubation : Cmd Msg
fetchNextIncubation =
    Http.get url responseDecoder
        |> Http.send LoadIncubation


standardSaved : Result Http.Error () -> Msg
standardSaved result =
    case result of
        Ok _ ->
            NoOp

        Err msg ->
            let
                _ =
                    Debug.log "ERROR standardSaved  " msg
            in
                NoOp


saveStandardList : List Standard -> Cmd Msg
saveStandardList standardList =
    HttpBuilder.post "http://localhost:4000/api/standards"
        |> withJsonBody (standardListEncoder standardList)
        |> send standardSaved


updateIncubation : Incubation -> (List Injection -> Point -> List Injection) -> Point -> Incubation
updateIncubation incubation updater point =
    let
        new_point =
            { point | deleted = not point.deleted }

        new_injections =
            updater incubation.injections new_point

        new_incubation =
            { incubation | injections = new_injections }
    in
        new_incubation



--- todo  move flux updating here by passing in the extractor


updateRunStandard : Run -> (List Standard -> Point -> List Standard) -> Point -> Run
updateRunStandard run updater point =
    let
        new_point =
            { point | deleted = not point.deleted }

        new_standards =
            updater run.standards new_point

        new_run =
            { run | standards = new_standards }
    in
        new_run


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SwitchInjection CO2 incubation point ->
            let
                updated_incubation =
                    updateIncubation incubation (update_co2_injections) point

                new_flux =
                    toFit (co2_injections updated_incubation.injections)

                newIncubation =
                    { updated_incubation | co2_flux = Just new_flux }

                ( oldIncubation, rest ) =
                    List.partition (\x -> x.id == incubation.id) model.run.incubations

                newIncubationList =
                    List.concat [ rest, [ newIncubation ] ]

                run =
                    model.run

                newRun =
                    { run | incubations = newIncubationList }
            in
                ( { model | run = newRun }, Cmd.none )

        SwitchInjection CH4 incubation point ->
            let
                updated_incubation =
                    updateIncubation incubation (update_ch4_injections) point

                new_flux =
                    toFit (ch4_injections updated_incubation.injections)

                newIncubation =
                    { updated_incubation | ch4_flux = Just new_flux }

                ( oldIncubation, rest ) =
                    List.partition (\x -> x.id == incubation.id) model.run.incubations

                newIncubationList =
                    List.concat [ rest, [ newIncubation ] ]

                run =
                    model.run

                newRun =
                    { run | incubations = newIncubationList }
            in
                ( { model | run = newRun }, Cmd.none )

        SwitchInjection N2O incubation point ->
            let
                updated_incubation =
                    updateIncubation incubation (update_n2o_injections) point

                new_flux =
                    toFit (n2o_injections updated_incubation.injections)

                newIncubation =
                    { updated_incubation | n2o_flux = Just new_flux }

                ( oldIncubation, rest ) =
                    List.partition (\x -> x.id == incubation.id) model.run.incubations

                newIncubationList =
                    List.concat [ rest, [ newIncubation ] ]

                run =
                    model.run

                newRun =
                    { run | incubations = newIncubationList }
            in
                ( { model | run = newRun }, Cmd.none )

        SwitchStandard CO2 point ->
            let
                updatedRun =
                    updateRunStandard model.run (updateCO2Standards) point

                new_flux =
                    toFit (co2_standards updatedRun.standards)

                newRun =
                    { updatedRun | co2_calibration = Just new_flux }
                        |> calibrateRunCO2
            in
                ( { model | run = newRun }, Cmd.none )

        SwitchStandard CH4 point ->
            let
                updatedRun =
                    updateRunStandard model.run (updateCH4Standards) point

                new_flux =
                    toFit (ch4_standards updatedRun.standards)

                newRun =
                    { updatedRun | ch4_calibration = Just new_flux }
            in
                ( { model | run = newRun }, Cmd.none )

        SwitchStandard N2O point ->
            let
                updatedRun =
                    updateRunStandard model.run (updateN2OStandards) point

                new_flux =
                    toFit (n2o_standards updatedRun.standards)

                newRun =
                    { updatedRun | n2o_calibration = Just new_flux }
            in
                ( { model | run = newRun }, Cmd.none )

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

        SavedIncubation (Ok incubation) ->
            ( model, fetchNextIncubation )

        SavedIncubation (Err msg) ->
            let
                _ =
                    Debug.log "error Saving" msg
            in
                --- TODO: need to alert the user that something failed
                ( model, Cmd.none )

        LoadIncubation (Ok injections) ->
            let
                _ =
                    Debug.log "ok incubation" injections

                newModel =
                    { model | next_incubation = (toIncubation injections initialStandards) }
            in
                ( newModel, Cmd.none )

        LoadIncubation (Err message) ->
            let
                _ =
                    Debug.log "Error" message
            in
                ( model, Cmd.none )

        LoadRun (Ok run) ->
            -- TODO: Compute calibration on load
            ( { model | run = run }, Cmd.none )

        LoadRun (Err msg) ->
            let
                _ =
                    Debug.log "Error" msg
            in
                ( model, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


init : ( Model, Cmd Msg )
init =
    ( initialModel, fetchRun runUrl )
