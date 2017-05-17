module Graph exposing (..)

import Date exposing (..)
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


-- import SampleIncubation exposing (json, nextJson)

import Data exposing (..)


type Msg
    = SwitchInjection Gas Incubation Point
    | SwitchStandard Gas Point
    | LoadRun (Result Http.Error Run)
    | RunSaved (Result Http.Error ())
    | SaveRun
    | LoadRunIds (Result Http.Error (List Run))
      -- | FluxGood Incubation
      -- | FluxMaybeGood Incubation
      -- | FluxBad Incubation
      -- | SavedIncubation (Result Http.Error Incubation)
    | NoOp


initialModel : Model
initialModel =
    { run = initialRun
    , next_run = Nothing
    , status = NotChecked
    , saving = False
    , error = Nothing
    , previous_runs = []
    , next_runs = []
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


fluxWithDefault : Gas -> Result String Fit -> Flux
fluxWithDefault gas fit =
    case fit of
        Ok fit ->
            Flux fit.slope fit.intercept fit.r2 Nothing gas

        Err message ->
            initialFlux



--- Calibration


calibrateInjectionN2O : Flux -> Injection -> Injection
calibrateInjectionN2O calibration injection =
    let
        n2o_ppm =
            calibration.intercept + calibration.slope * injection.n2o_mv
    in
        { injection | n2o_ppm = n2o_ppm }


calibrateInjectionCH4 : Flux -> Injection -> Injection
calibrateInjectionCH4 calibration injection =
    let
        ch4_ppm =
            calibration.intercept + calibration.slope * injection.ch4_mv
    in
        { injection | ch4_ppm = ch4_ppm }


calibrateInjectionCO2 : Flux -> Injection -> Injection
calibrateInjectionCO2 calibration injection =
    let
        co2_ppm =
            calibration.intercept + calibration.slope * injection.co2_mv
    in
        { injection | co2_ppm = co2_ppm }


calibrateIncubationN2O : Flux -> Incubation -> Incubation
calibrateIncubationN2O calibration incubation =
    let
        injections =
            incubation.injections
                |> List.map (calibrateInjectionN2O calibration)
    in
        { incubation | injections = injections }


calibrateIncubationCH4 : Flux -> Incubation -> Incubation
calibrateIncubationCH4 calibration incubation =
    let
        injections =
            incubation.injections
                |> List.map (calibrateInjectionCH4 calibration)
    in
        { incubation | injections = injections }


calibrateIncubationCO2 : Flux -> Incubation -> Incubation
calibrateIncubationCO2 calibration incubation =
    let
        injections =
            incubation.injections
                |> List.map (calibrateInjectionCO2 calibration)
    in
        { incubation | injections = injections }


calibrateRunN2O : Run -> Run
calibrateRunN2O run =
    case run.n2o_calibration of
        Just calibration ->
            let
                newIncubationList =
                    run.incubations
                        |> List.map (calibrateIncubationN2O calibration)
            in
                { run | incubations = newIncubationList }

        Nothing ->
            run


calibrateRunCH4 : Run -> Run
calibrateRunCH4 run =
    case run.ch4_calibration of
        Just calibration ->
            let
                newIncubationList =
                    run.incubations
                        |> List.map (calibrateIncubationCH4 calibration)
            in
                { run | incubations = newIncubationList }

        Nothing ->
            run


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


computeCalibrationN2O : List Standard -> Flux
computeCalibrationN2O standards =
    computeFlux N2O (n2o_standards standards)


computeCalibrationCH4 : List Standard -> Flux
computeCalibrationCH4 standards =
    computeFlux CH4 (ch4_standards standards)


computeCalibrationCO2 : List Standard -> Flux
computeCalibrationCO2 standards =
    computeFlux CO2 (co2_standards standards)


updateCalibrationN2O : Run -> Point -> Run
updateCalibrationN2O run point =
    let
        updatedRun =
            updateRunStandard run (updateN2OStandards) point
    in
        { updatedRun
            | n2o_calibration = Just (computeCalibrationN2O updatedRun.standards)
        }
            |> calibrateRunN2O
            |> computeN2OFluxes


updateCalibrationCH4 : Run -> Point -> Run
updateCalibrationCH4 run point =
    let
        updatedRun =
            updateRunStandard run (updateCH4Standards) point
    in
        { updatedRun | ch4_calibration = Just (computeCalibrationCH4 updatedRun.standards) }
            |> calibrateRunCH4
            |> computeCH4Fluxes


updateCalibrationCO2 : Run -> Point -> Run
updateCalibrationCO2 run point =
    let
        updatedRun =
            updateRunStandard run (updateCO2Standards) point
    in
        { updatedRun | co2_calibration = Just (computeCalibrationCO2 updatedRun.standards) }
            |> calibrateRunCO2
            |> computeCO2Fluxes



--- Compute Fluxes


computeFlux : Gas -> List Point -> Flux
computeFlux gas points =
    points
        |> fitLineByLeastSquares
        |> fluxWithDefault gas


computeIncubationFluxes : Incubation -> Incubation
computeIncubationFluxes incubation =
    let
        n2o_flux =
            incubation.injections
                |> n2o_injections
                |> computeFlux N2O

        co2_flux =
            incubation.injections
                |> co2_injections
                |> computeFlux CO2

        ch4_flux =
            incubation.injections
                |> ch4_injections
                |> computeFlux CH4
    in
        { incubation
            | n2o_flux = Just n2o_flux
            , ch4_flux = Just ch4_flux
            , co2_flux = Just co2_flux
        }



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



-- swapIncubation : Model -> Model
-- swapIncubation model =
--     let
--         inc =
--             model.next_incubation
--     in
--         { model
--             | next_incubation = model.incubation
--             , incubation = inc
--         }


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


drawRegressionLine : Axis -> Axis -> Maybe Flux -> Svg Msg
drawRegressionLine xAxis yAxis flux =
    case flux of
        Just flux ->
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

        Nothing ->
            g [] []


draw_standards : Gas -> Maybe Flux -> List Point -> Svg Msg
draw_standards gas flux points =
    let
        my_dots =
            standardDots gas points
    in
        draw_graph my_dots (toString gas) points flux


draw_injections : Gas -> Maybe Flux -> Incubation -> List Point -> Svg Msg
draw_injections gas flux incubation points =
    let
        my_dots =
            injectionDots gas incubation points
    in
        draw_graph my_dots (toString gas) points flux


renderFluxEq : String -> Maybe Flux -> Svg Msg
renderFluxEq label flux =
    case flux of
        Just flux ->
            let
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
                g []
                    [ text_ [ x "10", y "20" ] [ Svg.text label ]
                    , text_ [ x "10", y "40" ] [ Svg.text eq ]
                    , text_ [ x "10", y "60" ] [ Svg.text r ]
                    ]

        Nothing ->
            g [] []


draw_graph : List (Svg Msg) -> String -> List Point -> Maybe Flux -> Svg Msg
draw_graph drawing_func label points flux =
    let
        xAxis =
            toXAxis points

        yAxis =
            toYAxis points
    in
        svg
            [ width (toString (xAxis.max_extent + x_offset + 50))
            , height (toString (yAxis.max_extent + y_offset + 50))

            -- , viewBox (viewBox_ xAxis yAxis)
            ]
            [ g [ transform translateCoords ]
                [ g [] [ (drawRegressionLine xAxis yAxis flux) ]
                , g []
                    [ drawXAxis xAxis yAxis
                    , drawYAxis xAxis yAxis
                    ]
                , renderFluxEq label flux
                , g []
                    drawing_func
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
                        [ cx (toString (xAxis.max_extent + 20))
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
    Axis 0 100 0 (maxX points)


toYAxis : List Point -> Axis
toYAxis points =
    Axis 0 100 0 (maxY points)


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
        [ span [] [ Html.text incubation.chamber ]
        , draw_injections N2O incubation.n2o_flux incubation (n2o_injections incubation.injections)
        , draw_injections CO2 incubation.co2_flux incubation (co2_injections incubation.injections)
        , draw_injections CH4 incubation.ch4_flux incubation (ch4_injections incubation.injections)
        ]


orderedIncubation : Incubation -> Incubation -> Order
orderedIncubation a b =
    Basics.compare a.id b.id


showSavingIndicator : Model -> Html Msg
showSavingIndicator model =
    case model.saving of
        True ->
            div [] [ Html.text "Saving run" ]

        False ->
            div [] []


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick SaveRun ]
            [ Html.text "Save" ]
        , showSavingIndicator model
        , div []
            [ draw_standards N2O model.run.n2o_calibration (n2o_standards model.run.standards)
            , draw_standards CO2 model.run.co2_calibration (co2_standards model.run.standards)
            , draw_standards CH4 model.run.ch4_calibration (ch4_standards model.run.standards)
            ]
        , div []
            (model.run.incubations
                |> List.sortWith orderedIncubation
                |> List.map renderIncubation
            )

        -- , button
        --     [ onClick (FluxGood model.incubation) ]
        --     [ Html.text "Save" ]
        -- , button
        --     [ onClick (FluxMaybeGood model.incubation) ]
        --     [ Html.text "Cancel" ]
        ]



-- UPDATE


base_url : String
base_url =
    "http://localhost:4000/api/"


runUrl : Int -> String
runUrl id =
    base_url ++ "runs/" ++ (toString id)


runIdUrl : String
runIdUrl =
    base_url ++ "runs"


fetchRunIds : Cmd Msg
fetchRunIds =
    Http.get runIdUrl runIdResponseDecoder
        |> Http.send LoadRunIds


fetchRun : Run -> Cmd Msg
fetchRun run =
    Http.get (runUrl run.id) runResponseDecoder
        |> Http.send LoadRun


runSaved : Result Http.Error () -> Msg
runSaved result =
    RunSaved result



-- case result of
--     Ok _ ->
--         RunSaved
--
--     Err msg ->
--         let
--             _ =
--                 Debug.log "ERROR" msg
--         in
--             NoOp


saveUrl : Int -> String
saveUrl id =
    "http://localhost:4000/api/runs/" ++ (toString id)


saveRun : Run -> Cmd Msg
saveRun run =
    HttpBuilder.put (saveUrl run.id)
        |> withJsonBody (runEncoder run)
        |> send runSaved


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



--- TODO: move flux updating here by passing in the extractor


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


computeCO2Flux : Incubation -> Incubation
computeCO2Flux incubation =
    let
        new_flux =
            computeFlux CO2 (co2_injections incubation.injections)
    in
        { incubation | co2_flux = Just new_flux }


computeN2OFlux : Incubation -> Incubation
computeN2OFlux incubation =
    let
        new_flux =
            computeFlux N2O (n2o_injections incubation.injections)
    in
        { incubation | n2o_flux = Just new_flux }


computeCH4Flux : Incubation -> Incubation
computeCH4Flux incubation =
    let
        new_flux =
            computeFlux CH4 (ch4_injections incubation.injections)
    in
        { incubation | ch4_flux = Just new_flux }


computeN2OFluxes : Run -> Run
computeN2OFluxes run =
    let
        incubations =
            List.map computeN2OFlux run.incubations
    in
        { run | incubations = incubations }


computeCO2Fluxes : Run -> Run
computeCO2Fluxes run =
    let
        incubations =
            List.map computeCO2Flux run.incubations
    in
        { run | incubations = incubations }


computeCH4Fluxes : Run -> Run
computeCH4Fluxes run =
    let
        incubations =
            List.map computeCH4Flux run.incubations
    in
        { run | incubations = incubations }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SwitchInjection CO2 incubation point ->
            let
                updated_incubation =
                    updateIncubation incubation (update_co2_injections) point

                newIncubation =
                    computeCO2Flux updated_incubation

                run =
                    model.run

                ( oldIncubation, rest ) =
                    List.partition (\x -> x.id == incubation.id) run.incubations

                newIncubationList =
                    List.concat [ rest, [ newIncubation ] ]

                newRun =
                    { run | incubations = newIncubationList }
            in
                ( { model | run = newRun }, Cmd.none )

        SwitchInjection CH4 incubation point ->
            let
                run =
                    model.run

                updated_incubation =
                    updateIncubation incubation (update_ch4_injections) point

                newIncubation =
                    computeCH4Flux updated_incubation

                ( oldIncubation, rest ) =
                    List.partition (\x -> x.id == incubation.id) run.incubations

                newIncubationList =
                    List.concat [ rest, [ newIncubation ] ]

                newRun =
                    { run | incubations = newIncubationList }
            in
                ( { model | run = newRun }, Cmd.none )

        SwitchInjection N2O incubation point ->
            let
                updated_incubation =
                    updateIncubation incubation (update_n2o_injections) point

                new_flux =
                    computeFlux N2O (n2o_injections updated_incubation.injections)

                newIncubation =
                    { updated_incubation | n2o_flux = Just new_flux }

                run =
                    model.run

                ( oldIncubation, rest ) =
                    List.partition (\x -> x.id == incubation.id) run.incubations

                newIncubationList =
                    List.concat [ rest, [ newIncubation ] ]

                newRun =
                    { run | incubations = newIncubationList }
            in
                ( { model | run = newRun }, Cmd.none )

        SwitchInjection NoGas incubation point ->
            ( model, Cmd.none )

        SwitchStandard CO2 point ->
            ( { model | run = updateCalibrationCO2 model.run point }, Cmd.none )

        SwitchStandard CH4 point ->
            ( { model | run = updateCalibrationCH4 model.run point }, Cmd.none )

        SwitchStandard N2O point ->
            ( { model | run = updateCalibrationN2O model.run point }, Cmd.none )

        SwitchStandard NoGas point ->
            ( model, Cmd.none )

        -- FluxGood incubation ->
        --     let
        --         new_model =
        --             swapIncubation model
        --     in
        --         ( { new_model | status = Good }, fetchNextIncubation )
        --
        -- FluxMaybeGood incubation ->
        --     let
        --         new_model =
        --             swapIncubation model
        --     in
        --         ( { new_model | status = MaybeGood }, fetchNextIncubation )
        --
        -- FluxBad incubation ->
        --     let
        --         new_model =
        --             swapIncubation model
        --     in
        --         ( { new_model | status = Bad }, fetchNextIncubation )
        -- SavedIncubation (Ok incubation) ->
        --     ( model, fetchNextIncubation )
        --
        -- SavedIncubation (Err msg) ->
        --     let
        --         _ =
        --             Debug.log "error Saving" msg
        --     in
        --         --- TODO: need to alert the user that something failed
        --         ( model, Cmd.none )
        RunSaved (Ok run) ->
            ( { model | saving = False }, Cmd.none )

        RunSaved (Err msg) ->
            ( model, Cmd.none )

        SaveRun ->
            ( { model | saving = True }, saveRun model.run )

        LoadRun (Ok run) ->
            let
                co2_cal =
                    computeCalibrationCO2 run.standards

                ch4_cal =
                    computeCalibrationCH4 run.standards

                n2o_cal =
                    computeCalibrationN2O run.standards

                newRun =
                    { run
                        | co2_calibration = Just co2_cal
                        , ch4_calibration = Just ch4_cal
                        , n2o_calibration = Just n2o_cal
                    }
                        |> calibrateRunCO2
                        |> calibrateRunCH4
                        |> calibrateRunN2O

                updatedRun =
                    { newRun | incubations = List.map computeIncubationFluxes newRun.incubations }
            in
                ( { model | run = updatedRun }, Cmd.none )

        LoadRun (Err msg) ->
            let
                _ =
                    Debug.log "Error" msg
            in
                ( model, Cmd.none )

        LoadRunIds (Ok runs) ->
            let
                current_run =
                    Maybe.withDefault initialRun (List.head runs)

                next_runs =
                    Maybe.withDefault [] (List.tail runs)
            in
                -- TODO: fetch first run
                ( { model | run = current_run, next_runs = next_runs }
                , (fetchRun current_run)
                )

        LoadRunIds (Err msg) ->
            let
                _ =
                    Debug.log "Error" msg
            in
                ( model, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


init : ( Model, Cmd Msg )
init =
    ( initialModel, fetchRunIds )
