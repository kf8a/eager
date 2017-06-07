module Graph exposing (..)

import Date.Extra as DE exposing (..)
import Html
import Html exposing (..)
import Html exposing (program, Html)
import Html.Attributes as HA exposing (style, href, target)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events exposing (..)
import List.Extra as LE
import Http
import HttpBuilder exposing (..)
import LeastSquares exposing (..)
import Calibration exposing (..)
import Round exposing (..)
import Data exposing (..)


type Msg
    = SwitchInjection Gas Incubation Point
    | SwitchStandard Gas Point
    | LoadRun (Result Http.Error Run)
    | RunSaved (Result Http.Error ())
    | SaveRun
    | LoadRunIds (Result Http.Error (List Run))
    | PrevRun
    | NextRun
    | FluxGood Run
    | FluxMaybeGood Run
    | FluxBad Run
    | DeleteAllPoints Incubation
      -- | SavedIncubation (Result Http.Error Incubation)
    | NoOp


initialModel : Model
initialModel =
    { run = initialRun
    , status = NotChecked
    , saving = False
    , error = Nothing
    , previous_runs = []
    , next_runs = []
    }



--- VIEW


maxY : List Point -> Float
maxY points =
    case LE.maximumBy .y points of
        Just point ->
            point.y

        Nothing ->
            120


minY : List Point -> Float
minY points =
    case LE.minimumBy .y points of
        Just point ->
            point.y

        Nothing ->
            0


maxX : List Point -> Float
maxX points =
    case LE.maximumBy .x points of
        Just point ->
            point.x

        Nothing ->
            120


minX : List Point -> Float
minX points =
    case LE.minimumBy .x points of
        Just point ->
            point.x

        Nothing ->
            0


axisTransform : Axis -> Float -> Float
axisTransform axis value =
    axis.max_extent
        / (axis.max_value - axis.min_value)
        * (value
            - axis.min_value
          )


deleteAllFromInjection : Injection -> Injection
deleteAllFromInjection injection =
    { injection
        | co2_deleted = not injection.co2_deleted
        , n2o_deleted = not injection.n2o_deleted
        , ch4_deleted = not injection.ch4_deleted
    }


deleteAllPoints : Incubation -> Incubation
deleteAllPoints incubation =
    let
        newInjections =
            List.map deleteAllFromInjection incubation.injections
    in
        { incubation | injections = newInjections }


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
        , class "xAxis"
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
        , class "yAxis"
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
                    , class "regressionLine"
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
        draw_standard_graph my_dots (toString gas) points flux


draw_injections : Gas -> Maybe Flux -> Incubation -> List Point -> Svg Msg
draw_injections gas flux incubation points =
    let
        my_dots =
            injectionDots gas incubation points
    in
        draw_graph my_dots (toString gas) points flux


renderStandardValue : String -> Maybe Flux -> Svg Msg
renderStandardValue label flux =
    case flux of
        Just flux ->
            g []
                [ text_ [ x "10", y "20" ] [ Svg.text label ]
                , text_ [ x "10", y "40" ]
                    [ Svg.text
                        (Round.round 10
                            flux.slope
                        )
                    ]
                ]

        Nothing ->
            g [] []


renderFluxEq : String -> Maybe Flux -> Svg Msg
renderFluxEq label flux =
    case flux of
        Just flux ->
            let
                eq =
                    String.concat
                        [ "y = "
                        , Round.round 4 flux.intercept
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


draw_standard_graph : List (Svg Msg) -> String -> List Point -> Maybe Flux -> Svg Msg
draw_standard_graph drawing_func label points flux =
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
                [ g []
                    [ drawXAxis xAxis yAxis
                    , drawYAxis xAxis yAxis
                    ]
                , renderStandardValue label flux
                , g []
                    drawing_func
                ]
            ]


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
                    , stroke "blue"
                    , fill "blue"
                    , onClick msg
                    ]
                    []

            True ->
                g []
                    [ circle
                        [ cx (toString (xAxis_transform point.x))
                        , cy (toString (yAxis.max_extent - yAxis_transform point.y))
                        , r "5"
                        , stroke "grey"
                        , class "deleted"
                        , fill "none"
                        ]
                        []
                    , circle
                        [ cx (toString (xAxis.max_extent + 20))
                        , cy (toString (yAxis.max_extent - yAxis_transform point.y))
                        , r "5"
                        , stroke "grey"
                        , fill "grey"
                        , class "deleted"
                        , onClick msg
                        ]
                        []
                    ]


toXAxis : List Point -> Axis
toXAxis points =
    Axis 0 100 (minX points) (maxX points)


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
        [ div []
            [ Html.text incubation.chamber
            , Html.text " - "
            , Html.text (DE.toFormattedString "MMMM ddd, y" incubation.sampled_at)
            , Html.text " - "
            , Html.text (toString incubation.id)
            , label []
                [ input
                    [ class "deletePoint"
                    , type_ "checkbox"
                    , onClick (DeleteAllPoints incubation)
                    ]
                    []
                , Html.text "Bad Incubation"
                ]
            ]
        , div []
            [ draw_injections N2O incubation.n2o_flux incubation (n2o_injections incubation.injections)
            , draw_injections CO2 incubation.co2_flux incubation (co2_injections incubation.injections)
            , draw_injections CH4 incubation.ch4_flux incubation (ch4_injections incubation.injections)
            ]
        ]


orderedIncubation : Incubation -> Incubation -> Order
orderedIncubation a b =
    Basics.compare a.id b.id


showSavingIndicator : Model -> Html Msg
showSavingIndicator model =
    if model.saving then
        div [] [ Html.text "Saving run" ]
    else
        div [] []


nextRunButton : Model -> Html Msg
nextRunButton model =
    if List.isEmpty model.next_runs then
        button [ class "disabled" ]
            [ Html.text "Next" ]
    else
        button [ onClick NextRun ]
            [ Html.text "Next" ]


prevRunButton : Model -> Html Msg
prevRunButton model =
    if List.isEmpty model.previous_runs then
        button [ class "disabled" ]
            [ Html.text "Prev" ]
    else
        button [ onClick PrevRun ]
            [ Html.text "Prev" ]


drawNextPrevRun : Model -> Html Msg
drawNextPrevRun model =
    div []
        [ prevRunButton model
        , Html.a
            [ HA.href
                ("https://fluxprep.kbs.msu.edu/runs/"
                    ++ (toString
                            model.run.id
                       )
                )
            , HA.target
                "_blank"
            ]
            [ Html.text (toString model.run.id) ]
        , nextRunButton model
        , Html.text model.run.setup_file
        , button
            [ onClick (FluxGood model.run) ]
            [ Html.text "Good" ]
        , button
            [ onClick (FluxMaybeGood model.run) ]
            [ Html.text "Maybe" ]
        ]


renderError : Maybe String -> Html Msg
renderError error =
    case error of
        Just msg ->
            div [ class "error" ]
                [ Html.text msg ]

        Nothing ->
            div [] []


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick SaveRun ]
            [ Html.text "Save" ]
        , drawNextPrevRun model
        , showSavingIndicator model
        , div []
            [ draw_standards N2O model.run.n2o_calibration (n2o_standards model.run.standards)
            , draw_standards CO2 model.run.co2_calibration (co2_standards model.run.standards)
            , draw_standards CH4 model.run.ch4_calibration (ch4_standards model.run.standards)
            ]
        , renderError model.error
        , div []
            (model.run.incubations
                |> List.sortWith orderedIncubation
                |> List.map renderIncubation
            )
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
    let
        _ =
            Debug.log "fetching run" run.id
    in
        Http.get (runUrl run.id) runResponseDecoder
            |> Http.send LoadRun


runSaved : Result Http.Error () -> Msg
runSaved result =
    RunSaved result


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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SwitchInjection CO2 incubation point ->
            let
                _ =
                    Debug.log "switching CO2" incubation

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

        FluxGood incubation ->
            ( { model | status = Good }, Cmd.none )

        FluxMaybeGood incubation ->
            ( { model | status = MaybeGood }, Cmd.none )

        FluxBad incubation ->
            ( { model | status = Bad }, Cmd.none )

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

        LoadRun (Result.Err msg) ->
            let
                _ =
                    Debug.log "ERROR " msg
            in
                ( { model | error = (Just (toString msg)) }, Cmd.none )

        LoadRunIds (Ok runs) ->
            let
                current_run =
                    Maybe.withDefault initialRun (List.head runs)

                next_runs =
                    Maybe.withDefault [] (List.tail runs)
            in
                ( { model | run = current_run, next_runs = next_runs }
                , (fetchRun current_run)
                )

        LoadRunIds (Err msg) ->
            let
                _ =
                    Debug.log "Error" msg
            in
                ( model, Cmd.none )

        PrevRun ->
            let
                next_runs =
                    model.run :: model.next_runs

                run =
                    Maybe.withDefault initialRun (List.head model.previous_runs)

                previous_runs =
                    Maybe.withDefault [] (List.tail model.previous_runs)

                -- TODO: clean up older prev runs to reduce memory usage
            in
                ( { model | run = run, previous_runs = previous_runs, next_runs = next_runs }, fetchRun run )

        NextRun ->
            let
                previous_runs =
                    model.run :: model.previous_runs

                run =
                    Maybe.withDefault initialRun (List.head model.next_runs)

                next_runs =
                    Maybe.withDefault [] (List.tail model.next_runs)
            in
                ( { model | run = run, previous_runs = previous_runs, next_runs = next_runs }, fetchRun run )

        DeleteAllPoints incubation ->
            let
                newIncubation =
                    deleteAllPoints incubation

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

        NoOp ->
            ( model, Cmd.none )


init : ( Model, Cmd Msg )
init =
    ( initialModel, fetchRunIds )
