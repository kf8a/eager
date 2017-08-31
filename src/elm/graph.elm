port module Graph exposing (..)

import Date.Extra as DE exposing (..)
import Html
import Html exposing (..)
import Html exposing (program, Html)
import Html.Attributes as HA exposing (style, href, target, src)
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
import Authentication
import Auth0


type Msg
    = SwitchInjection Gas Incubation Point
    | SwitchStandard Gas Point
    | LoadRun (Result Http.Error Run)
    | PreLoadRun (Result Http.Error Run)
    | RunSaved (Result Http.Error ())
    | LoadRunIds (Result Http.Error (List Run))
    | PrevRun
    | NextRun
    | FluxGood Run
    | FluxMaybeGood Run
    | FluxBad Run
    | DeleteAllPoints Incubation
      -- | SavedIncubation (Result Http.Error Incubation)
    | AuthenticationMsg Authentication.Msg


initialModel : Maybe Auth0.LoggedInUser -> Model
initialModel initialUser =
    { run = initialRun
    , saving = False
    , error = Nothing
    , previous_runs = []
    , next_runs = []
    , authModel = (Authentication.init auth0showLock auth0logout initialUser)
    }


init : Maybe Auth0.LoggedInUser -> ( Model, Cmd Msg )
init initialUser =
    ( (initialModel initialUser)
    , fetchRunIds
        (Maybe.withDefault
            ""
            ((Authentication.tryGetToken
                (initialModel initialUser).authModel
             )
            )
        )
    )



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
        [ line
            [ x1 (toString xAxis.min_extent)
            , y1 (toString yAxis.min_extent)
            , x2 (toString xAxis.min_extent)
            , y2 (toString (yAxis.min_extent + 5))
            ]
            []
        ]


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
    Axis 0 150 (minX points) (maxX points)


toYAxis : List Point -> Axis
toYAxis points =
    Axis 0 150 0 (maxY points)


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
    span [ HA.style [ ( "display", "inline-block" ) ] ]
        [ div []
            [ Html.text "Chamber "
            , Html.text incubation.chamber
            , Html.text " - "
            , Html.a
                [ href
                    ("https://fluxprep.kbs.msu.edu/incubations/"
                        ++ (toString incubation.id)
                    )
                ]
                [ Html.text (DE.toFormattedString "MMMM ddd, y HH:MM" incubation.sampled_at)
                ]
            , Html.text " - "
            , Html.text "mv -"
            , button [ onClick (DeleteAllPoints incubation) ] [ Html.text "Delete" ]
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
        div [ class "alert alert-info" ] [ Html.text "Saving run" ]
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
        , button
            [ onClick (FluxBad model.run) ]
            [ Html.text "Bad" ]
        ]


renderError : Maybe String -> Html Msg
renderError error =
    case error of
        Just msg ->
            div [ class "alert-warning" ]
                [ Html.text msg ]

        Nothing ->
            div [] []


view : Model -> Html Msg
view model =
    div []
        [ drawNextPrevRun model
        , showSavingIndicator model
        , div [ class "text-right" ] [ Html.text (toString model.run.status) ]
        , Html.textarea [ HA.cols 40 ] []
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
        , div []
            (case Authentication.tryGetUserProfile model.authModel of
                Nothing ->
                    [ p [] [ Html.text "Please log in" ] ]

                Just user ->
                    [ p [] [ Html.text ("Hello, " ++ user.name ++ "!") ] ]
            )
        , p []
            [ button
                [ class "btn btn-primary"
                , onClick
                    (AuthenticationMsg
                        (if Authentication.isLoggedIn model.authModel then
                            Authentication.LogOut
                         else
                            Authentication.ShowLogIn
                        )
                    )
                ]
                [ Html.text
                    (if Authentication.isLoggedIn model.authModel then
                        "Logout"
                     else
                        "Login"
                    )
                ]
            ]
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


runIdRequest : String -> Http.Request (List Run)
runIdRequest token =
    Http.request
        { method = "GET"
        , headers = [ Http.header "Authorization" ("Bearer " ++ token) ]
        , url = runIdUrl
        , body = Http.emptyBody
        , expect = Http.expectJson runIdResponseDecoder
        , timeout = Nothing
        , withCredentials = False
        }


fetchRunIds : String -> Cmd Msg
fetchRunIds token =
    Http.send LoadRunIds (runIdRequest token)


runRequest : String -> Int -> Http.Request Run
runRequest token id =
    Http.request
        { method = "GET"
        , headers = [ Http.header "Authorization" ("Bearer " ++ token) ]
        , url = runUrl id
        , body = Http.emptyBody
        , expect = Http.expectJson runResponseDecoder
        , timeout = Nothing
        , withCredentials = False
        }


fetchRun : String -> Run -> Cmd Msg
fetchRun token run =
    runRequest token run.id
        |> Http.send LoadRun


fetchNextRun : Model -> Cmd Msg
fetchNextRun model =
    let
        run =
            List.head model.next_runs
    in
        case run of
            Just run ->
                case Authentication.tryGetToken model.authModel of
                    Just token ->
                        Http.send PreLoadRun (runRequest token run.id)

                    Nothing ->
                        let
                            _ =
                                Debug.log "not logged in"
                        in
                            Cmd.none

            Nothing ->
                Cmd.none


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


nextRun : Model -> Model
nextRun model =
    let
        previous_runs =
            model.run :: model.previous_runs

        run =
            Maybe.withDefault initialRun (List.head model.next_runs)

        next_runs =
            Maybe.withDefault [] (List.tail model.next_runs)
    in
        { model
            | run = run
            , previous_runs = previous_runs
            , next_runs = next_runs
        }


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

        -- TODO: see if the FluxGood/Bad/Maybe can be collapsed by passing the
        -- msg
        FluxGood run ->
            let
                newRun =
                    { run | status = Good }

                newModel =
                    { model | run = newRun }
            in
                ( { newModel | saving = True }, saveRun newModel.run )

        FluxMaybeGood run ->
            let
                newRun =
                    { run | status = MaybeGood }

                newModel =
                    { model | run = newRun }
            in
                ( { newModel | saving = True }, saveRun newModel.run )

        FluxBad run ->
            let
                newRun =
                    { run | status = Bad }

                newModel =
                    { model | run = newRun }
            in
                ( { newModel | saving = True }, saveRun newModel.run )

        RunSaved (Ok run) ->
            let
                newModel =
                    nextRun model
            in
                ( { newModel | saving = False }, fetchNextRun newModel )

        RunSaved (Err msg) ->
            ( model, Cmd.none )

        LoadRun (Ok run) ->
            let
                updatedRun =
                    calibrateRun run
            in
                ( { model | run = updatedRun }, fetchNextRun model )

        LoadRun (Err msg) ->
            let
                _ =
                    Debug.log "ERROR " msg
            in
                ( { model | error = (Just (toString msg)) }, Cmd.none )

        PreLoadRun (Ok run) ->
            let
                tail =
                    Maybe.withDefault [] (List.tail model.next_runs)

                updatedRun =
                    calibrateRun run

                newRuns =
                    [ updatedRun ] ++ tail
            in
                ( { model | next_runs = newRuns }, Cmd.none )

        PreLoadRun (Err msg) ->
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
                case Authentication.tryGetToken model.authModel of
                    Just token ->
                        ( { model | run = current_run, next_runs = next_runs }
                        , fetchRun token current_run
                        )

                    Nothing ->
                        ( model, Cmd.none )

        -- TODO: Log out when the server returns unauthorized, so that
        -- we can display the login box again
        -- LoadRunIds (Err (Http.BadStatus response)) ->
        --     let
        --         _ =
        --             Debug.log "Bad Status" response.status
        --
        --         cmd =
        --             if response.status.code == 401 then
        --                 Authentication.LogOut
        --             else
        --                 Cmd.none
        --     in
        --         ( model, cmd )
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
                case Authentication.tryGetToken model.authModel of
                    Just token ->
                        ( { model
                            | run = run
                            , previous_runs = previous_runs
                            , next_runs =
                                next_runs
                          }
                        , fetchRun token run
                        )

                    Nothing ->
                        ( model, Cmd.none )

        NextRun ->
            let
                newModel =
                    nextRun model
            in
                ( newModel, fetchNextRun newModel )

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

        AuthenticationMsg authMsg ->
            let
                ( authModel, cmd ) =
                    Authentication.update authMsg model.authModel

                -- We should have a token at this point
                token =
                    Maybe.withDefault "" (Authentication.tryGetToken authModel)
            in
                ( { model | authModel = authModel }
                , Cmd.batch
                    [ Cmd.map AuthenticationMsg cmd
                    , fetchRunIds token
                    ]
                )



-- Ports


port auth0showLock : Auth0.Options -> Cmd msg


port auth0authResult : (Auth0.RawAuthenticationResult -> msg) -> Sub msg


port auth0logout : () -> Cmd msg


subscriptions : a -> Sub Msg
subscriptions model =
    auth0authResult (Authentication.handleAuthResult >> AuthenticationMsg)
