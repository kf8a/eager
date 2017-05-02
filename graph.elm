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
    { slope : Float
    , intercept : Float
    , r2 : Float
    }


type alias Incubation =
    { co2_flux : Flux
    , ch4_flux : Flux
    , n2o_flux : Flux
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
    , co2_deleted : Bool
    , n2o_deleted : Bool
    , ch4_deleted : Bool
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
    = SwitchInjection Gas Point
    | SwitchStandard Gas Point
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



-- Point extractors


co2_standards : List Standard -> List Point
co2_standards standards =
    List.map (\x -> Point x.co2_ppm x.co2_mv x.co2_deleted x.id) standards


n2o_standards : List Standard -> List Point
n2o_standards standards =
    List.map (\x -> Point x.n2o_ppm x.n2o_mv x.n2o_deleted x.id) standards


ch4_standards : List Standard -> List Point
ch4_standards standards =
    List.map (\x -> Point x.ch4_ppm x.ch4_mv x.ch4_deleted x.id) standards


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



-- updaters


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


updateCO2Injection : Injection -> Point -> Injection
updateCO2Injection injection co2 =
    if co2.id == injection.id then
        { injection | co2_ppm = co2.y, co2_deleted = co2.deleted }
    else
        let
            -- TODO: Log this to the server side
            msg =
                String.concat [ "ERROR: ", toString co2, " did not match any id in " ]

            _ =
                Debug.log msg injection
        in
            injection


updateN2OInjection : Injection -> Point -> Injection
updateN2OInjection injection n2o =
    if n2o.id == injection.id then
        { injection | n2o_ppm = n2o.y, n2o_deleted = n2o.deleted }
    else
        let
            -- TODO: Log this to the server side
            msg =
                String.concat [ "ERROR: ", toString n2o, " did not match any id in " ]

            _ =
                Debug.log msg injection
        in
            injection


updateCH4Injection : Injection -> Point -> Injection
updateCH4Injection injection ch4 =
    if ch4.id == injection.id then
        { injection | ch4_ppm = ch4.y, ch4_deleted = ch4.deleted }
    else
        let
            -- TODO: Log this to the server side
            msg =
                String.concat [ "ERROR: ", toString ch4, " did not match any id in " ]

            _ =
                Debug.log msg injection
        in
            injection


update_co2_injections : List Injection -> Point -> List Injection
update_co2_injections injections co2 =
    let
        ( injection, rest ) =
            List.partition (\x -> x.id == co2.id) injections

        newInjection =
            case (List.head injection) of
                Just myInjection ->
                    [ updateCO2Injection myInjection co2 ]

                Nothing ->
                    []
    in
        rest ++ newInjection


update_ch4_injections : List Injection -> Point -> List Injection
update_ch4_injections injections ch4 =
    let
        ( injection, rest ) =
            List.partition (\x -> x.id == ch4.id) injections

        newInjection =
            case (List.head injection) of
                Just myInjection ->
                    [ updateCH4Injection myInjection ch4 ]

                Nothing ->
                    []
    in
        rest ++ newInjection


update_n2o_injections : List Injection -> Point -> List Injection
update_n2o_injections injections n2o =
    let
        ( injection, rest ) =
            List.partition (\x -> x.id == n2o.id) injections

        newInjection =
            case (List.head injection) of
                Just myInjection ->
                    [ updateN2OInjection myInjection n2o ]

                Nothing ->
                    []
    in
        rest ++ newInjection



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
        |> hardcoded False
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


x_offset : Float
x_offset =
    20.0


y_offset : Float
y_offset =
    0.0


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
            , y2 (toString (yAxis.max_extent - (yAxisTransform (flux.slope * xAxis.max_value))))
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
        draw_graph my_dots points flux


draw_injections : Gas -> List Point -> Svg Msg
draw_injections gas points =
    let
        fit =
            fitLineByLeastSquares points

        flux =
            fluxWithDefault fit

        my_dots =
            injectionDots gas points
    in
        draw_graph my_dots points flux


draw_graph : List (Svg Msg) -> List Point -> Flux -> Svg Msg
draw_graph drawing_func points flux =
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


injectionDots : Gas -> List Point -> List (Svg Msg)
injectionDots gas points =
    let
        dotTransform =
            dot (toXAxis points) (toYAxis points)
    in
        List.map (\x -> dotTransform (SwitchInjection gas x) x) points


renderListElement : Point -> Html Msg
renderListElement point =
    let
        msg =
            String.concat
                [ "id = "
                , toString point.id
                , " x = "
                , toString point.x
                , " y = "
                , toString point.y
                ]
    in
        li [] [ Html.text msg ]


renderList : List Point -> Html Msg
renderList points =
    ul []
        (List.map (\x -> renderListElement x) points)


view : Model -> Html Msg
view model =
    div []
        [ div []
            [ draw_standards N2O (n2o_standards model.incubation.standards)
            , draw_standards CO2 (co2_standards model.incubation.standards)
            , draw_standards CH4 (ch4_standards model.incubation.standards)
            ]
        , div []
            [ draw_injections N2O (n2o_injections model.incubation.injections)
            , draw_injections CO2 (co2_injections model.incubation.injections)
            , draw_injections CH4 (ch4_injections model.incubation.injections)
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


updateStandard : Incubation -> (List Standard -> Point -> List Standard) -> Point -> Incubation
updateStandard incubation updater point =
    let
        new_point =
            { point | deleted = not point.deleted }

        new_standards =
            updater incubation.standards new_point

        new_incubation =
            { incubation | standards = new_standards }
    in
        new_incubation


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SwitchInjection CO2 point ->
            let
                updated_incubation =
                    updateIncubation model.incubation (update_co2_injections) point

                new_flux =
                    toFit (co2_injections updated_incubation.injections)

                new_incubation =
                    { updated_incubation | co2_flux = new_flux }
            in
                ( { model | incubation = new_incubation }, Cmd.none )

        SwitchInjection CH4 point ->
            let
                updated_incubation =
                    updateIncubation model.incubation (update_ch4_injections) point

                new_flux =
                    toFit (ch4_injections updated_incubation.injections)

                new_incubation =
                    { updated_incubation | ch4_flux = new_flux }
            in
                ( { model | incubation = new_incubation }, Cmd.none )

        SwitchInjection N2O point ->
            let
                updated_incubation =
                    updateIncubation model.incubation (update_n2o_injections) point

                new_flux =
                    toFit (n2o_injections updated_incubation.injections)

                new_incubation =
                    { updated_incubation | n2o_flux = new_flux }
            in
                ( { model | incubation = new_incubation }, Cmd.none )

        SwitchStandard CO2 point ->
            let
                updatedIncubation =
                    updateStandard model.incubation (updateCO2Standards) point

                new_flux =
                    toFit (co2_standards updatedIncubation.standards)

                newIncubation =
                    { updatedIncubation | co2_calibration = new_flux }
            in
                ( { model | incubation = newIncubation }, Cmd.none )

        SwitchStandard CH4 point ->
            let
                updatedIncubation =
                    updateStandard model.incubation (updateCH4Standards) point

                new_flux =
                    toFit (ch4_standards updatedIncubation.standards)

                newIncubation =
                    { updatedIncubation | ch4_calibration = new_flux }
            in
                ( { model | incubation = newIncubation }, Cmd.none )

        SwitchStandard N2O point ->
            let
                updatedIncubation =
                    updateStandard model.incubation (updateN2OStandards) point

                new_flux =
                    toFit (n2o_standards updatedIncubation.standards)

                newIncubation =
                    { updatedIncubation | n2o_calibration = new_flux }
            in
                ( { model | incubation = newIncubation }, Cmd.none )

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
