module Data exposing (..)

import Date exposing (..)
import Date.Extra as DE exposing (..)
import LeastSquares exposing (..)
import Time exposing (..)
import Json.Decode as JD exposing (..)
import Json.Decode.Pipeline exposing (decode, required, optional, hardcoded)
import Json.Encode as JE exposing (..)


type alias Flux =
    { slope : Float
    , intercept : Float
    , r2 : Float
    }


type alias Run =
    { id : Int
    , setup_file : String
    , data_file : List String
    , incubations : List Incubation
    , standards : List Standard
    , co2_calibration : Maybe Flux
    , ch4_calibration : Maybe Flux
    , n2o_calibration : Maybe Flux
    }


type alias Incubation =
    { injections : List Injection
    , id : Int
    , status : Status
    , co2_flux : Maybe Flux
    , ch4_flux : Maybe Flux
    , n2o_flux : Maybe Flux
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
    { run : Run
    , next_run : Maybe Run
    , status : Status
    }


type alias Injection =
    { co2_ppm : Float
    , n2o_ppm : Float
    , ch4_ppm : Float
    , co2_mv : Float
    , n2o_mv : Float
    , ch4_mv : Float
    , id : Int
    , co2_deleted : Bool
    , n2o_deleted : Bool
    , ch4_deleted : Bool
    , datetime : Date
    }


type Status
    = Good
    | Bad
    | MaybeGood
    | NotChecked



--- initial data


initialIncubation : Incubation
initialIncubation =
    { injections = []
    , id = 0
    , status = NotChecked
    , co2_flux = Nothing
    , ch4_flux = Nothing
    , n2o_flux = Nothing
    }


initialRun : Run
initialRun =
    Run 0 "nothing" [] [] [] Nothing Nothing Nothing


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


initialFlux : Flux
initialFlux =
    Flux 0 0 0


initialStandards : List Standard
initialStandards =
    [ initialStandard ]


sortedRecords : Injection -> Injection -> Order
sortedRecords a b =
    DE.compare a.datetime b.datetime



-- Point extractors


co2_standards : List Standard -> List Point
co2_standards standards =
    List.map (\x -> Point x.co2_mv x.co2_ppm x.co2_deleted x.id) standards


n2o_standards : List Standard -> List Point
n2o_standards standards =
    List.map (\x -> Point x.n2o_mv x.n2o_ppm x.n2o_deleted x.id) standards


ch4_standards : List Standard -> List Point
ch4_standards standards =
    List.map (\x -> Point x.ch4_mv x.ch4_ppm x.ch4_deleted x.id) standards
        |> List.filter (\x -> x.y /= 0.0)



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


fluxDecoder : Decoder Flux
fluxDecoder =
    decode Flux
        |> required "slope" JD.float
        |> required "intercept" JD.float
        |> required "r2" JD.float


incubationDecoder : Decoder Incubation
incubationDecoder =
    decode Incubation
        |> required "injections" (JD.list injectionDecoder)
        |> required "id" JD.int
        |> hardcoded NotChecked
        |> optional "co2_flux" (JD.map Just fluxDecoder) Nothing
        |> optional "ch4_flux" (JD.map Just fluxDecoder) Nothing
        |> optional "n2o_flux" (JD.map Just fluxDecoder) Nothing


responseIncubationDecoder : Decoder Incubation
responseIncubationDecoder =
    decode identity
        |> required "data" incubationDecoder


decodeIncubation : String -> Incubation
decodeIncubation json =
    case decodeString responseIncubationDecoder json of
        Ok incubation ->
            incubation

        Err msg ->
            initialIncubation


responseIncubationListDecoder : Decoder (List Incubation)
responseIncubationListDecoder =
    decode identity
        |> required "data" (JD.list incubationDecoder)


decodeIncubationList : String -> List Incubation
decodeIncubationList json =
    case decodeString responseIncubationListDecoder json of
        Ok listOfIncubations ->
            listOfIncubations

        Err msg ->
            let
                _ =
                    Debug.log "ERROR:" msg
            in
                []


injectionDecoder : Decoder Injection
injectionDecoder =
    decode Injection
        |> hardcoded 0
        |> hardcoded 0
        |> hardcoded 0
        |> required "co2" JD.float
        |> required "n2o" JD.float
        |> required "ch4" JD.float
        |> required "id" JD.int
        |> optional "n2o_deleted" JD.bool False
        |> optional "co2_deleted" JD.bool False
        |> optional "ch4_deleted" JD.bool False
        |> required "sampled_at" date


responseDecoder : Decoder (List Injection)
responseDecoder =
    decode identity
        |> required "injections" (JD.list injectionDecoder)


injectionDataDecoder : Decoder Injection
injectionDataDecoder =
    decode identity
        |> required "data" injectionDecoder


decodeInjection : String -> Injection
decodeInjection json =
    case decodeString injectionDataDecoder json of
        Ok injection ->
            injection

        Err msg ->
            let
                date =
                    Date.fromTime (Time.inSeconds 0)
            in
                Injection 0 0 0 0 0 0 0 False False False date


decodeInjections : String -> List Injection
decodeInjections json =
    case decodeString responseDecoder json of
        Ok listOfInjections ->
            listOfInjections

        Err msg ->
            let
                _ =
                    Debug.log "ERROR decoding injections:" msg
            in
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
        |> required "n2o_ppm" JD.float
        |> required "n2o_mv" JD.float
        |> required "co2_ppm" JD.float
        |> required "co2_mv" JD.float
        |> required "ch4_ppm" JD.float
        |> required "ch4_mv" JD.float
        |> hardcoded False
        |> hardcoded False
        |> hardcoded False
        |> required "id" JD.int


standardDataDecoder : Decoder (List Standard)
standardDataDecoder =
    decode identity
        |> required "standards" (JD.list standardDecoder)


standardResponseDecoder : Decoder (List Standard)
standardResponseDecoder =
    decode identity
        |> required "data" standardDataDecoder


runDecoder : Decoder Run
runDecoder =
    decode Run
        |> required "id" JD.int
        |> required "file" JD.string
        |> hardcoded []
        |> required "incubations" (JD.list incubationDecoder)
        |> required "standards" (JD.list standardDecoder)
        |> optional "co2_calibration" (JD.map Just fluxDecoder) Nothing
        |> optional "ch4_calibration" (JD.map Just fluxDecoder) Nothing
        |> optional "n2o_calibration" (JD.map Just fluxDecoder) Nothing


runResponseDecoder : Decoder Run
runResponseDecoder =
    decode identity
        |> required "data" runDecoder


decodeRun : String -> Run
decodeRun json =
    case decodeString runResponseDecoder json of
        Ok run ->
            run

        Err msg ->
            let
                _ =
                    Debug.log "ERROR parsing run" msg
            in
                Run 0 "nothing" [] [] [] Nothing Nothing Nothing



--- ENCODERS


dataInjectionEncoder : Injection -> JE.Value
dataInjectionEncoder injection =
    JE.object [ ( "data", (injectionEncoder injection) ) ]


injectionEncoder : Injection -> JE.Value
injectionEncoder injection =
    JE.object
        [ ( "co2", JE.float injection.co2_ppm )
        , ( "ch4", JE.float injection.ch4_ppm )
        , ( "n2o", JE.float injection.n2o_ppm )
        , ( "id", JE.int injection.id )
        , ( "co2_deleted", JE.bool injection.co2_deleted )
        , ( "n2o_deleted", JE.bool injection.n2o_deleted )
        , ( "ch4_deleted", JE.bool injection.co2_deleted )
        ]


incubationEncoder : Incubation -> JE.Value
incubationEncoder incubation =
    let
        n2o_flux =
            case incubation.n2o_flux of
                Just flux ->
                    JE.float flux.slope

                Nothing ->
                    JE.null

        co2_flux =
            case incubation.co2_flux of
                Just flux ->
                    JE.float flux.slope

                Nothing ->
                    JE.null

        ch4_flux =
            case incubation.ch4_flux of
                Just flux ->
                    JE.float flux.slope

                Nothing ->
                    JE.null
    in
        JE.object
            [ ( "injections", JE.list (List.map injectionEncoder incubation.injections) )
            , ( "id", JE.int incubation.id )
            , ( "n2o_flux", n2o_flux )
            , ( "co2_flux", co2_flux )
            , ( "ch4_flux", ch4_flux )
            ]


standardEncoder : Standard -> JE.Value
standardEncoder standard =
    JE.object
        [ ( "id", JE.int standard.id )
        , ( "n2o_deleted", JE.bool standard.n2o_deleted )
        , ( "co2_deleted", JE.bool standard.co2_deleted )
        , ( "ch4_deleted", JE.bool standard.ch4_deleted )
        ]


standardListEncoder : List Standard -> JE.Value
standardListEncoder standardList =
    JE.object [ ( "standards", JE.list (List.map standardEncoder standardList) ) ]


runEncoder : Run -> JE.Value
runEncoder run =
    JE.object
        [ ( "incubations", JE.list (List.map incubationEncoder run.incubations) )
        , ( "standards", JE.list (List.map standardEncoder run.standards) )
        , ( "id", JE.int run.id )
        ]



-- updaters


updateN2OStandard : Standard -> Point -> Standard
updateN2OStandard standard n2o =
    if n2o.id == standard.id then
        { standard | n2o_ppm = n2o.y, n2o_mv = n2o.x, n2o_deleted = n2o.deleted }
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
        { standard | co2_ppm = co2.y, co2_mv = co2.x, co2_deleted = co2.deleted }
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
        { standard | ch4_ppm = ch4.y, ch4_mv = ch4.x, ch4_deleted = ch4.deleted }
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
