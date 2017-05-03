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


type alias Incubation =
    { injections : List Injection
    , standards : List Standard
    , id : Int
    , co2_flux : Maybe Flux
    , ch4_flux : Maybe Flux
    , n2o_flux : Maybe Flux
    , co2_calibration : Maybe Flux
    , ch4_calibration : Maybe Flux
    , n2o_calibration : Maybe Flux
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


type Status
    = Good
    | Bad
    | MaybeGood
    | NotChecked



--- initial data


initialIncubation : Incubation
initialIncubation =
    { injections = []
    , standards = []
    , id = 0
    , co2_flux = Nothing
    , ch4_flux = Nothing
    , n2o_flux = Nothing
    , co2_calibration = Nothing
    , ch4_calibration = Nothing
    , n2o_calibration = Nothing
    }


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
        |> optional "standards" (JD.list standardDecoder) []
        |> required "id" JD.int
        |> optional "co2_flux" (JD.map Just fluxDecoder) Nothing
        |> optional "ch4_flux" (JD.map Just fluxDecoder) Nothing
        |> optional "n2o_flux" (JD.map Just fluxDecoder) Nothing
        |> optional "co2_calibration" (JD.map Just fluxDecoder) Nothing
        |> optional "ch4_calibration" (JD.map Just fluxDecoder) Nothing
        |> optional "n2o_calibration" (JD.map Just fluxDecoder) Nothing


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
                Injection 0 0 0 0 False False False date


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
        , ( "co2_deleted", JE.bool injection.co2_deleted )
        ]


incubationEncoder : Incubation -> JE.Value
incubationEncoder incubation =
    JE.object
        [ ( "injections", JE.list (List.map injectionEncoder incubation.injections) )
        , ( "id", JE.int incubation.id )
        ]


standardEncoder : Standard -> JE.Value
standardEncoder standard =
    JE.object
        [ ( "n2o_ppm", JE.float standard.n2o_ppm )
        , ( "n2o_mv", JE.float standard.n2o_mv )
        , ( "n2o_deleted", JE.bool standard.n2o_deleted )
        ]


standardListEncoder : List Standard -> JE.Value
standardListEncoder standardList =
    JE.object [ ( "standards", JE.list (List.map standardEncoder standardList) ) ]
