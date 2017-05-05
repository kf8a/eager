module Tests exposing (..)

import Test exposing (..)
import Fuzz exposing (..)
import Expect exposing (Expectation)
import Json.Decode exposing (decodeString, Value)
import Json.Encode exposing (encode)
import String
import Graph exposing (..)
import LeastSquares exposing (..)
import Date exposing (Month(..))
import Date.Extra as DE exposing (..)
import Data exposing (..)
import TestJson exposing (..)


standard1 : Standard
standard1 =
    Standard 1 2 3 4 5 6 False False False 1


standard2 : Standard
standard2 =
    Standard 1 2 3 4 5 6 False False False 2


injection1 : Injection
injection1 =
    let
        date =
            DE.fromParts 2017 May 1 12 50 0 0
    in
        Injection 500 0.3 2 0 0 0 1 False False False date


injection2 : Injection
injection2 =
    let
        date =
            DE.fromParts 2017 May 1 12 55 0 0
    in
        Injection 600 0.6 1 0 0 0 2 False False False date


all : Test
all =
    describe "the graph module"
        [ describe "transform from standards to point list"
            [ test "extract co2 point list" <|
                \() ->
                    co2_standards [ standard1, standard2 ]
                        |> Expect.equal [ (Point 4 3 False 1), (Point 4 3 False 2) ]
            , test "extract n2o point list" <|
                \() ->
                    n2o_standards [ standard1, standard2 ]
                        |> Expect.equal [ (Point 2 1 False 1), (Point 2 1 False 2) ]
            , test "extract ch4 point list" <|
                \() ->
                    ch4_standards [ standard1, standard2 ]
                        |> Expect.equal [ (Point 6 5 False 1), (Point 6 5 False 2) ]
            ]
        , describe "transform from incubation to standard"
            [ test "update standard from point with matching id" <|
                \() ->
                    let
                        n2oPoint =
                            Point 8 9 True 1
                    in
                        updateN2OStandard standard1 n2oPoint
                            |> Expect.equal (Standard 8 9 3 4 5 6 True False False 1)
            , test "update standard list from point" <|
                \() ->
                    let
                        n2oPoint =
                            Point 8 9 True 1

                        standards =
                            [ standard1, standard2 ]

                        updated =
                            updateN2OStandards standards n2oPoint
                    in
                        updated
                            |> Expect.equal [ standard2, (Standard 8 9 3 4 5 6 True False False 1) ]
            ]
        , describe "transform from injection to point list"
            [ test "extract c2o point list" <|
                \() ->
                    co2_injections [ injection1, injection2 ]
                        |> Expect.equal [ (Point 0 500 False 1), (Point 5 600 False 2) ]
            , test "extract n2o point list" <|
                \() ->
                    n2o_injections [ injection1, injection2 ]
                        |> Expect.equal [ (Point 0 0.3 False 1), (Point 5 0.6 False 2) ]
            , test "extract ch4 point list" <|
                \() ->
                    ch4_injections [ injection1, injection2 ]
                        |> Expect.equal [ (Point 0 2 False 1), (Point 5 1 False 2) ]
            , test "extract c2o point list reversed " <|
                \() ->
                    co2_injections [ injection2, injection1 ]
                        |> Expect.equal [ (Point 5 600 False 2), (Point 0 500 False 1) ]
            ]
        , describe "transfrom from point to injection"
            [ test "update injections from N2O point with matching id" <|
                \() ->
                    let
                        n2oPoint =
                            Point 8 9 False 1

                        date =
                            DE.fromParts 2017 May 1 12 50 0 0
                    in
                        updateN2OInjection injection1 n2oPoint
                            |> Expect.equal (Injection 500 9 2 0 0 0 1 False False False date)
            ]
        , describe "least squares fit"
            [ test "simple fit" <|
                \() ->
                    let
                        points =
                            [ Point 0 0 False 1
                            , Point 1 1 False 2
                            ]

                        result =
                            fitLineByLeastSquares points
                    in
                        case result of
                            Ok fit ->
                                fit.slope
                                    |> Expect.equal 1

                            Err msg ->
                                msg
                                    |> Expect.equal "error"
            , test "more complex fit" <|
                \() ->
                    let
                        points =
                            [ Point 0 0 False 1
                            , Point 10 6 False 2
                            , Point 10 4 False 3
                            ]

                        result =
                            fitLineByLeastSquares points
                    in
                        case result of
                            Ok fit ->
                                fit.slope
                                    |> Expect.equal 0.5

                            Err msg ->
                                msg
                                    |> Expect.equal "error"
            ]
        , describe "axis transform"
            [ fuzz (floatRange 0 100) "it scales the scaled values for in range values" <|
                \value ->
                    let
                        axis =
                            Axis 0 50 0 100
                    in
                        axisTransform axis value
                            |> Expect.equal (value / 2)
            , fuzz (floatRange 0 100) "it retuns a value in the extent range" <|
                \value ->
                    let
                        axis =
                            Axis 0 60 0 100

                        output =
                            axisTransform axis value
                    in
                        ((output >= 0) && (output <= 60))
                            |> Expect.true "Expected result be within extent range"
            ]

        -- , describe "injection encoder"
        --     [ test "it encodes an injection" <|
        --         \() ->
        --             let
        --                 result =
        --                     injectionEncoder injection1
        --                         |> encode 0
        --                         |> decodeInjection
        --             in
        --                 result
        --                     |> Expect.equal injection1
        --     ]
        , describe "injection decoder"
            [ test "it parses correct json" <|
                \() ->
                    injectionJson
                        |> decodeInjections
                        |> List.length
                        |> Expect.equal 4
            ]
        , describe "incubations decoder"
            [ test "it parses correct json for a single incubation" <|
                \() ->
                    let
                        incubation =
                            decodeIncubation incubationJson
                    in
                        incubation.injections
                            |> List.length
                            |> Expect.equal 4
            , test "it parses correct json for an incubation list" <|
                \() ->
                    incubationListJson
                        |> decodeIncubationList
                        |> List.length
                        |> Expect.equal 1
            ]
        , describe "runs decoder"
            [ test "it parses correct json" <|
                \() ->
                    let
                        result =
                            decodeRun runJson
                    in
                        result.setup_file
                            |> Expect.equal "EAGER1.CSV"
            , test "it parses simplified json" <|
                \() ->
                    let
                        result =
                            decodeRun simpleRunJson
                    in
                        result.setup_file
                            |> Expect.equal "EAGER1.CSV"
            ]
        , describe "standards decoder"
            [ test "it parses correct json" <|
                \() ->
                    standardJson
                        |> decodeStandards
                        |> List.length
                        |> Expect.equal 28
            ]
        ]
