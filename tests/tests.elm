module Tests exposing (..)

import Test exposing (..)
import Fuzz exposing (..)
import Expect exposing (Expectation)
import Json.Decode exposing (decodeString, Value)
import String
import Graph exposing (..)
import LeastSquares exposing (..)


standard1 : Standard
standard1 =
    Standard 1 2 3 4 5 6 1


standard2 : Standard
standard2 =
    Standard 1 2 3 4 5 6 2


all : Test
all =
    describe "the graph module"
        [ describe "transform from standards to point"
            [ test "it extracts a CO2 point" <|
                \() ->
                    standardToCO2Point standard1
                        |> Expect.equal (Point 3 4 True 1)
            , test "it extracts a N2O point" <|
                \() ->
                    standardToN2OPoint standard1
                        |> Expect.equal (Point 1 2 True 1)
            , test "it extracts a CH4 point" <|
                \() ->
                    standardToCH4Point standard1
                        |> Expect.equal (Point 5 6 True 1)
            ]
        , describe "transform from standards to point list"
            []
        , describe "transform from incubation to standard"
            [ test "update standard from point with matching id" <|
                \() ->
                    let
                        n2oPoint =
                            Point 8 9 True 1
                    in
                        updateN2OStandard standard1 n2oPoint
                            |> Expect.equal (Standard 8 9 3 4 5 6 1)
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
                            |> Expect.equal [ standard2, (Standard 8 9 3 4 5 6 1) ]
            ]
        , describe "transform from injection to point"
            []
        , describe "transfrom from point to injection"
            []
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
            , fuzz float "it retuns a value in the extent range" <|
                \value ->
                    let
                        axis =
                            Axis 0 60 0 100

                        output =
                            axisTransform axis value
                    in
                        ((output >= 0) && (output <= 60))
                            |> Expect.true "Expected result be within extent range"
            , fuzz (floatRange 101 1000) "it retuns the upper range if above range" <|
                \value ->
                    let
                        axis =
                            Axis 0 60 0 100
                    in
                        axisTransform axis value
                            |> Expect.equal 60
            , fuzz (floatRange -101 0) "it retuns the lower range if below range" <|
                \value ->
                    let
                        axis =
                            Axis 0 60 0 100
                    in
                        axisTransform axis value
                            |> Expect.equal 0
            ]
        , describe "the standard filters to points"
            [ test "it grabs the right co2 parameters" <|
                \() ->
                    let
                        standards =
                            [ Standard 0.4 100 500 1000 2 50 0 ]
                    in
                        standards
                            |> co2_standards
                            |> Expect.equal [ Point 500 1000 False 0 ]
            , test "it grabs the right n2o parameters" <|
                \() ->
                    let
                        standards =
                            [ Standard 0.4 100 500 1000 2 50 0 ]
                    in
                        standards
                            |> n2o_standards
                            |> Expect.equal [ Point 0.4 100 False 0 ]
            , test "it grabs the right ch4 parameters" <|
                \() ->
                    let
                        standards =
                            [ Standard 0.4 100 500 1000 2 50 0 ]
                    in
                        standards
                            |> ch4_standards
                            |> Expect.equal [ Point 2 50 False 0 ]
            ]
        , describe "standards decoder"
            [ test "it parses correct json" <|
                \() ->
                    let
                        json =
                            """ {"data":{"standards":[{"vial":"STD00A","n2o_ppm":0.0,"n2o_mv":0.0,"id":2,"co2_ppm":0.0,"co2_mv":0.0,"ch4_ppm":0.0,"ch4_mv":0.0},{"vial":"STD00C","n2o_ppm":0.0,"n2o_mv":0.0,"id":3,"co2_ppm":0.0,"co2_mv":0.0,"ch4_ppm":0.0,"ch4_mv":0.0},{"vial":"STD07A","n2o_ppm":0.294,"n2o_mv":805.512817,"id":4,"co2_ppm":350.423,"co2_mv":126110.507813,"ch4_ppm":0.565,"ch4_mv":14.578513},{"vial":"STD07C","n2o_ppm":0.294,"n2o_mv":520.3573,"id":5,"co2_ppm":350.423,"co2_mv":83075.828125,"ch4_ppm":0.565,"ch4_mv":14.634157},{"vial":"STD10A","n2o_ppm":0.42,"n2o_mv":1088.805908,"id":6,"co2_ppm":500.605,"co2_mv":174609.40625,"ch4_ppm":0.806,"ch4_mv":17.025524},{"vial":"STD10C","n2o_ppm":0.42,"n2o_mv":1101.84082,"id":7,"co2_ppm":500.605,"co2_mv":174714.1875,"ch4_ppm":0.806,"ch4_mv":17.392969},{"vial":"STD15A","n2o_ppm":0.629,"n2o_mv":1477.518433,"id":8,"co2_ppm":750.907,"co2_mv":244768.640625,"ch4_ppm":1.21,"ch4_mv":19.894928},{"vial":"STD15C","n2o_ppm":0.629,"n2o_mv":1530.701538,"id":9,"co2_ppm":750.907,"co2_mv":252542.640625,"ch4_ppm":1.21,"ch4_mv":20.586039},{"vial":"STD20A","n2o_ppm":0.839,"n2o_mv":1799.6604,"id":10,"co2_ppm":1001.201,"co2_mv":308658.6875,"ch4_ppm":1.613,"ch4_mv":22.114155},{"vial":"STD20C","n2o_ppm":0.839,"n2o_mv":1875.328613,"id":11,"co2_ppm":1001.201,"co2_mv":317967.34375,"ch4_ppm":1.613,"ch4_mv":22.941065},{"vial":"STD30A","n2o_ppm":1.259,"n2o_mv":2664.494385,"id":12,"co2_ppm":1501.815,"co2_mv":482659.84375,"ch4_ppm":2.419,"ch4_mv":29.487915},{"vial":"STD30C","n2o_ppm":1.259,"n2o_mv":2628.794189,"id":13,"co2_ppm":1501.815,"co2_mv":472068.21875,"ch4_ppm":2.419,"ch4_mv":29.351187},{"vial":"STD40A","n2o_ppm":1.678,"n2o_mv":3331.138428,"id":14,"co2_ppm":2002.419,"co2_mv":632893.75,"ch4_ppm":3.226,"ch4_mv":34.833195},{"vial":"STD40C","n2o_ppm":1.678,"n2o_mv":3426.725586,"id":15,"co2_ppm":2002.419,"co2_mv":639703.9375,"ch4_ppm":3.226,"ch4_mv":35.634666},{"vial":"STD00B","n2o_ppm":0.0,"n2o_mv":0.0,"id":200,"co2_ppm":0.0,"co2_mv":0.0,"ch4_ppm":0.0,"ch4_mv":14.152516},{"vial":"STD00D","n2o_ppm":0.0,"n2o_mv":0.0,"id":201,"co2_ppm":0.0,"co2_mv":0.0,"ch4_ppm":0.0,"ch4_mv":14.678946},{"vial":"STD07B","n2o_ppm":0.294,"n2o_mv":926.001648,"id":202,"co2_ppm":350.423,"co2_mv":127831.578125,"ch4_ppm":0.565,"ch4_mv":19.136793},{"vial":"STD07D","n2o_ppm":0.294,"n2o_mv":946.895203,"id":203,"co2_ppm":350.423,"co2_mv":130525.875,"ch4_ppm":0.565,"ch4_mv":19.337883},{"vial":"STD10B","n2o_ppm":0.42,"n2o_mv":1192.180054,"id":204,"co2_ppm":500.605,"co2_mv":172341.5625,"ch4_ppm":0.806,"ch4_mv":21.219637},{"vial":"STD10D","n2o_ppm":0.42,"n2o_mv":1218.864502,"id":205,"co2_ppm":500.605,"co2_mv":173176.21875,"ch4_ppm":0.806,"ch4_mv":21.118393},{"vial":"STD15B","n2o_ppm":0.629,"n2o_mv":1610.550415,"id":206,"co2_ppm":750.907,"co2_mv":243974.625,"ch4_ppm":1.21,"ch4_mv":23.927797},{"vial":"STD15D","n2o_ppm":0.629,"n2o_mv":1672.735229,"id":207,"co2_ppm":750.907,"co2_mv":253287.4375,"ch4_ppm":1.21,"ch4_mv":24.113028},{"vial":"STD20B","n2o_ppm":0.839,"n2o_mv":2015.37561,"id":208,"co2_ppm":1001.201,"co2_mv":318978.78125,"ch4_ppm":1.613,"ch4_mv":27.295242},{"vial":"STD20D","n2o_ppm":0.839,"n2o_mv":2082.026855,"id":209,"co2_ppm":1001.201,"co2_mv":323814.15625,"ch4_ppm":1.613,"ch4_mv":26.947424},{"vial":"STD30B","n2o_ppm":1.259,"n2o_mv":2859.93042,"id":210,"co2_ppm":1501.815,"co2_mv":486949.5,"ch4_ppm":2.419,"ch4_mv":32.337463},{"vial":"STD30D","n2o_ppm":1.259,"n2o_mv":2819.568604,"id":211,"co2_ppm":1501.815,"co2_mv":468271.625,"ch4_ppm":2.419,"ch4_mv":31.783016},{"vial":"STD40B","n2o_ppm":1.678,"n2o_mv":3621.908203,"id":212,"co2_ppm":2002.419,"co2_mv":637937.6875,"ch4_ppm":3.226,"ch4_mv":36.706039},{"vial":"STD40D","n2o_ppm":1.678,"n2o_mv":3664.023682,"id":213,"co2_ppm":2002.419,"co2_mv":633270.9375,"ch4_ppm":3.226,"ch4_mv":37.041862}],"slope":null,"r2":null,"intercept":null,"id":1,"file":"EAGER1.CSV"}}
"""
                    in
                        json
                            |> decodeStandards
                            |> List.length
                            |> Expect.equal 28
            ]
        ]
