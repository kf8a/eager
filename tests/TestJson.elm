module TestJson exposing (..)


simpleRunJson : String
simpleRunJson =
    """
  {
    "data" : {
    "n2o_slope": null,
    "n2o_r2": null,
    "n2o_intercept": null,
    "standards": [],
    "incubations": [],
    "id": 1,
    "file": "EAGER1.CSV",
    "co2_slope": null,
    "co2_r2": null,
    "co2_intercept": null,
    "ch4_slope": null,
    "ch4_r2": null,
    "ch4_intercept": null
  }
}
  """


runJson : String
runJson =
    """
{
  "data": {
    "n2o_slope": null,
    "n2o_r2": null,
    "n2o_intercept": null,
    "standards": [{
          "n2o_ppm": 0.0,
          "n2o_mv": 0.0,
          "id": 2,
          "co2_ppm": 0.0,
          "co2_mv": 0.0,
          "ch4_ppm": 0.0,
          "ch4_mv": 0.0
      }, {
          "n2o_ppm": 0.0,
          "n2o_mv": 0.0,
          "id": 3,
          "co2_ppm": 0.0,
          "co2_mv": 0.0,
          "ch4_ppm": 0.0,
          "ch4_mv": 0.0
      } ],
    "incubations": [{
      "sampled_at": "2016-05-11T13:30:00",
      "injections": [{
        "sampled_at": "2016-05-11T13:30:00",
        "n2o_deleted": null,
        "n2o": 965.684509,
        "id": 4,
        "co2_deleted": false,
        "co2": 150812.609375,
        "ch4_deleted": false,
        "ch4": 23.364407
        }, {
          "sampled_at": "2016-05-11T13:30:00",
          "n2o_deleted": null,
          "n2o": 963.038452,
          "id": 1,
          "co2_deleted": false,
          "co2": 143038.15625,
          "ch4_deleted": false,
          "ch4": 23.165073
        }, {
          "sampled_at": "2016-05-11T13:30:00",
          "n2o_deleted": null,
          "n2o": 969.535034,
          "id": 2,
          "co2_deleted": false,
          "co2": 150083.28125,
          "ch4_deleted": false,
          "ch4": 23.336943
        }, {
          "sampled_at": "2016-05-11T13:30:00",
          "n2o_deleted": null,
          "n2o": 975.268616,
          "id": 3,
          "co2_deleted": false,
          "co2": 147395.390625,
          "ch4_deleted": false,
          "ch4": 23.775814
        }],
        "id": 1,
        "height": 18.625
      }, {
        "sampled_at": "2016-05-11T13:30:00",
        "injections": [{
          "sampled_at": "2016-05-11T13:30:00",
          "n2o_deleted": null,
          "n2o": 1456.418457,
          "id": 8,
          "co2_deleted": false,
          "co2": 367425.40625,
          "ch4_deleted": false,
          "ch4": 23.658358
        }, {
          "sampled_at": "2016-05-11T13:30:00",
          "n2o_deleted": null,
          "n2o": 1030.071655,
          "id": 5,
          "co2_deleted": false,
          "co2": 171892.859375,
          "ch4_deleted": false,
          "ch4": 23.303488
        }, {
          "sampled_at": "2016-05-11T13:30:00",
          "n2o_deleted": null,
          "n2o": 1209.962646,
          "id": 6,
          "co2_deleted": false,
          "co2": 258458.40625,
          "ch4_deleted": false,
          "ch4": 23.390278
        }, {
          "sampled_at": "2016-05-11T13:30:00",
          "n2o_deleted": null,
          "n2o": 1352.095215,
          "id": 7,
          "co2_deleted": false,
          "co2": 302823.125,
          "ch4_deleted": false,
          "ch4": 23.206163
        }],
        "id": 2,
        "height": 19.375
      }],
      "id": 1,
      "file": "EAGER1.CSV",
      "co2_slope": null,
      "co2_r2": null,
      "co2_intercept": null,
      "ch4_slope": null,
      "ch4_r2": null,
      "ch4_intercept": null
  }
}
"""


incubationJson : String
incubationJson =
    """
      {
      "data": {
        "sampled_at": "2016-05-31T15:00:00",
        "injections": [{
          "sampled_at": "2016-05-31T15:00:00",
          "n2o_deleted": null,
          "n2o": 984.054749,
          "id": 109,
          "co2_deleted": false,
          "co2": 148110.140625,
          "ch4_deleted": false,
          "ch4": 21.499937
          }, {
            "sampled_at": "2016-05-31T15:00:00",
            "n2o_deleted": null,
            "n2o": 1187.261353,
            "id": 111,
            "co2_deleted": false,
            "co2": 419945.34375,
            "ch4_deleted": false,
            "ch4": 21.085075
          }, {
            "sampled_at": "2016-05-31T15:00:00",
            "n2o_deleted": null,
            "n2o": 1281.096558,
            "id": 112,
            "co2_deleted": false,
            "co2": 558592.5,
            "ch4_deleted": false,
            "ch4": 20.934746
          }, {
            "sampled_at": "2016-05-31T15:00:00",
            "n2o_deleted": null,
            "n2o": 1060.04895,
            "id": 110,
            "co2_deleted": false,
            "co2": 298877.28125,
            "ch4_deleted": false,
            "ch4": 21.336681
          }],
          "id": 28,
          "height": 19.5
        }
    }
    """


injectionJson : String
injectionJson =
    """
  {"injections": [{
    "sampled_at": "2016-05-31T15:00:00",
    "n2o_deleted": null,
    "n2o": 984.054749,
    "id": 109,
    "co2_deleted": false,
    "co2": 148110.140625,
    "ch4_deleted": false,
    "ch4": 21.499937
  }, {
    "sampled_at": "2016-05-31T15:00:00",
    "n2o_deleted": null,
    "n2o": 1187.261353,
    "id": 111,
    "co2_deleted": false,
    "co2": 419945.34375,
    "ch4_deleted": false,
    "ch4": 21.085075
  }, {
    "sampled_at": "2016-05-31T15:00:00",
    "n2o_deleted": null,
    "n2o": 1281.096558,
    "id": 112,
    "co2_deleted": false,
    "co2": 558592.5,
    "ch4_deleted": false,
    "ch4": 20.934746
  }, {
    "sampled_at": "2016-05-31T15:00:00",
    "n2o_deleted": null,
    "n2o": 1060.04895,
    "id": 110,
    "co2_deleted": false,
    "co2": 298877.28125,
    "ch4_deleted": false,
    "ch4": 21.336681
    }
    ]}
      """


incubationListJson : String
incubationListJson =
    """
          {
          "data": [{
            "sampled_at": "2016-05-31T15:00:00",
            "injections": [{
              "sampled_at": "2016-05-31T15:00:00",
              "n2o_deleted": null,
              "n2o": 984.054749,
              "id": 109,
              "co2_deleted": false,
              "co2": 148110.140625,
              "ch4_deleted": false,
              "ch4": 21.499937
              }, {
                "sampled_at": "2016-05-31T15:00:00",
                "n2o_deleted": null,
                "n2o": 1187.261353,
                "id": 111,
                "co2_deleted": false,
                "co2": 419945.34375,
                "ch4_deleted": false,
                "ch4": 21.085075
              }, {
                "sampled_at": "2016-05-31T15:00:00",
                "n2o_deleted": null,
                "n2o": 1281.096558,
                "id": 112,
                "co2_deleted": false,
                "co2": 558592.5,
                "ch4_deleted": false,
                "ch4": 20.934746
              }, {
                "sampled_at": "2016-05-31T15:00:00",
                "n2o_deleted": null,
                "n2o": 1060.04895,
                "id": 110,
                "co2_deleted": false,
                "co2": 298877.28125,
                "ch4_deleted": false,
                "ch4": 21.336681
              }],
              "id": 28,
              "height": 19.5
            }]
        }
        """


standardJson : String
standardJson =
    """ {"data":{"standards":[{"vial":"STD00A","n2o_ppm":0.0,"n2o_mv":0.0,"id":2,"co2_ppm":0.0,"co2_mv":0.0,"ch4_ppm":0.0,"ch4_mv":0.0},{"vial":"STD00C","n2o_ppm":0.0,"n2o_mv":0.0,"id":3,"co2_ppm":0.0,"co2_mv":0.0,"ch4_ppm":0.0,"ch4_mv":0.0},{"vial":"STD07A","n2o_ppm":0.294,"n2o_mv":805.512817,"id":4,"co2_ppm":350.423,"co2_mv":126110.507813,"ch4_ppm":0.565,"ch4_mv":14.578513},{"vial":"STD07C","n2o_ppm":0.294,"n2o_mv":520.3573,"id":5,"co2_ppm":350.423,"co2_mv":83075.828125,"ch4_ppm":0.565,"ch4_mv":14.634157},{"vial":"STD10A","n2o_ppm":0.42,"n2o_mv":1088.805908,"id":6,"co2_ppm":500.605,"co2_mv":174609.40625,"ch4_ppm":0.806,"ch4_mv":17.025524},{"vial":"STD10C","n2o_ppm":0.42,"n2o_mv":1101.84082,"id":7,"co2_ppm":500.605,"co2_mv":174714.1875,"ch4_ppm":0.806,"ch4_mv":17.392969},{"vial":"STD15A","n2o_ppm":0.629,"n2o_mv":1477.518433,"id":8,"co2_ppm":750.907,"co2_mv":244768.640625,"ch4_ppm":1.21,"ch4_mv":19.894928},{"vial":"STD15C","n2o_ppm":0.629,"n2o_mv":1530.701538,"id":9,"co2_ppm":750.907,"co2_mv":252542.640625,"ch4_ppm":1.21,"ch4_mv":20.586039},{"vial":"STD20A","n2o_ppm":0.839,"n2o_mv":1799.6604,"id":10,"co2_ppm":1001.201,"co2_mv":308658.6875,"ch4_ppm":1.613,"ch4_mv":22.114155},{"vial":"STD20C","n2o_ppm":0.839,"n2o_mv":1875.328613,"id":11,"co2_ppm":1001.201,"co2_mv":317967.34375,"ch4_ppm":1.613,"ch4_mv":22.941065},{"vial":"STD30A","n2o_ppm":1.259,"n2o_mv":2664.494385,"id":12,"co2_ppm":1501.815,"co2_mv":482659.84375,"ch4_ppm":2.419,"ch4_mv":29.487915},{"vial":"STD30C","n2o_ppm":1.259,"n2o_mv":2628.794189,"id":13,"co2_ppm":1501.815,"co2_mv":472068.21875,"ch4_ppm":2.419,"ch4_mv":29.351187},{"vial":"STD40A","n2o_ppm":1.678,"n2o_mv":3331.138428,"id":14,"co2_ppm":2002.419,"co2_mv":632893.75,"ch4_ppm":3.226,"ch4_mv":34.833195},{"vial":"STD40C","n2o_ppm":1.678,"n2o_mv":3426.725586,"id":15,"co2_ppm":2002.419,"co2_mv":639703.9375,"ch4_ppm":3.226,"ch4_mv":35.634666},{"vial":"STD00B","n2o_ppm":0.0,"n2o_mv":0.0,"id":200,"co2_ppm":0.0,"co2_mv":0.0,"ch4_ppm":0.0,"ch4_mv":14.152516},{"vial":"STD00D","n2o_ppm":0.0,"n2o_mv":0.0,"id":201,"co2_ppm":0.0,"co2_mv":0.0,"ch4_ppm":0.0,"ch4_mv":14.678946},{"vial":"STD07B","n2o_ppm":0.294,"n2o_mv":926.001648,"id":202,"co2_ppm":350.423,"co2_mv":127831.578125,"ch4_ppm":0.565,"ch4_mv":19.136793},{"vial":"STD07D","n2o_ppm":0.294,"n2o_mv":946.895203,"id":203,"co2_ppm":350.423,"co2_mv":130525.875,"ch4_ppm":0.565,"ch4_mv":19.337883},{"vial":"STD10B","n2o_ppm":0.42,"n2o_mv":1192.180054,"id":204,"co2_ppm":500.605,"co2_mv":172341.5625,"ch4_ppm":0.806,"ch4_mv":21.219637},{"vial":"STD10D","n2o_ppm":0.42,"n2o_mv":1218.864502,"id":205,"co2_ppm":500.605,"co2_mv":173176.21875,"ch4_ppm":0.806,"ch4_mv":21.118393},{"vial":"STD15B","n2o_ppm":0.629,"n2o_mv":1610.550415,"id":206,"co2_ppm":750.907,"co2_mv":243974.625,"ch4_ppm":1.21,"ch4_mv":23.927797},{"vial":"STD15D","n2o_ppm":0.629,"n2o_mv":1672.735229,"id":207,"co2_ppm":750.907,"co2_mv":253287.4375,"ch4_ppm":1.21,"ch4_mv":24.113028},{"vial":"STD20B","n2o_ppm":0.839,"n2o_mv":2015.37561,"id":208,"co2_ppm":1001.201,"co2_mv":318978.78125,"ch4_ppm":1.613,"ch4_mv":27.295242},{"vial":"STD20D","n2o_ppm":0.839,"n2o_mv":2082.026855,"id":209,"co2_ppm":1001.201,"co2_mv":323814.15625,"ch4_ppm":1.613,"ch4_mv":26.947424},{"vial":"STD30B","n2o_ppm":1.259,"n2o_mv":2859.93042,"id":210,"co2_ppm":1501.815,"co2_mv":486949.5,"ch4_ppm":2.419,"ch4_mv":32.337463},{"vial":"STD30D","n2o_ppm":1.259,"n2o_mv":2819.568604,"id":211,"co2_ppm":1501.815,"co2_mv":468271.625,"ch4_ppm":2.419,"ch4_mv":31.783016},{"vial":"STD40B","n2o_ppm":1.678,"n2o_mv":3621.908203,"id":212,"co2_ppm":2002.419,"co2_mv":637937.6875,"ch4_ppm":3.226,"ch4_mv":36.706039},{"vial":"STD40D","n2o_ppm":1.678,"n2o_mv":3664.023682,"id":213,"co2_ppm":2002.419,"co2_mv":633270.9375,"ch4_ppm":3.226,"ch4_mv":37.041862}],"slope":null,"r2":null,"intercept":null,"id":1,"file":"EAGER1.CSV"}}
"""
