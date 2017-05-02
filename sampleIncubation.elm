module SampleIncubation exposing (..)


json : String
json =
    """
    {
    "data": {
        "sampled_at": "2016-05-20T09:30:00",
        "injections": [{
            "sampled_at": "2016-05-20T09:30:00",
            "n2o_deleted": null,
            "n2o": 932.134766,
            "id": 61,
            "co2_deleted": false,
            "co2": 140414.765625,
            "ch4_deleted": false,
            "ch4": 24.330027
        }, {
            "sampled_at": "2016-05-20T09:30:00",
            "n2o_deleted": null,
            "n2o": 1078.101196,
            "id": 63,
            "co2_deleted": false,
            "co2": 227618.078125,
            "ch4_deleted": false,
            "ch4": 24.967705
        }, {
            "sampled_at": "2016-05-20T09:30:00",
            "n2o_deleted": null,
            "n2o": 1009.84021,
            "id": 62,
            "co2_deleted": false,
            "co2": 189666.828125,
            "ch4_deleted": false,
            "ch4": 24.237082
        }, {
            "sampled_at": "2016-05-20T09:30:00",
            "n2o_deleted": null,
            "n2o": 1188.570923,
            "id": 64,
            "co2_deleted": false,
            "co2": 282831.25,
            "ch4_deleted": false,
            "ch4": 23.825558
        }],
        "id": 16,
        "height": 19.625
    }
  }
  """


nextJson : String
nextJson =
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
