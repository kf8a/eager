module SampleIncubation exposing (..)


json : String
json =
    """
  {
    "data": [
        {
            "sampled_at": "2011-05-04T19:16:52.393222",
            "n2o_ppm": 0.34854400157928467,
            "incubation_id": 50203,
            "id": 264623,
            "fid_mv": null,
            "ecd_mv": null,
            "co2_ppm": 134.1361083984375,
            "ch4_ppm": 1.8629305362701416
        },
        {
            "sampled_at": "2011-05-04T19:35:29.709224",
            "n2o_ppm": 0.35114768147468567,
            "incubation_id": 50203,
            "id": 264636,
            "fid_mv": null,
            "ecd_mv": null,
            "co2_ppm": 73.04869842529297,
            "ch4_ppm": 2282.13671875
        },
        {
            "sampled_at": "2011-05-04T20:11:15.849210",
            "n2o_ppm": 0.3541080057621002,
            "incubation_id": 50203,
            "id": 264644,
            "fid_mv": null,
            "ecd_mv": null,
            "co2_ppm": 674.137939453125,
            "ch4_ppm": 1.8641358613967896
        },
        {
            "sampled_at": "2011-05-04T19:53:22.905214",
            "n2o_ppm": 0.35141608119010925,
            "incubation_id": 50203,
            "id": 264640,
            "fid_mv": null,
            "ecd_mv": null,
            "co2_ppm": 746.5516967773438,
            "ch4_ppm": 1.8520569801330566
        },
        {
            "sampled_at": "2011-05-04T20:29:09.113220",
            "n2o_ppm": 0.3564942479133606,
            "incubation_id": 50203,
            "id": 264648,
            "fid_mv": null,
            "ecd_mv": null,
            "co2_ppm": -50,
            "ch4_ppm": 1.8188018798828125
        }
    ]
}
  """


nextJson : String
nextJson =
    """
  {
    "data": [
        {
            "sampled_at": "2011-05-04T19:16:52.393222",
            "n2o_ppm": 0.24854400157928467,
            "incubation_id": 50203,
            "id": 264623,
            "fid_mv": null,
            "ecd_mv": null,
            "co2_ppm": 104.1361083984375,
            "ch4_ppm": 1.6629305362701416
        },
        {
            "sampled_at": "2011-05-04T19:35:29.709224",
            "n2o_ppm": 0.25114768147468567,
            "incubation_id": 50203,
            "id": 264636,
            "fid_mv": null,
            "ecd_mv": null,
            "co2_ppm": 63.04869842529297,
            "ch4_ppm": 282.13671875
        },
        {
            "sampled_at": "2011-05-04T20:11:15.849210",
            "n2o_ppm": 0.3841080057621002,
            "incubation_id": 50203,
            "id": 264644,
            "fid_mv": null,
            "ecd_mv": null,
            "co2_ppm": 64.137939453125,
            "ch4_ppm": 1.9641358613967896
        },
        {
            "sampled_at": "2011-05-04T19:53:22.905214",
            "n2o_ppm": 0.45141608119010925,
            "incubation_id": 50203,
            "id": 264640,
            "fid_mv": null,
            "ecd_mv": null,
            "co2_ppm": 746.5516967773438,
            "ch4_ppm": 1.8520569801330566
        },
        {
            "sampled_at": "2011-05-04T20:29:09.113220",
            "n2o_ppm": 0.4564942479133606,
            "incubation_id": 50203,
            "id": 264648,
            "fid_mv": null,
            "ecd_mv": null,
            "co2_ppm": -50,
            "ch4_ppm": 1.8188018798828125
        }
    ]
}
  """
