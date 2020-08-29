import os

import geopandas as gpd
import numpy as np
import pandas as pd
import pytest
from covidcast import plotting

SHAPEFILE_PATHS = {"county": "../../covidcast/shapefiles/county/cb_2019_us_county_5m.shp",
                   "state": "../../covidcast/shapefiles/state/cb_2019_us_state_5m.shp"}

CURRENT_PATH = os.path.dirname(os.path.realpath(__file__))

NON_GEOMETRY_COLS = ["geo_value", "time_value", "direction", "issue", "lag", "value", "stderr",
                     "sample_size", "geo_type", "data_source", "signal", "state_fips"]


def test_get_geo_df():
    test_input = pd.DataFrame({"geo_value": ["24510", "31169", "37000"],
                               "value": [1.5, 2.5, 3],
                               "geo_type": ["county", "county", "county"],
                               "signal": ["a", "a", "a"],
                               "data_source": ["b", "b", "b"]})
    # test counties
    output1 = plotting.get_geo_df(test_input)
    expected1 = gpd.read_file(
        os.path.join(CURRENT_PATH, "../reference_data/expected_get_geo_df_right.gpkg"),
        dtype={"geo_value": str})
    pd.testing.assert_frame_equal(expected1, output1)

    # test counties w/ left join
    output2 = plotting.get_geo_df(test_input, join_type="left")
    expected2 = gpd.read_file(
        os.path.join(CURRENT_PATH, "../reference_data/expected_get_geo_df_left.gpkg"),
        dtype={"geo_value": str})
    pd.testing.assert_frame_equal(expected2, output2)

    # test states
    test_input["geo_type"] = "state"
    test_input["geo_value"] = ["24510", "31169", "ca"]
    output3 = plotting.get_geo_df(test_input)
    expected3 = gpd.read_file(
        os.path.join(CURRENT_PATH, "../reference_data/expected_get_geo_df_state.gpkg"),
        dtype={"geo_value": str})
    pd.testing.assert_frame_equal(expected3, output3)

    # test with sample signal
    test_input2 = pd.read_csv(
        os.path.join(CURRENT_PATH, "../reference_data/test_input_county_signal.csv"),
        dtype={"geo_value": str}, parse_dates=["time_value", "issue"]
    )
    expected4 = gpd.read_file(
        os.path.join(CURRENT_PATH, "../reference_data/expected_get_geo_df_right_2.gpkg"),
        dtype={"geo_value": str})
    # geopandas reads file types slightly differently than pandas so need to recast
    expected4["time_value"] = expected4.time_value.astype("datetime64[ns]")
    expected4["issue"] = expected4.issue.astype("datetime64[ns]")
    expected4["direction"] = np.nan
    output4 = plotting.get_geo_df(test_input2)
    pd.testing.assert_frame_equal(expected4, output4)

    # test a non county or state geo_type
    with pytest.raises(ValueError):
        plotting.get_geo_df(pd.DataFrame(
            {"geo_value": ["a"], "geo_type": ["b"], "signal": ["c"], "data_source": ["d"]}))

    # test_duplicate_values
    with pytest.raises(ValueError):
        plotting.get_geo_df(pd.DataFrame({"geo_value": ["24510", "24510"]}))


def test__join_state_geo_df():
    test_input = pd.DataFrame({"state_code": ["ca", "al", "ak"],
                               "value": [1.5, 2.5, 3]})
    geo_info = gpd.read_file(os.path.join(CURRENT_PATH, SHAPEFILE_PATHS["state"]))
    # test right join
    output1 = plotting._join_state_geo_df(test_input, "state_code", geo_info)
    assert type(output1) is gpd.GeoDataFrame
    expected1 = gpd.read_file(
        os.path.join(CURRENT_PATH, "../reference_data/expected__join_state_geo_df_right.gpkg"),
        dtype={"geo_value": str})
    pd.testing.assert_frame_equal(expected1, output1)

    # test left join
    output2 = plotting._join_state_geo_df(test_input, "state_code", geo_info, "left")
    expected2 = gpd.read_file(
        os.path.join(CURRENT_PATH, "../reference_data/expected__join_state_geo_df_left.gpkg"),
        dtype={"geo_value": str})
    pd.testing.assert_frame_equal(expected2, output2)


def test__join_county_geo_df():
    test_input = pd.DataFrame({"county_code": ["24510", "31169", "37000"],
                               "test_value": [1.5, 2.5, 3],
                               "test_value2": [21.5, 32.5, 34]})
    geo_info = gpd.read_file(os.path.join(CURRENT_PATH, SHAPEFILE_PATHS["county"]))
    # test right join
    output1 = plotting._join_county_geo_df(test_input, "county_code", geo_info)
    assert type(output1) is gpd.GeoDataFrame
    expected1 = gpd.read_file(
        os.path.join(CURRENT_PATH, "../reference_data/expected__join_county_geo_df_right.gpkg"),
        dtype={"geo_value": str})
    pd.testing.assert_frame_equal(expected1, output1)

    # test left join
    output2 = plotting._join_county_geo_df(test_input, "county_code", geo_info, "left")
    expected2 = gpd.read_file(
        os.path.join(CURRENT_PATH, "../reference_data/expected__join_county_geo_df_left.gpkg"),
        dtype={"geo_value": str})
    pd.testing.assert_frame_equal(expected2, output2)

