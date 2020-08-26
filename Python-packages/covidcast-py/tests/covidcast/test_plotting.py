import os

import geopandas as gpd
import pandas as pd
from covidcast import plotting

import pytest

SHAPEFILE_PATHS = {"county": "../../covidcast/shapefiles/county/cb_2019_us_county_5m.shp",
                   "state": "../../covidcast/shapefiles/state/cb_2019_us_state_5m.shp"}

CURRENT_PATH = os.path.dirname(os.path.realpath(__file__))


def test_get_geo_df():
    test_input = pd.DataFrame({"geo_value": ["24510", "31169", "37000"],
                               "value": [1.5, 2.5, 3],
                               "geo_type": ["county", "county", "county"],
                               "signal": ["a", "a", "a"],
                               "data_source": ["b", "b", "b"]})
    # test counties
    output = plotting.get_geo_df(test_input)
    assert output.shape == (3233, test_input.shape[1]+2)
    assert not any(pd.isna(output.geometry))
    assert not any(pd.isna(output.geo_value))  # all geo_values should be populated

    # test counties w/ left join
    output = plotting.get_geo_df(test_input, join_type="left")
    assert output.shape == (3, test_input.shape[1]+2)
    assert not any(pd.isna(output.geo_value))
    assert sum(pd.isna(output.geometry)) == 1  # one megacounty should be nan

    # test states
    test_input["geo_type"] = "state"
    output = plotting.get_geo_df(test_input)
    assert output.shape == (56, test_input.shape[1]+2)
    assert not any(pd.isna(output.geometry))
    assert not any(pd.isna(output.geo_value))

    # just test output cols match, since the actual values are tested in the _join_*_geo_df methods
    assert set(output.columns) == {"geo_value", "value", "geo_type", "signal",
                                   "data_source", "geometry", "state_fips"}

    # test a non county or state geo_type
    with pytest.raises(ValueError):
        plotting.get_geo_df(pd.DataFrame(
            {"geo_value": ["a"], "geo_type": ["b"], "signal": ["c"], "data_source": ["d"]}))
    # test_duplicate_values
    with pytest.raises(ValueError):
        plotting.get_geo_df(pd.DataFrame({"geo_value": ["24510", "24510"]}))


def test__join_state_geo_df():
    # TODO? generate expected output shapefile+metadata files for tests
    test_input = pd.DataFrame({"state_code": ["ca", "al", "ak"],
                               "value": [1.5, 2.5, 3]})
    geo_info = gpd.read_file(os.path.join(CURRENT_PATH, SHAPEFILE_PATHS["state"]))
    output = plotting._join_state_geo_df(test_input, "state_code", geo_info)
    # test output df is the right dimensions
    assert output.shape == (56, test_input.shape[1]+2)
    # verify values correct
    assert output.loc[output.state_code == "ca", "value"].iloc[0] == 1.5
    assert output.loc[output.state_code == "al", "value"].iloc[0] == 2.5
    assert output.loc[output.state_code == "ak", "value"].iloc[0] == 3
    # test all polygons and state values populated
    assert not any(pd.isna(output.geometry))
    assert not any(pd.isna(output.state_code))
    # test left join
    output = plotting._join_state_geo_df(test_input, "state_code", geo_info, "left")
    assert output.shape == (3, test_input.shape[1]+2)


def test__join_county_geo_df():
    test_input = pd.DataFrame({"county_code": ["24510", "31169", "37000"],
                               "test_value": [1.5, 2.5, 3],
                               "test_value2": [21.5, 32.5, 34]})
    geo_info = gpd.read_file(os.path.join(CURRENT_PATH, SHAPEFILE_PATHS["county"]))
    output = plotting._join_county_geo_df(test_input, "county_code", geo_info)
    assert output.shape == (3233, test_input.shape[1]+2)
    assert output.loc[output.county_code == "24510", "test_value"].iloc[0] == 1.5
    assert output.loc[output.county_code == "31169", "test_value"].iloc[0] == 2.5
    # verify megacounties all populated
    assert all(output.loc[[i.startswith("37") for i in output.county_code], "test_value"] == 3)
    assert output.loc[output.county_code == "24510", "test_value2"].iloc[0] == 21.5
    assert output.loc[output.county_code == "31169", "test_value2"].iloc[0] == 32.5
    assert all(output.loc[[i.startswith("37") for i in output.county_code], "test_value2"] == 34)
    assert not any(pd.isna(output.geometry))
    assert not any(pd.isna(output.county_code))

    output = plotting._join_county_geo_df(test_input, "county_code", geo_info, "left")
    assert output.shape == (3, test_input.shape[1]+2)

