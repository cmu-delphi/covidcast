import pandas as pd
import geopandas as gpd
from datetime import date
import os
from covidcast import plotting
import pytest

SHAPEFILE_PATHS = {"county": "../../shapefiles/county/cb_2019_us_county_5m.shp",
                   "state": "../../shapefiles/state/cb_2019_us_state_5m.shp"}

CURRENT_PATH = os.path.dirname(os.path.realpath(__file__))


def test_get_geo_df():
    test_input = pd.DataFrame({"geo_value": ["24510", "31169", "37000"],
                               "value": [1.5, 2.5, 3],
                               "time_value": [date(2020, 1, 1), date(2020, 2, 3),
                                              date(2020, 2, 2)]})

    output = plotting.get_geo_df(test_input, "county")
    assert output.shape == (3233, 4)
    assert not any(pd.isna(output.geometry))


def test__join_state_geo_df():
    test_input = pd.DataFrame({"state_code": ["ca", "al", "ak"],
                               "value": [1.5, 2.5, 3]})
    geo_info = gpd.read_file(os.path.join(CURRENT_PATH, SHAPEFILE_PATHS["state"]))
    output = plotting._join_state_geo_df(test_input, "state_code", geo_info)
    assert output.shape == (56, 12)
    assert output.loc[output.STUSPS == "ca", "value"].iloc[0] == 1.5
    assert output.loc[output.STUSPS == "al", "value"].iloc[0] == 2.5
    assert output.loc[output.STUSPS == "ak", "value"].iloc[0] == 3
    assert not any(pd.isna(output.geometry))


def test__join_county_geo_df():
    test_input = pd.DataFrame({"county_code": ["24510", "31169", "37000"],
                               "value": [1.5, 2.5, 3]})
    geo_info = gpd.read_file(os.path.join(CURRENT_PATH, SHAPEFILE_PATHS["county"]))
    output = plotting._join_county_geo_df(test_input, "county_code", geo_info)
    assert output.shape == (3233, 16)
    assert output.loc[output.GEOID == "24510", "value"].iloc[0] == 1.5
    assert output.loc[output.GEOID == "31169", "value"].iloc[0] == 2.5
    assert output.loc[output.GEOID == "37077", "value"].iloc[0] == 3
    assert not any(pd.isna(output.geometry))

