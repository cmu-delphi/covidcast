import os

import geopandas as gpd
import matplotlib

matplotlib.use("agg")

import pandas as pd
from covidcast import plotting
import numpy as np

SHAPEFILE_PATHS = {"county": "../../covidcast/shapefiles/cb_2019_us_county_5m.zip",
                   "state": "../../covidcast/shapefiles/cb_2019_us_state_5m.zip"}

CURRENT_PATH = os.path.dirname(os.path.realpath(__file__))


def test_plot_choropleth():
    matplotlib.use("agg")
    # load expected choropleth as an array
    expected = np.load(os.path.join(CURRENT_PATH, "../reference_data/expected_plot_array.npz"))
    test_df = pd.read_csv(os.path.join(CURRENT_PATH, "../reference_data/test_signal.csv"),
                          dtype=str)
    test_df["time_value"] = test_df.time_value.astype("datetime64[D]")
    test_df["value"] = test_df.value.astype("float")

    fig1 = plotting.plot_choropleth(test_df)
    data1 = np.frombuffer(fig1.canvas.tostring_rgb(), dtype=np.uint8)  # get np array representation
    # give margin of +-2 for floating point errors and platform discrepancies.
    assert np.allclose(data1, expected["expected_1"], atol=2, rtol=0)

    fig2 = plotting.plot_choropleth(test_df, cmap="viridis", figsize=(5, 5), edgecolor="0.8")
    data2 = np.frombuffer(fig2.canvas.tostring_rgb(), dtype=np.uint8)
    assert np.allclose(data2, expected["expected_2"], atol=2, rtol=0)


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

    # test states
    test_input["geo_type"] = "state"
    output = plotting.get_geo_df(test_input)
    assert output.shape == (56, test_input.shape[1]+2)
    assert not any(pd.isna(output.geometry))

    # just test output cols match, since the actual values are tested in the _join_*_geo_df methods
    assert set(output.columns) == {"geo_value", "value", "geo_type", "signal",
                                   "data_source", "geometry", "state_fips"}


def test__join_state_geo_df():
    # TODO? generate expected output shapefile+metadata files for tests
    test_input = pd.DataFrame({"state_code": ["ca", "al", "ak"],
                               "value": [1.5, 2.5, 3]})
    geo_info = gpd.read_file("zip://" + os.path.join(CURRENT_PATH, SHAPEFILE_PATHS["state"]))
    output = plotting._join_state_geo_df(test_input, "state_code", geo_info)
    # test output df is the right dimensions
    assert output.shape == (56, test_input.shape[1]+10)
    # verify values right
    assert output.loc[output.STUSPS == "ca", "value"].iloc[0] == 1.5
    assert output.loc[output.STUSPS == "al", "value"].iloc[0] == 2.5
    assert output.loc[output.STUSPS == "ak", "value"].iloc[0] == 3
    # test all polygons populated
    assert not any(pd.isna(output.geometry))


def test__join_county_geo_df():
    test_input = pd.DataFrame({"county_code": ["24510", "31169", "37000"],
                               "value": [1.5, 2.5, 3]})
    geo_info = gpd.read_file("zip://" + os.path.join(CURRENT_PATH, SHAPEFILE_PATHS["county"]))
    output = plotting._join_county_geo_df(test_input, "county_code", geo_info)
    assert output.shape == (3233, test_input.shape[1]+14)
    assert output.loc[output.GEOID == "24510", "value"].iloc[0] == 1.5
    assert output.loc[output.GEOID == "31169", "value"].iloc[0] == 2.5
    assert output.loc[output.GEOID == "37077", "value"].iloc[0] == 3
    assert not any(pd.isna(output.geometry))

