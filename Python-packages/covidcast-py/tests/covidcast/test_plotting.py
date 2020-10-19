import os
from datetime import date
from unittest.mock import patch

import matplotlib
import platform
import geopandas as gpd
import numpy as np
import pandas as pd
import pytest
from covidcast import plotting

SHAPEFILE_PATHS = {
    "county": "../../covidcast/shapefiles/county/cb_2019_us_county_5m.shp",
    "state": "../../covidcast/shapefiles/state/cb_2019_us_state_5m.shp",
    "msa": "../../covidcast/shapefiles/msa/cb_2019_us_cbsa_5m.shp",
    "hrr": "../../covidcast/shapefiles/hrr/geo_export_ad86cff5-e5ed-432e-9ec2-2ce8732099ee.shp"}

CURRENT_PATH = os.path.dirname(os.path.realpath(__file__))

NON_GEOMETRY_COLS = ["geo_value", "time_value", "direction", "issue", "lag", "value", "stderr",
                     "sample_size", "geo_type", "data_source", "signal", "state_fips"]


def _convert_to_array(fig: matplotlib.figure.Figure) -> np.array:
    """Covert Matplotlib Figure into an numpy array for comparison."""
    return np.frombuffer(fig.canvas.tostring_rgb(), dtype=np.uint8)  # get np array representation


@pytest.mark.skipif(platform.system() != "Linux", reason="Linux specific plot rendering expected.")
@patch("covidcast.plotting._signal_metadata")
def test_plot(mock_metadata):
    mock_metadata.side_effect = [
        {"mean_value": 0.5330011, "stdev_value": 0.4683431},
        {"mean_value": 0.5330011, "stdev_value": 0.4683431},
        {"mean_value": 0.5330011, "stdev_value": 0.4683431},
        {"mean_value": 0.5304083, "stdev_value": 0.235302},
        {"mean_value": 0.5705364, "stdev_value": 0.4348706},
        {"mean_value": 0.5705364, "stdev_value": 0.4348706},
    ]
    matplotlib.use("agg")
    # load expected choropleth as an array
    expected = np.load(os.path.join(CURRENT_PATH, "../reference_data/expected_plot_arrays.npz"))

    # test county plots
    test_county = pd.read_csv(
        os.path.join(CURRENT_PATH, "../reference_data/test_input_county_signal.csv"), dtype=str)
    test_county["time_value"] = test_county.time_value.astype("datetime64[D]")
    test_county["value"] = test_county.value.astype("float")

    # w/o megacounties
    no_mega_fig1 = plotting.plot(test_county,
                                 time_value=date(2020, 8, 4),
                                 combine_megacounties=False)
    # give margin of +-2 for floating point errors and weird variations (1 isn't consistent)
    assert np.allclose(_convert_to_array(no_mega_fig1), expected["no_mega_1"], atol=2, rtol=0)

    no_mega_fig2 = plotting.plot_choropleth(test_county,
                                            cmap="viridis",
                                            figsize=(5, 5),
                                            edgecolor="0.8",
                                            combine_megacounties=False)
    assert np.allclose(_convert_to_array(no_mega_fig2), expected["no_mega_2"], atol=2, rtol=0)

    # w/ megacounties
    mega_fig = plotting.plot_choropleth(test_county, time_value=date(2020, 8, 4))
    # give margin of +-2 for floating point errors and weird variations (1 isn't consistent)
    assert np.allclose(_convert_to_array(mega_fig), expected["mega"], atol=2, rtol=0)

    # test state
    test_state = pd.read_csv(
        os.path.join(CURRENT_PATH, "../reference_data/test_input_state_signal.csv"), dtype=str)
    test_state["time_value"] = test_state.time_value.astype("datetime64[D]")
    test_state["value"] = test_state.value.astype("float")
    state_fig = plotting.plot(test_state)
    assert np.allclose(_convert_to_array(state_fig), expected["state"], atol=2, rtol=0)

    # test MSA
    test_msa = pd.read_csv(
        os.path.join(CURRENT_PATH, "../reference_data/test_input_msa_signal.csv"), dtype=str)
    test_msa["time_value"] = test_msa.time_value.astype("datetime64[D]")
    test_msa["value"] = test_msa.value.astype("float")
    msa_fig = plotting.plot(test_msa)
    assert np.allclose(_convert_to_array(msa_fig), expected["msa"], atol=2, rtol=0)

    # test bubble
    msa_bubble_fig = plotting.plot(test_msa, plot_type="bubble")
    from matplotlib import pyplot as plt
    assert np.allclose(_convert_to_array(msa_bubble_fig), expected["msa_bubble"], atol=2, rtol=0)


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

    # test MSAs
    test_input["geo_type"] = "msa"
    test_input["geo_value"] = ["10420", "10580", "ca"]
    output4 = plotting.get_geo_df(test_input)
    expected4 = gpd.read_file(
        os.path.join(CURRENT_PATH, "../reference_data/expected_get_geo_df_msa.gpkg"),
        dtype={"geo_value": str})
    pd.testing.assert_frame_equal(expected4, output4)

    # test HRRs
    # test MSAs
    test_input["geo_type"] = "hrr"
    test_input["geo_value"] = ["10420", "102", "96"]
    output5 = plotting.get_geo_df(test_input)
    expected5 = gpd.read_file(
        os.path.join(CURRENT_PATH, "../reference_data/expected_get_geo_df_hrr.gpkg"),
        dtype={"geo_value": str})
    pd.testing.assert_frame_equal(expected5, output5)

    # test with sample signal
    test_signal = pd.read_csv(
        os.path.join(CURRENT_PATH, "../reference_data/test_input_county_signal.csv"),
        dtype={"geo_value": str}, parse_dates=["time_value", "issue"]
    )
    expected_geo_signal = gpd.read_file(
        os.path.join(CURRENT_PATH, "../reference_data/expected_get_geo_df_right_2.gpkg"),
        dtype={"geo_value": str})
    # geopandas reads file types slightly differently than pandas so need t`o recast
    expected_geo_signal["time_value"] = expected_geo_signal.time_value.astype("datetime64[ns]")
    expected_geo_signal["issue"] = expected_geo_signal.issue.astype("datetime64[ns]")
    expected_geo_signal["direction"] = np.nan
    output5 = plotting.get_geo_df(test_signal)
    pd.testing.assert_frame_equal(expected_geo_signal, output5)

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
    # test w/o megacounty combine
    # test right join
    no_mega_r = plotting._join_county_geo_df(test_input, "county_code", geo_info)
    assert type(no_mega_r) is gpd.GeoDataFrame
    expected_no_mega_r = gpd.read_file(
        os.path.join(CURRENT_PATH,
                     "../reference_data/expected__join_county_geo_df_no_mega_right.gpkg"),
        dtype={"geo_value": str})
    pd.testing.assert_frame_equal(expected_no_mega_r, no_mega_r)

    # test left join
    no_mega_l = plotting._join_county_geo_df(test_input, "county_code", geo_info, "left")
    expected_no_mega_l = gpd.read_file(
        os.path.join(CURRENT_PATH,
                     "../reference_data/expected__join_county_geo_df_no_mega_left.gpkg"),
        dtype={"geo_value": str})
    pd.testing.assert_frame_equal(expected_no_mega_l, no_mega_l)

    # test w/ megacounty combine
    mega = plotting._join_county_geo_df(test_input, "county_code", geo_info, "left", True)
    expected_mega = gpd.read_file(
        os.path.join(CURRENT_PATH,
                     "../reference_data/expected__join_county_geo_df_mega.gpkg"),
        dtype={"geo_value": str})
    pd.testing.assert_frame_equal(expected_mega, mega)


def test__join_msa_geo_df():
    test_input = pd.DataFrame({"msa": ["10180", "10420", "10580"],
                               "test_value": [1.5, 2.5, 3],
                               "test_value2": [21.5, 32.5, 34]})
    geo_info = gpd.read_file(os.path.join(CURRENT_PATH, SHAPEFILE_PATHS["msa"]))
    # test right join
    output1 = plotting._join_msa_geo_df(test_input, "msa", geo_info)
    assert type(output1) is gpd.GeoDataFrame
    # check that state parsing is working as intended
    assert all(output1[output1.msa == "35620"].state_fips == "36")

    expected1 = gpd.read_file(
        os.path.join(CURRENT_PATH, "../reference_data/expected__join_msa_geo_df_right.gpkg"),
        dtype={"geo_value": str})
    pd.testing.assert_frame_equal(expected1, output1)

    # test left join
    output2 = plotting._join_msa_geo_df(test_input, "msa", geo_info, "left")
    expected2 = gpd.read_file(
        os.path.join(CURRENT_PATH, "../reference_data/expected__join_msa_geo_df_left.gpkg"),
        dtype={"geo_value": str})
    pd.testing.assert_frame_equal(expected2, output2)


def test__join_hrr_geo_df():
    test_input = pd.DataFrame({"hrr": ["1", "102", "96"],
                               "test_value": [1.5, 2.5, 3],
                               "test_value2": [21.5, 32.5, 34]})
    geo_info = gpd.read_file(os.path.join(CURRENT_PATH, SHAPEFILE_PATHS["hrr"]))
    # test right join
    output1 = plotting._join_hrr_geo_df(test_input, "hrr", geo_info)
    assert type(output1) is gpd.GeoDataFrame

    expected1 = gpd.read_file(
        os.path.join(CURRENT_PATH, "../reference_data/expected__join_hrr_geo_df_right.gpkg"),
        dtype={"geo_value": str})
    pd.testing.assert_frame_equal(expected1, output1)

    # test left join
    output2 = plotting._join_hrr_geo_df(test_input, "hrr", geo_info, "left")
    expected2 = gpd.read_file(
        os.path.join(CURRENT_PATH, "../reference_data/expected__join_hrr_geo_df_left.gpkg"),
        dtype={"geo_value": str})
    pd.testing.assert_frame_equal(expected2, output2)


def test__is_megacounty():
    assert plotting._is_megacounty("12000")
    assert not plotting._is_megacounty("12001")
    assert not plotting._is_megacounty("120000")
