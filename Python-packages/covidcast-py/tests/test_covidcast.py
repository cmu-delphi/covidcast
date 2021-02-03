import warnings
from datetime import date, datetime
from unittest.mock import patch

# Force tests to use a specific backend, so they reproduce across platforms
import matplotlib

matplotlib.use("AGG")

import pandas as pd
import numpy as np
import pytest
from epiweeks import Week
from covidcast import covidcast
from covidcast.errors import NoDataWarning


def sort_df(df):
    """Helper function for sorting dfs for comparison."""
    df = df.sort_index(axis=1)
    df.sort_values(df.columns[0], inplace=True)
    return df


@patch("covidcast.covidcast._signal_metadata")
@patch("delphi_epidata.Epidata.covidcast")
def test_signal(mock_covidcast, mock_metadata):
    mock_covidcast.return_value = {"result": 1,  # successful API response
                                   "epidata": [{"time_value": 20200622,
                                                "issue": 20200724,
                                                "direction": None}],
                                   "message": "success"}
    mock_metadata.return_value = {"max_time": pd.Timestamp("2020-08-04 00:00:00"),
                                  "min_time": pd.Timestamp("2020-08-03 00:00:00")}

    return_rows = {"time_value": [datetime(2020, 6, 22)],
                   "issue": datetime(2020, 7, 24),
                   "geo_type": "county",
                   "data_source": "source",
                   "signal": "signal"
                   }
    # test happy path with no start or end day and one geo_value
    response = covidcast.signal("source", "signal", geo_values="CA")
    expected = pd.DataFrame(return_rows, index=[0]*2)
    assert sort_df(response).equals(sort_df(expected))

    # test happy path with start and end day (8 days apart) and one geo_value
    response = covidcast.signal("source", "signal", start_day=date(2020, 8, 1),
                                end_day=date(2020, 8, 8), geo_values="CA")
    expected = pd.DataFrame(return_rows, index=[0]*8)
    assert sort_df(response).equals(sort_df(expected))

    # test duplicate geo values
    response = covidcast.signal("source", "signal", start_day=date(2020, 8, 1),
                                end_day=date(2020, 8, 8), geo_values=["CA", "CA"])
    expected = pd.DataFrame(return_rows, index=[0]*8)

    assert sort_df(response).equals(sort_df(expected))

    # test no df output
    mock_covidcast.return_value = {"result": -2,
                                   "message": "no results fouds"}
    assert not covidcast.signal("source", "signal", geo_values=[])

    # test incorrect geo
    with pytest.raises(ValueError):
        covidcast.signal("source", "signal", geo_type="not_a_real_geo")

    # test invalid dates
    with pytest.raises(ValueError):
        covidcast.signal("source", "signal", geo_type="state",
                         start_day=date(2020, 4, 2), end_day=date(2020, 4, 1))


@patch("delphi_epidata.Epidata.covidcast_meta")
def test_metadata(mock_covidcast_meta):
    # not generating full DF since most attributes used
    mock_covidcast_meta.side_effect = [
        {"result": 1,  # successful API response
         "epidata": [{"max_time": 20200622, "min_time": 20200421, "last_update": 12345, "time_type": "day"},
                     {"max_time": 20200724, "min_time": 20200512, "last_update": 99999, "time_type": "day"}],
         "message": "success"},
        {"result": 0,  # unsuccessful API response
         "epidata": [{"max_time": 20200622, "min_time": 20200421},
                     {"max_time": 20200724, "min_time": 20200512}],
         "message": "error: failed"}]
    # test happy path
    response = covidcast.metadata()
    expected = pd.DataFrame({
        "max_time": [datetime(2020, 6, 22), datetime(2020, 7, 24)],
        "min_time": [datetime(2020, 4, 21), datetime(2020, 5, 12)],
        "last_update": [datetime(1970, 1, 1, 3, 25, 45), datetime(1970, 1, 2, 3, 46, 39)],
        "time_type": ["day", "day"]})
    assert sort_df(response).equals(sort_df(expected))

    # test failed response raises RuntimeError
    with pytest.raises(RuntimeError):
        covidcast.metadata()


def test_aggregate_signals():
    test_input1 = pd.DataFrame(
        {"geo_value": ["a", "b", "c", "a"],
         "time_value": [date(2020, 1, 1), date(2020, 1, 1), date(2020, 1, 1), date(2020, 1, 2)],
         "value": [2, 4, 6, 8],
         "signal": ["i", "i", "i", "i"],
         "geo_type": ["state", "state", "state", "state"],
         "data_source": ["x", "x", "x", "x"]})
    test_input2 = pd.DataFrame(
        {"geo_value": ["a", "b", "c", "d"],
         "time_value": [date(2020, 1, 1), date(2020, 1, 1), date(2020, 1, 1), date(2020, 1, 1)],
         "value": [1, 3, 5, 7],
         "signal": ["j", "j", "j", "j"],
         "geo_type": ["state", "state", "state", "state"],
         "data_source": ["y", "y", "y", "y"],
         "extra_col": ["0", "0", "0", "0"]})
    test_input3 = pd.DataFrame(
        {"geo_value": ["b", "c", "d", "b"],
         "time_value": [date(2020, 1, 1), date(2020, 1, 1), date(2020, 1, 1), date(2020, 1, 2)],
         "value": [0.5, 1.5, 2.5, 3.5],
         "signal": ["k", "k", "k", "k"],
         "geo_type": ["state", "state", "state", "state"],
         "data_source": ["z", "z", "z", "z"]})
    # test 3 signals from 3 sources with outer join
    expected1 = pd.DataFrame(
        {"geo_value": ["a", "b", "c", "d", "a", "b", "c", "d", "b"],
         "time_value": [date(2020, 1, 1), date(2020, 1, 1), date(2020, 1, 1), date(2020, 1, 1),
                        date(2020, 1, 2), date(2020, 1, 2), date(2020, 1, 2), date(2020, 1, 2),
                        date(2020, 1, 3)],
         "x_i_0_value": [2, 4, 6, np.nan, 8, np.nan, np.nan, np.nan, np.nan],
         "y_j_1_value": [1, 3, 5, 7, np.nan, np.nan, np.nan, np.nan, np.nan],
         "y_j_1_extra_col": ["0", "0", "0", "0", np.nan, np.nan, np.nan, np.nan, np.nan],
         "z_k_2_value": [np.nan, np.nan, np.nan, np.nan, np.nan, 0.5, 1.5, 2.5, 3.5],
         "geo_type": ["state"]*9})
    assert covidcast.aggregate_signals(
        [test_input1, test_input2, test_input3], dt=[0, 0, 1]).equals(expected1)

    # test 3 signals from 3 sources with inner join has no intersection
    assert covidcast.aggregate_signals(
        [test_input1, test_input3], dt=[0, 1], join_type="inner").empty

    # test 2 signals from same source (one lagged) with inner join
    expected2 = pd.DataFrame(
        {"geo_value": ["a"],
         "time_value": [date(2020, 1, 2)],
         "x_i_0_value": [8],
         "x_i_1_value": [2],
         "geo_type": ["state"]})
    assert covidcast.aggregate_signals(
        [test_input1, test_input1], dt=[0, 1], join_type="inner").equals(expected2)

    # test same signal twice with a lag
    expected3 = pd.DataFrame(
        {"geo_value": ["a", "b", "c", "a", "b", "c", "a"],
         "time_value": [date(2020, 1, 1), date(2020, 1, 1), date(2020, 1, 1), date(2020, 1, 2),
                        date(2020, 1, 2), date(2020, 1, 2), date(2020, 1, 3)],
         "x_i_0_value": [2, 4, 6, 8, np.nan, np.nan, np.nan],
         "x_i_1_value": [np.nan, np.nan, np.nan, 2, 4, 6, 8],
         "geo_type": ["state"]*7})

    assert covidcast.aggregate_signals([test_input1, test_input1], dt=[0, 1]).equals(expected3)

    # test invalid lag length
    with pytest.raises(ValueError):
        covidcast.aggregate_signals([test_input1, test_input1], dt=[0])

    # test mixed geo_types
    test_input4 = pd.DataFrame(
        {"geo_value": ["b", "c", "d", "b"],
         "time_value": [date(2020, 1, 1), date(2020, 1, 1), date(2020, 1, 1), date(2020, 1, 2)],
         "value": [0.5, 1.5, 2.5, 3.5],
         "signal": ["k", "k", "k", "k"],
         "geo_type": ["county", "county", "county", "county"],
         "data_source": ["z", "z", "z", "z"]})
    with pytest.raises(ValueError):
        covidcast.aggregate_signals([test_input1, test_input4])


def test__parse_datetimes():
    assert covidcast._parse_datetimes("202001", "week") == pd.Timestamp(Week(2020, 1).startdate())
    assert covidcast._parse_datetimes("20200205", "day") == pd.Timestamp("20200205")
    assert pd.isna(covidcast._parse_datetimes("2020", "test"))


def test__detect_metadata():
    test_input = pd.DataFrame(
        {"data_source": ["a", "a"], "signal": ["b", "b"], "geo_type": ["c", "c"]})
    assert covidcast._detect_metadata(test_input) == ("a", "b", "c")

    # test heterogenous cases error
    test_bad_source = pd.DataFrame(
        {"data_source": ["a", "d"], "signal": ["b", "b"], "geo_type": ["c", "c"]})
    with pytest.raises(ValueError):
        covidcast._detect_metadata(test_bad_source)
    test_bad_signal = pd.DataFrame(
        {"data_source": ["a", "a"], "signal": ["d", "b"], "geo_type": ["c", "c"]})
    with pytest.raises(ValueError):
        covidcast._detect_metadata(test_bad_signal)
    test_bad_geo = pd.DataFrame(
        {"data_source": ["a", "a"], "signal": ["b", "b"], "geo_type": ["c", "x"]})
    with pytest.raises(ValueError):
        covidcast._detect_metadata(test_bad_geo)


@patch("delphi_epidata.Epidata.covidcast")
def test__fetch_epidata(mock_covidcast):
    mock_covidcast.return_value = {"message": "failed"}
    # test warning when an unknown bad response is received
    with warnings.catch_warnings(record=True) as w:
        covidcast._fetch_epidata("source", "signal", date(2020, 4, 2), date(2020, 4, 2),
                                 "*", None, None, None, None)
        assert len(w) == 1
        assert str(w[0].message) == \
               "Problem obtaining source signal data on 20200402 for geography '*': failed"
        assert w[0].category is RuntimeWarning

    # test warning when a no data response is received
    mock_covidcast.return_value = {"message": "no results"}  # no data API response
    with warnings.catch_warnings(record=True) as w:
        covidcast._fetch_epidata("source", "signal", date(2020, 4, 2), date(2020, 4, 2),
                                 "county", None, None, None, None)
        assert len(w) == 1
        assert str(w[0].message) == "No source signal data found on 20200402 for geography 'county'"
        assert w[0].category is NoDataWarning

    # test no epidata yields nothing
    mock_covidcast.return_value = {"message": "success"}  # no epidata
    assert not covidcast._fetch_epidata(None, None, date(2020, 4, 1), date(2020, 4, 1),
                                        None, None, None, None, None)
    # test end_day < start_day yields nothing
    mock_covidcast.return_value = {"message": "success"}  # no epidata
    assert not covidcast._fetch_epidata(None, None, date(2020, 4, 2), date(2020, 4, 1),
                                        None, None, None, None, None)
    # not generating full DF since most attributes used
    mock_covidcast.side_effect = [{"result": 1,  # successful API response
                                   "epidata": [{"time_value": 20200622,
                                                "issue": 20200724,
                                                "direction": None}],
                                   "message": "success"},
                                  {"result": 1,  # second successful API
                                   "epidata": [{"time_value": 20200821,
                                                "issue": 20200925}],
                                   "message": "success"}]
    # test happy path with 2 day range
    response = covidcast._fetch_epidata(
        None, None, date(2020, 4, 2), date(2020, 4, 3), None, None, None, None, None)
    expected = [pd.DataFrame({"time_value": [20200622],
                              "issue": [20200724],
                              "direction": None
                             }),
                pd.DataFrame({"time_value": [20200821],
                              "issue": [20200925],
                              }),
                ]
    assert len(response) == 2
    pd.testing.assert_frame_equal(response[0], expected[0])
    pd.testing.assert_frame_equal(response[1], expected[1])

@patch("delphi_epidata.Epidata.async_epidata")
def test__async_fetch_epidata(mock_async_epidata):
    mock_async_epidata.return_value = [({"message": "failed"}, {"time_values": 20200402})]
    # test warning when an unknown bad response is received
    with warnings.catch_warnings(record=True) as w:
        covidcast._async_fetch_epidata("source", "signal", date(2020, 4, 2), date(2020, 4, 2),
                                 "*", None, None, None, None)
        assert len(w) == 1
        assert str(w[0].message) == \
               "Problem obtaining source signal data on 20200402 for geography '*': failed"
        assert w[0].category is RuntimeWarning

    # test warning when a no data response is received
    mock_async_epidata.return_value = [({"message": "no results"}, {"time_values": 20200402})]  # no data API response
    with warnings.catch_warnings(record=True) as w:
        covidcast._async_fetch_epidata("source", "signal", date(2020, 4, 2), date(2020, 4, 2),
                                 "county", None, None, None, None)
        assert len(w) == 1
        assert str(w[0].message) == "No source signal data found on 20200402 for geography 'county'"
        assert w[0].category is NoDataWarning

    # test no epidata yields nothing
    mock_async_epidata.return_value = [({"message": "success"}, None)]  # no epidata
    assert not covidcast._async_fetch_epidata(None, None, date(2020, 4, 1), date(2020, 4, 1),
                                        None, None, None, None, None)
    # test end_day < start_day yields nothing
    mock_async_epidata.return_value = [({"message": "success"}, None)]  # no epidata
    assert not covidcast._async_fetch_epidata(None, None, date(2020, 4, 2), date(2020, 4, 1),
                                        None, None, None, None, None)

    # not generating full DF since most attributes used
    mock_async_epidata.return_value = [
        ({"result": 1,  # successful API response
           "epidata": [{"time_value": 20200622,
                        "issue": 20200724,
                        "direction": None}],
           "message": "success"}, {}),
        ({"result": 1,  # second successful API
           "epidata": [{"time_value": 20200821,
                        "issue": 20200925}],
           "message": "success"}, {})
    ]
    # test happy path with 2 day range
    response = covidcast._async_fetch_epidata(
        None, None, date(2020, 4, 2), date(2020, 4, 3), None, None, None, None, None)
    expected = [pd.DataFrame({"time_value": [20200622],
                              "issue": [20200724],
                              "direction": None
                              }),
                pd.DataFrame({"time_value": [20200821],
                              "issue": [20200925],
                              }),
                ]
    assert len(response) == 2
    pd.testing.assert_frame_equal(response[0], expected[0])
    pd.testing.assert_frame_equal(response[1], expected[1])

@patch("covidcast.covidcast.metadata")
def test__signal_metadata(mock_metadata):
    mock_metadata.return_value = pd.DataFrame({"data_source": ["usa-facts", "doctor-visits"],
                                               "signal": ["raw_cli", "smooth_cli"],
                                               "time_type": ["day", "day"],
                                               "geo_type": ["hrr", "state"]})

    # test happy path
    assert covidcast._signal_metadata("usa-facts", "raw_cli", "hrr") == \
           {"data_source": "usa-facts", "signal": "raw_cli", "time_type": "day", "geo_type": "hrr"}

    # test no matches raises ValueError
    with pytest.raises(ValueError):
        covidcast._signal_metadata("not", "actual", "values")


def test__date_to_api_string():
    # since the function just wraps strftime, this is just to ensure the format doesn't change
    assert covidcast._date_to_api_string(date(2020, 4, 2)) == "20200402"


def test__dates_to_api_strings():
    # test happy paths
    assert covidcast._dates_to_api_strings(date(2020, 4, 2)) == "20200402"
    assert covidcast._dates_to_api_strings([date(2020, 4, 2),
                                            date(2020, 5, 2)]) == "20200402-20200502"
