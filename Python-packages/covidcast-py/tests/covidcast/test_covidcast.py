from datetime import date, datetime
from unittest.mock import patch

# Force tests to use a specific backend, so they reproduce across platforms
import matplotlib
matplotlib.use("AGG")

import pandas as pd
import pytest
from covidcast import covidcast


def sort_df(df):
    """Helper function for sorting dfs for comparison."""
    df = df.sort_index(axis=1)
    df.sort_values(df.columns[0], inplace=True)
    return df


@patch("covidcast.covidcast._signal_metadata")
@patch("delphi_epidata.Epidata.covidcast")
def test_signal(mock_covidcast, mock_metadata):
    mock_covidcast.return_value = {"result": 1,  # successful API response
                                   "epidata": [{"time_value": 20200622, "issue": 20200724}],
                                   "message": "success"}
    mock_metadata.return_value = {"max_time": pd.Timestamp("2020-08-04 00:00:00"),
                                  "min_time": pd.Timestamp("2020-08-03 00:00:00")}

    # test happy path with no start or end day and one geo_value
    response = covidcast.signal("source", "signal", geo_values="CA")
    expected = pd.DataFrame({"time_value": [datetime(2020, 6, 22)]*2,
                             "issue": datetime(2020, 7, 24),
                             "geo_type": "county",
                             "data_source": "source",
                             "signal": "signal"
                             },
                            index=[0]*2)
    assert sort_df(response).equals(sort_df(expected))

    # test happy path with no start or end day and two geo_values
    response = covidcast.signal("source", "signal", geo_values=["CA", "AL"])
    expected = pd.DataFrame({"time_value": [datetime(2020, 6, 22)]*4,
                             "issue": datetime(2020, 7, 24),
                             "geo_type": "county",
                             "data_source": "source",
                             "signal": "signal"
                             },
                            index=[0]*4)
    assert sort_df(response).equals(sort_df(expected))

    # test happy path with start and end day (8 days apart) and one geo_value
    response = covidcast.signal("source", "signal", start_day=date(2020, 8, 1),
                                end_day=date(2020, 8, 8), geo_values="CA")
    expected = pd.DataFrame({"time_value": [datetime(2020, 6, 22)]*8,
                             "issue": datetime(2020, 7, 24),
                             "geo_type": "county",
                             "data_source": "source",
                             "signal": "signal"
                             },
                            index=[0]*8,
                            )
    assert sort_df(response).equals(sort_df(expected))

    # test no df output
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
    mock_covidcast_meta.side_effect = [{"result": 1,  # successful API response
                                        "epidata": [{"max_time": 20200622, "min_time": 20200421},
                                                    {"max_time": 20200724, "min_time": 20200512}],
                                        "message": "success"},
                                       {"result": 0,  # unsuccessful API response
                                        "epidata": [{"max_time": 20200622, "min_time": 20200421},
                                                    {"max_time": 20200724, "min_time": 20200512}],
                                        "message": "error: failed"}]

    # test happy path
    response = covidcast.metadata()
    expected = pd.DataFrame({"max_time": [datetime(2020, 6, 22), datetime(2020, 7, 24)],
                                     "min_time": [datetime(2020, 4, 21), datetime(2020, 5, 12)]})
    assert sort_df(response).equals(sort_df(expected))

    # test failed response raises RuntimeError
    with pytest.raises(RuntimeError):
        covidcast.metadata()


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
def test__fetch_single_geo(mock_covidcast):
    # not generating full DF since most attributes used
    mock_covidcast.side_effect = [{"result": 1,  # successful API response
                                   "epidata": [{"time_value": 20200622, "issue": 20200724}],
                                   "message": "success"},
                                  {"result": 1,  # second successful API
                                   "epidata": [{"time_value": 20200821, "issue": 20200925}],
                                   "message": "success"},
                                  {"message": "error: failed"},  # unsuccessful API response
                                  {"message": "success"}]  # no epidata

    # test happy path with 2 day range
    response = covidcast._fetch_single_geo(
        None, None, date(2020, 4, 2), date(2020, 4, 3), None, None, None, None, None)
    expected = pd.DataFrame({"time_value": [datetime(2020, 6, 22), datetime(2020, 8, 21)],
                             "issue": [datetime(2020, 7, 24), datetime(2020, 9, 25)],
                             "geo_type": None,
                             "data_source": None,
                             "signal": None
                             },
                            index=[0, 0])
    assert sort_df(response).equals(sort_df(expected))

    # test warning is raised if unsuccessful API response
    with pytest.warns(UserWarning):
        covidcast._fetch_single_geo(None, None, date(2020, 4, 2), date(2020, 4, 2),
                                    None, None, None, None, None)

    # test no epidata yields nothing
    assert not covidcast._fetch_single_geo(None, None, date(2020, 4, 2), date(2020, 4, 1),
                                           None, None, None, None, None)

    # test end_day < start_day yields nothing
    assert not covidcast._fetch_single_geo(None, None, date(2020, 4, 2), date(2020, 4, 1),
                                           None, None, None, None, None)


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
