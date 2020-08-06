from datetime import date, datetime
from unittest.mock import patch

import pandas as pd
import pytest
from covidcast import covidcast


def test_signal():
    # TODO test happy path.
    with pytest.raises(ValueError):
        covidcast.signal("source", "signal", geo_type="not_a_real_geo")


@patch("delphi_epidata.Epidata.covidcast_meta")
def test_metadata(mock_covidcast_meta):
    # not generating full DF since most attributes used
    mock_covidcast_meta.side_effect = [{"result": 1,  # successful API response
                                        "epidata": [{'max_time': 20200622, 'min_time': 20200421},
                                                    {'max_time': 20200724, 'min_time': 20200512}],
                                        "message": "success"},
                                       {"result": 0,  # unsuccessful API response
                                        "epidata": [{'max_time': 20200622, 'min_time': 20200421},
                                                    {'max_time': 20200724, 'min_time': 20200512}],
                                        "message": "error: failed"}]

    # test happy path
    assert covidcast.metadata().equals(
        pd.DataFrame({"max_time": [datetime(2020, 6, 22), datetime(2020, 7, 24)],
                      "min_time": [datetime(2020, 4, 21), datetime(2020, 5, 12)]}))

    # test failed response raises RuntimeError
    with pytest.raises(RuntimeError):
        covidcast.metadata()


@patch("delphi_epidata.Epidata.covidcast")
def test__fetch_single_geo(mock_covidcast):
    # not generating full DF since most attributes used
    mock_covidcast.side_effect = [{"result": 1,  # successful API response
                                   "epidata": [{'time_value': 20200622, 'issue': 20200724}],
                                   "message": "success"},
                                  {"result": 1,  # second successful API
                                   "epidata": [{'time_value': 20200821, 'issue': 20200925}],
                                   "message": "success"},
                                  {"message": "error: failed"},  # unsuccessful API response
                                  {"message": "success"}]  # no epidata

    # test happy path with 2 day range
    assert covidcast._fetch_single_geo(
        None, None, date(2020, 4, 2), date(2020, 4, 3), None, None, None, None, None).equals(
            pd.DataFrame({"time_value": [datetime(2020, 6, 22), datetime(2020, 8, 21)],
                          "issue": [datetime(2020, 7, 24), datetime(2020, 9, 25)]}, index=[0, 0]))

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
def test__signal_metadata(metadata):
    metadata.return_value = pd.DataFrame({"data_source": ["usa-facts", "doctor-visits"],
                                          "signal": ["raw_cli", "smooth_cli"],
                                          "time_type": ["day", "day"],
                                          "geo_type": ["hrr", "state"]})

    # test happy path
    assert covidcast._signal_metadata("usa-facts", "raw_cli", "hrr") == \
           {"data_source": "usa-facts", "signal": "raw_cli", "time_type": "day", "geo_type": "hrr"}

    # test no matches raises ValueError
    with pytest.raises(ValueError):
        covidcast._signal_metadata("not", "actual", "values")


def test__dates_to_api_strings():
    # test happy paths
    assert covidcast._dates_to_api_strings(date(2020, 4, 2)) == "20200402"
    assert covidcast._dates_to_api_strings([date(2020, 4, 2),
                                            date(2020, 5, 2)]) == "20200402-20200502"
