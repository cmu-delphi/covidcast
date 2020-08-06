from datetime import date, datetime
from unittest.mock import patch

import pandas as pd
import pytest
from covidcast import covidcast


def test_signal():
    with pytest.raises(ValueError):
        covidcast.signal("source", "signal", geo_type="not_a_real_geo")
    # TODO Test happy path.


@patch("delphi_epidata.Epidata.covidcast_meta")
def test_metadata(mock_covidcast_meta):
    mock_covidcast_meta.side_effect = [{"result": 1,  # successful API response
                                        "epidata": [{'max_time': 20200622, 'min_time': 20200421},
                                                    {'max_time': 20200724, 'min_time': 20200512}],
                                        "message": "success"},
                                       {"result": 0,  # unsuccessful API response
                                        "epidata": [{'max_time': 20200622, 'min_time': 20200421},
                                                    {'max_time': 20200724, 'min_time': 20200512}],
                                        "message": "error: failed"}
                                       ]

    # test successful API response creates proper DF
    assert covidcast.metadata().equals(
        pd.DataFrame({"max_time": [datetime(2020, 6, 22), datetime(2020, 7, 24)],
                      "min_time": [datetime(2020, 4, 21), datetime(2020, 5, 12)]}))

    # test failed response raises RuntimeError
    with pytest.raises(RuntimeError):
        covidcast.metadata()


@patch("delphi_epidata.Epidata.covidcast")
def test__fetch_single_geo(mock_covidcast):
    mock_covidcast.side_effect = [{"result": 1,  # successful API response
                                   "epidata": [{'time_value': 20200622, 'issue': 20200724}],
                                   "message": "success"},
                                  {"result": 1,
                                   "epidata": [{'time_value': 20200821, 'issue': 20200925}],
                                   "message": "success"},
                                  {"message": "error: failed"},  # unsuccessful API response
                                  {"message": "success"}  # no epidata
                                  ]
    assert covidcast._fetch_single_geo(
        None, None, date(2020, 4, 2), date(2020, 4, 3), None, None, None, None, None).equals(
            pd.DataFrame({"time_value": [datetime(2020, 6, 22), datetime(2020, 8, 21)],
                          "issue": [datetime(2020, 7, 24), datetime(2020, 9, 25)]}, index=[0, 0])
        )

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


def test__dates_to_api_strings():
    # test happy path
    assert covidcast._dates_to_api_strings(date(2020, 4, 2)) == "20200402"
    assert covidcast._dates_to_api_strings([date(2020, 4, 2),
                                            date(2020, 5, 2)]) == "20200402-20200502"
