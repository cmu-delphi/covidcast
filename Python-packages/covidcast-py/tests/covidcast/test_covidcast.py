import pytest
from unittest.mock import patch
from datetime import date, datetime
import pandas as pd

from covidcast import covidcast


def test_wrong_geo_type():
    with pytest.raises(ValueError):
        covidcast.signal("source", "signal", geo_type="not_a_real_geo")


@patch("delphi_epidata.Epidata.covidcast_meta")
def test_metadata(mock_covidcast_meta):
    mock_covidcast_meta.side_effect = [{"result": 1,
                                        "epidata": [{'max_time': 20200622, 'min_time': 20200421},
                                                    {'max_time': 20200724, 'min_time': 20200512}],
                                        "message": "success"},
                                       {"result": 0,
                                        "epidata": [{'max_time': 20200622, 'min_time': 20200421},
                                                    {'max_time': 20200724, 'min_time': 20200512}],
                                        "message": "error: failed"}
                                       ]

    assert covidcast.metadata().equals(
        pd.DataFrame({"max_time": [datetime(2020, 6, 22), datetime(2020, 7, 24)],
                      "min_time": [datetime(2020, 4, 21), datetime(2020, 5, 12)]}))
    with pytest.raises(RuntimeError) as e:
        covidcast.metadata()


def test__dates_to_api_strings():
    """Test happy path."""
    assert covidcast._dates_to_api_strings(date(2020, 4, 2)) == "20200402"
    assert covidcast._dates_to_api_strings([date(2020, 4, 2), date(2020, 5, 2)]) == "20200402-20200502"
