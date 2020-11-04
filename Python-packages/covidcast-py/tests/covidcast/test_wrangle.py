import pytest
from datetime import date

import pandas as pd
import numpy as np

from covidcast import wrangle


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
    assert wrangle.aggregate_signals(
        [test_input1, test_input2, test_input3], dt=[0, 0, 1]).equals(expected1)

    # test 3 signals from 3 sources with inner join has no intersection
    assert wrangle.aggregate_signals(
        [test_input1, test_input3], dt=[0, 1], join_type="inner").empty

    # test 2 signals from same source (one lagged) with inner join
    expected2 = pd.DataFrame(
        {"geo_value": ["a"],
         "time_value": [date(2020, 1, 2)],
         "x_i_0_value": [8],
         "x_i_1_value": [2],
         "geo_type": ["state"]})
    assert wrangle.aggregate_signals(
        [test_input1, test_input1], dt=[0, 1], join_type="inner").equals(expected2)

    # test same signal twice with a lag
    expected3 = pd.DataFrame(
        {"geo_value": ["a", "b", "c", "a", "b", "c", "a"],
         "time_value": [date(2020, 1, 1), date(2020, 1, 1), date(2020, 1, 1), date(2020, 1, 2),
                        date(2020, 1, 2), date(2020, 1, 2), date(2020, 1, 3)],
         "x_i_0_value": [2, 4, 6, 8, np.nan, np.nan, np.nan],
         "x_i_1_value": [np.nan, np.nan, np.nan, 2, 4, 6, 8],
         "geo_type": ["state"]*7})

    assert wrangle.aggregate_signals([test_input1, test_input1], dt=[0, 1]).equals(expected3)

    # test long output
    expected4 = pd.DataFrame(
        {"geo_value": ["a", "b", "c", "a"]*2,
         "time_value": [date(2020, 1, 1), date(2020, 1, 1), date(2020, 1, 1), date(2020, 1, 2),
                        date(2020, 1, 2), date(2020, 1, 2), date(2020, 1, 2), date(2020, 1, 3)],
         "value": [2, 4, 6, 8]*2,
         "signal": ["i", "i", "i", "i"]*2,
         "geo_type": ["state", "state", "state", "state"]*2,
         "data_source": ["x", "x", "x", "x"]*2})

    assert wrangle.aggregate_signals([test_input1, test_input1],
                                     dt=[0, 1],
                                     output_format="long").equals(expected4)
    # test long output with different column names
    with pytest.raises(ValueError):
        wrangle.aggregate_signals([test_input1, test_input2], output_format="long")

    # test invalid lag length
    with pytest.raises(ValueError):
        wrangle.aggregate_signals([test_input1, test_input1], dt=[0])

    # test mixed geo_types
    test_input4 = pd.DataFrame(
        {"geo_value": ["b", "c", "d", "b"],
         "time_value": [date(2020, 1, 1), date(2020, 1, 1), date(2020, 1, 1), date(2020, 1, 2)],
         "value": [0.5, 1.5, 2.5, 3.5],
         "signal": ["k", "k", "k", "k"],
         "geo_type": ["county", "county", "county", "county"],
         "data_source": ["z", "z", "z", "z"]})
    with pytest.raises(ValueError):
        wrangle.aggregate_signals([test_input1, test_input4])
