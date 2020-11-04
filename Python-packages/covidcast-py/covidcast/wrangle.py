"""Functions for manipulating signal DataFrames."""
from datetime import timedelta
from functools import reduce

import pandas as pd

from .covidcast import _detect_metadata


def aggregate_signals(signals: list,
                      dt: list = None,
                      join_type: str = "outer",
                      output_format: str = "wide") -> pd.DataFrame:
    """Given a list of DataFrames, [optionally] lag each one and join them into one DataFrame.

    This method takes a list of DataFrames containing signal information for
    geographic regions across time, and outputs a single DataFrame of the signals aggregated
    with lags applied to signals if specified. The input DataFrames must all be of the same
    geography type.

    If ``output_format = 'wide'``, a DataFrame with a column
    for each signal value for each region/time. The ``data_source``,
    ``signal``, and index of each DataFrame in ``signals`` are appended to the
    front of each output column name separated by underscores (e.g.
    ``source_signal_0_inputcolumn``), and the original data_source and signal
    columns will be dropped. A single ``geo_type`` column will be returned in the final
    DataFrame.

    If ``output_format = 'wide'``, all input DataFrames must have the same columns,
    and the output will be the concatenation of all the lagged DataFrames.

    Each signal's time value can be shifted for analysis on lagged signals using the ``dt``
    argument, which takes a list of integer days to lag each signal's date. Lagging a signal by +1
    day means that all the dates get shifted forward by 1 day (e.g. Jan 1 becomes Jan 2).

    :param signals: List of DataFrames to join.
    :param dt: List of lags in days for each of the input DataFrames in ``signals``.
      Defaults to ``None``. When provided, must be the same length as ``signals``.
    :param join_type: Type of join to be done between the DataFrames in ``signals``.
      Defaults to ``"outer"``, so the output DataFrame contains all region/time
      combinations at which at least one signal was observed.
      Only applies if ``output_format='wide'``
    :param output_format: ``'wide'`` or ``'long'``. If ``wide``, a dataframe with a column
      per signal is returned. If ``long``, all signals are concatenated into one dataframe with
      a single column for the signal value.
    :return: DataFrame of aggregated signals.

    """
    if dt is not None and len(dt) != len(signals):
        raise ValueError("Length of `dt` must be same as length of `signals`")
    if output_format not in ["long", "wide"]:
        raise ValueError("`output_format` must be either 'long' or 'wide'")

    dt = [0] * len(signals) if not dt else dt
    first_geo_type = _detect_metadata(signals[0])[2]
    dt_dfs = []
    for df, lag in zip(signals, dt):
        source, sig_type, geo_type = _detect_metadata(df)
        if geo_type != first_geo_type:
            raise ValueError("Multiple geo_types detected. "
                             "All signals must have the same geo_type to be aggregated.")
        df_c = df.copy()  # make a copy so we don't modify originals
        df_c["time_value"] = [day + timedelta(lag) for day in df_c["time_value"]]  # lag dates
        dt_dfs.append((df_c, source, sig_type, geo_type))
    return _agg_wide(dt_dfs, join_type) if output_format == "wide" else _agg_long(dt_dfs)


def _agg_wide(processed_signals: list,
              join_type: str = "outer") -> pd.DataFrame:
    """Join together a list of signal DataFrames, renaming columns to prevent collisions.

    :param processed_signals: List of df and metadata tuples to join together.
    :param join_type: Type of join to conduct between all the DataFrames.
    :return: A single DataFrames which is the join of the input DataFrames.
    """
    join_cols = ["time_value", "geo_value"]
    for i, (df, source, sig_type, _) in enumerate(processed_signals):
        # drop and rename columns so the joined doesn't have duplicate and/or redundant columns.
        df.drop(["signal", "data_source", "geo_type"], axis=1, inplace=True)
        df.rename(
            columns={j: f"{source}_{sig_type}_{i}_{j}" for j in df.columns if j not in join_cols},
            inplace=True)
    dfs_to_join = [df for df, *_ in processed_signals]
    joined_df = reduce(lambda x, y: x.merge(y, on=join_cols, how=join_type, sort=True), dfs_to_join)
    joined_df["geo_type"] = processed_signals[0][-1]  # use geotype of first df
    return joined_df


def _agg_long(processed_signals: list) -> pd.DataFrame:
    """Concatenate a list of signal DataFrames with identical columns.

    :param processed_signals: List of DataFrame and metadata tuples to concatenate together.
    :return: Single DataFrames of all input signals concatenated
    """
    first_columns = processed_signals[0][0].columns
    for df, *_ in processed_signals:
        if any(df.columns != first_columns):
            raise ValueError("Inconsistent columns detected. All columns must be the same to use"
                             "'long' output.")
    return pd.concat([df for df, *_ in processed_signals]).reset_index(drop=True)
