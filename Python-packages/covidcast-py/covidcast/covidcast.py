"""This is the client side library for accessing the COVIDcast API."""
import warnings
from datetime import timedelta, date
from functools import reduce
from typing import Union, Iterable, Tuple, List

import pandas as pd
import numpy as np
from delphi_epidata import Epidata
from epiweeks import Week

from .errors import NoDataWarning

# Point API requests to the default endpoint
Epidata.BASE_URL = "https://api.covidcast.cmu.edu/epidata/api.php"

VALID_GEO_TYPES = {"county", "hrr", "msa", "dma", "state",  "hhs", "nation"}

_ASYNC_CALL = False

def use_api_key(key):
    """Set the API key to use for all subsequent queries.

    :param key: String containing the API key for you and/or your group.

    Anyone may access the Epidata API anonymously without providing an API key.
    Anonymous API access is currently rate-limited and with a maximum of two of
    the requested parameters having multiple selections (signals, dates, issues,
    regions, etc). To be exempt from these limits, use this function to apply an
    API key to all subsequent queries. You can register for an API key at
    <https://forms.gle/hkBr5SfQgxguAfEt7>.

    Consult the `API documentation
    <https://cmu-delphi.github.io/delphi-epidata/api/api_keys.html>`_
    for details on our API key policies.
    """
    Epidata.auth = key

def signal(data_source: str,
           signal: str,  # pylint: disable=W0621
           start_day: date = None,
           end_day: date = None,
           geo_type: str = "county",
           geo_values: Union[str, Iterable[str]] = "*",
           as_of: date = None,
           issues: Union[date, Tuple[date], List[date]] = None,
           lag: int = None,
           time_type: str = "day") -> Union[pd.DataFrame, None]:
    """Download a Pandas data frame for one signal.

    Obtains data for selected date ranges for all geographic regions of the
    United States. Available data sources and signals are documented in the
    `COVIDcast signal documentation
    <https://cmu-delphi.github.io/delphi-epidata/api/covidcast_signals.html>`_.
    Most (but not all) data sources are available at the county level, but the
    API can also return data aggregated to metropolitan statistical areas,
    hospital referral regions, or states, as desired, by using the ``geo_type``
    argument.

    The COVIDcast API tracks updates and changes to its underlying data, and
    records the first date each observation became available. For example, a
    data source may report its estimate for a specific state on June 3rd on June
    5th, once records become available. This data is considered "issued" on June
    5th. Later, the data source may update its estimate for June 3rd based on
    revised data, creating a new issue on June 8th. By default, ``signal()``
    returns the most recent issue available for every observation. The
    ``as_of``, ``issues``, and ``lag`` parameters allow the user to select
    specific issues instead, or to see all updates to observations. These
    options are mutually exclusive; if you specify more than one, ``as_of`` will
    take priority over ``issues``, which will take priority over ``lag``.

    Note that the API only tracks the initial value of an estimate and *changes*
    to that value. If a value was first issued on June 5th and never updated,
    asking for data issued on June 6th (using ``issues`` or ``lag``) would *not*
    return that value, though asking for data ``as_of`` June 6th would.

    Note also that the API enforces a maximum result row limit; results beyond
    the maximum limit are truncated. This limit is sufficient to fetch
    observations in all counties in the United States on one day. This client
    automatically splits queries for multiple days across multiple API calls.
    However, if data for one day has been issued many times, using the
    ``issues`` argument may return more results than the query limit. A warning
    will be issued in this case. To see all results, split your query across
    multiple calls with different ``issues`` arguments.

    See the `COVIDcast API documentation
    <https://cmu-delphi.github.io/delphi-epidata/api/covidcast.html>`_ for more
    information on available geography types, signals, and data formats, and
    further discussion of issue dates and data versioning.

    :param data_source: String identifying the data source to query, such as
      ``"fb-survey"``.
    :param signal: String identifying the signal from that source to query,
      such as ``"smoothed_cli"``.
    :param start_day: Query data beginning on this date. Provided as a
      ``datetime.date`` object. If ``start_day`` is ``None``, defaults to the
      first day data is available for this signal. If ``time_type == "week"``, then
      this is rounded to the epiweek containing the day (i.e. the previous Sunday).
    :param end_day: Query data up to this date, inclusive. Provided as a
      ``datetime.date`` object. If ``end_day`` is ``None``, defaults to the most
      recent day data is available for this signal. If ``time_type == "week"``, then
      this is rounded to the epiweek containing the day (i.e. the previous Sunday).
    :param geo_type: The geography type for which to request this data, such as
      ``"county"`` or ``"state"``. Available types are described in the
      COVIDcast signal documentation. Defaults to ``"county"``.
    :param geo_values: The geographies to fetch data for. The default, ``"*"``,
      fetches all geographies. To fetch one geography, specify its ID as a
      string; multiple geographies can be provided as an iterable (list, tuple,
      ...) of strings.
    :param as_of: Fetch only data that was available on or before this date,
      provided as a ``datetime.date`` object. If ``None``, the default, return
      the most recent available data. If ``time_type == "week"``, then
      this is rounded to the epiweek containing the day (i.e. the previous Sunday).
    :param issues: Fetch only data that was published or updated ("issued") on
      these dates. Provided as either a single ``datetime.date`` object,
      indicating a single date to fetch data issued on, or a tuple or list
      specifying (start, end) dates. In this case, return all data issued in
      this range. There may be multiple rows for each observation, indicating
      several updates to its value. If ``None``, the default, return the most
      recently issued data. If ``time_type == "week"``, then these are rounded to
      the epiweek containing the day (i.e. the previous Sunday).
    :param lag: Integer. If, for example, ``lag=3``, fetch only data that was
      published or updated exactly 3 days after the date. For example, a row
      with ``time_value`` of June 3 will only be included in the results if its
      data was issued or updated on June 6. If ``None``, the default, return the
      most recently issued data regardless of its lag.
    :param time_type: The temporal resolution to request this data. Most signals
      are available at the "day" resolution (the default); some are only
      available at the "week" resolution, representing an MMWR week ("epiweek").
    :returns: A Pandas data frame with matching data, or ``None`` if no data is
      returned. Each row is one observation on one day in one geographic location.
      Contains the following columns:

      ``geo_value``
        Identifies the location, such as a state name or county FIPS code. The
        geographic coding used by COVIDcast is described in the `API
        documentation here
        <https://cmu-delphi.github.io/delphi-epidata/api/covidcast_geography.html>`_.

      ``signal``
        Name of the signal, same as the value of the ``signal`` input argument. Used for
        downstream functions to recognize where this signal is from.

      ``time_value``
        Contains a `pandas Timestamp object
        <https://pandas.pydata.org/pandas-docs/stable/reference/api/pandas.Timestamp.html>`_
        identifying the date this estimate is for. For data with ``time_type = "week"``, this
        is the first day of the corresponding epiweek.

      ``issue``
        Contains a `pandas Timestamp object
        <https://pandas.pydata.org/pandas-docs/stable/reference/api/pandas.Timestamp.html>`_
        identifying the date this estimate was issued. For example, an estimate
        with a ``time_value`` of June 3 might have been issued on June 5, after
        the data for June 3rd was collected and ingested into the API.

      ``lag``
        Integer giving the difference between ``issue`` and ``time_value``,
        in days.

      ``value``
        The signal quantity requested. For example, in a query for the
        ``confirmed_cumulative_num`` signal from the ``usa-facts`` source,
        this would be the cumulative number of confirmed cases in the area, as
        of the ``time_value``.

      ``stderr``
        The value's standard error, if available.

      ``sample_size``
        Indicates the sample size available in that geography on that day;
        sample size may not be available for all signals, due to privacy or
        other constraints.

      ``geo_type``
        Geography type for the signal, same as the value of the ``geo_type`` input argument.
        Used for downstream functions to parse ``geo_value`` correctly

      ``data_source``
        Name of the signal source, same as the value of the ``data_source`` input argument. Used for
        downstream functions to recognize where this signal is from.

    Consult the `signal documentation
    <https://cmu-delphi.github.io/delphi-epidata/api/covidcast_signals.html>`_
    for more details on how values and standard errors are calculated for
    specific signals.

    """
    if geo_type not in VALID_GEO_TYPES:
        raise ValueError("geo_type must be one of " + ", ".join(VALID_GEO_TYPES))

    if start_day is None or end_day is None:
        signal_meta = _signal_metadata(data_source, signal, geo_type)

    start_day = signal_meta["min_time"].to_pydatetime().date() \
        if start_day is None else start_day

    end_day = signal_meta["max_time"].to_pydatetime().date() \
        if end_day is None else end_day

    if start_day > end_day:
        raise ValueError("end_day must be on or after start_day, but "
                         f"start_day = '{start_day}', end_day = '{end_day}'")
    if _ASYNC_CALL:
        dfs = _async_fetch_epidata(
            data_source, signal, start_day, end_day, geo_type,
            geo_values, as_of, issues, lag, time_type
        )
    else:
        dfs = _fetch_epidata(
            data_source, signal, start_day, end_day, geo_type,
            geo_values, as_of, issues, lag, time_type
        )
    if len(dfs) > 0:
        out = pd.concat(dfs)
        out.drop("direction", axis=1, inplace=True)
        out["time_value"] = out["time_value"].apply(lambda x: _parse_datetimes(x, time_type))
        out["issue"] = out["issue"].apply(lambda x: _parse_datetimes(x, time_type))
        out["geo_type"] = geo_type
        out["data_source"] = data_source
        out["signal"] = signal
        return out
    return None


def metadata() -> pd.DataFrame:
    """Fetch COVIDcast surveillance stream metadata.

    Obtains a data frame of metadata describing all publicly available data
    streams from the COVIDcast API. See the `data source and signals
    documentation
    <https://cmu-delphi.github.io/delphi-epidata/api/covidcast_signals.html>`_
    for descriptions of the available sources.

    :returns: A data frame containing one row per available signal, with the
      following columns:

      ``data_source``
        Data source name.

      ``signal``
        Signal name.

      ``time_type``
        Temporal resolution at which this signal is reported. "day", for
        example, means the signal is reported daily.

      ``geo_type``
        Geographic level for which this signal is available, such as county,
        state, msa, hss, hrr, or nation. Most signals are available at multiple geographic
        levels and will hence be listed in multiple rows with their own
        metadata.

      ``min_time``
        First day for which this signal is available. For weekly signals, will be
        the first day of the epiweek.

      ``max_time``
        Most recent day for which this signal is available. For weekly signals, will be
        the first day of the epiweek.

      ``num_locations``
        Number of distinct geographic locations available for this signal. For
        example, if `geo_type` is county, the number of counties for which this
        signal has ever been reported.

      ``min_value``
        The smallest value that has ever been reported.

      ``max_value``
        The largest value that has ever been reported.

      ``mean_value``
        The arithmetic mean of all reported values.

      ``stdev_value``
        The sample standard deviation of all reported values.

      ``last_update``
        The UTC datetime for when the signal value was last updated.

      ``max_issue``
        Most recent date data was issued.

      ``min_lag``
        Smallest lag from observation to issue, in days.

      ``max_lag``
        Largest lag from observation to issue, in days.
    """
    meta = Epidata.covidcast_meta()

    if meta["result"] != 1:
        # Something failed in the API and we did not get real metadata
        raise RuntimeError("Error when fetching metadata from the API",
                           meta["message"])

    meta_df = pd.DataFrame.from_dict(meta["epidata"])
    meta_df["min_time"] = meta_df.apply(lambda x: _parse_datetimes(x.min_time, x.time_type), axis=1)
    meta_df["max_time"] = meta_df.apply(lambda x: _parse_datetimes(x.max_time, x.time_type), axis=1)
    meta_df["last_update"] = pd.to_datetime(meta_df["last_update"], unit="s")
    return meta_df


def aggregate_signals(signals: list, dt: list = None, join_type: str = "outer") -> pd.DataFrame:
    """Given a list of DataFrames, [optionally] lag each one and join them into one DataFrame.

    This method takes a list of DataFrames containing signal information for
    geographic regions across time, and outputs a single DataFrame with a column
    for each signal value for each region/time. The ``data_source``,
    ``signal``, and index of each DataFrame in ``signals`` are appended to the
    front of each output column name separated by underscores (e.g.
    ``source_signal_0_inputcolumn``), and the original data_source and signal
    columns will be dropped. The input DataFrames must all be of the same
    geography type, and a single ``geo_type`` column will be returned in the final
    DataFrame.

    Each signal's time value can be shifted for analysis on lagged signals using the ``dt``
    argument, which takes a list of integer days to lag each signal's date. Lagging a signal by +1
    day means that all the dates get shifted forward by 1 day (e.g. Jan 1 becomes Jan 2).

    :param signals: List of DataFrames to join.
    :param dt: List of lags in days for each of the input DataFrames in ``signals``.
      Defaults to ``None``. When provided, must be the same length as ``signals``.
    :param join_type: Type of join to be done between the DataFrames in ``signals``.
      Defaults to ``"outer"``, so the output DataFrame contains all region/time
      combinations at which at least one signal was observed.
    :return: DataFrame of aggregated signals.

    """
    if dt is not None and len(dt) != len(signals):
        raise ValueError("Length of `dt` must be same as length of `signals`")
    dt = [0] * len(signals) if not dt else dt
    join_cols = ["time_value", "geo_value"]
    dt_dfs = []
    first_geo_type = _detect_metadata(signals[0])[2]

    for i, (df, lag) in enumerate(zip(signals, dt)):
        df_c = df.copy()  # make a copy so we don't modify originals
        source, sig_type, geo_type = _detect_metadata(df_c)
        if geo_type != first_geo_type:
            raise ValueError("Multiple geo_types detected. "
                             "All signals must have the same geo_type to be aggregated.")

        df_c["time_value"] = [day + timedelta(lag) for day in df_c["time_value"]]  # lag dates
        df_c.drop(["signal", "data_source", "geo_type"], axis=1, inplace=True)
        df_c.rename(
            columns={j: f"{source}_{sig_type}_{i}_{j}" for j in df_c.columns if j not in join_cols},
            inplace=True)
        dt_dfs.append(df_c)

    joined_df = reduce(lambda x, y: pd.merge(x, y, on=join_cols, how=join_type, sort=True), dt_dfs)
    joined_df["geo_type"] = geo_type
    return joined_df


def _parse_datetimes(date_int: int,
                     time_type: str,
                     date_format: str = "%Y%m%d") -> Union[pd.Timestamp]:  # annotating nan errors
    """Convert a date or epiweeks string into timestamp objects.

    Datetimes (length 8) are converted to their corresponding date, while epiweeks (length 6)
    are converted to the date of the start of the week. Returns nan otherwise

    Epiweeks use the CDC format.

    :param date_int: Int representation of date.
    :param date_format: String of the date format to parse.
    :returns: Timestamp.
    """
    date_str = str(date_int)
    if time_type == "day":
        return pd.to_datetime(date_str, format=date_format)
    if time_type == "week":
        epiwk = Week(int(date_str[:4]), int(date_str[-2:]))
        return pd.to_datetime(epiwk.startdate())
    return np.nan


def _detect_metadata(data: pd.DataFrame,
                     data_source_col: str = "data_source",
                     signal_col: str = "signal",
                     geo_type_col: str = "geo_type") -> Tuple:
    """Given a DataFrame, return the signal attributes of that DataFrame.

    Raises ValueError if any of the columns are heterogeneous.
    Inputs must have all three of the relevant columns: data source, signal, and geography type.

    :param data: DataFrame with data_source, signal, and geo_tye columns.
    :param data_source_col: name of column with data source info
    :param signal_col: name of column with signal info
    :param geo_type_col: name of column with geography type
    :return: tuple of the three types
    """
    unique_data_source_vals = data[data_source_col].unique()
    unique_signal_col_vals = data[signal_col].unique()
    unique_geo_type_vals = data[geo_type_col].unique()
    if len(unique_data_source_vals) > 1:
        raise ValueError("Multiple data sources detected.")
    if len(unique_signal_col_vals) > 1:
        raise ValueError("Multiple signals detected.")
    if len(unique_geo_type_vals) > 1:
        raise ValueError("Multiple geography types detected.")
    return unique_data_source_vals[0], unique_signal_col_vals[0], unique_geo_type_vals[0]


def _fetch_epidata(data_source: str,
                   signal: str,  # pylint: disable=W0621
                   start_day: date,
                   end_day: date,
                   geo_type: str,
                   geo_value: Union[str, Iterable[str]],
                   as_of: date,
                   issues: Union[date, tuple, list],
                   lag: int,
                   time_type: str = "day") -> Union[pd.DataFrame, None]:
    """Fetch data from Epidata API.

    signal() wraps this to support fetching data over a range of dates
    and stacks the resulting data frames.

    If no data is found, return None, so signal() can easily filter out these
    entries.

    """
    as_of_str = _date_to_api_string(as_of, time_type) if as_of is not None else None
    issues_strs = _dates_to_api_strings(issues, time_type) if issues is not None else None
    cur_day = start_day
    dfs = []
    while cur_day <= end_day:
        day_str = _date_to_api_string(cur_day, time_type)
        day_data = Epidata.covidcast(data_source, signal, time_type=time_type,
                                     geo_type=geo_type, time_values=day_str,
                                     geo_value=geo_value, as_of=as_of_str,
                                     issues=issues_strs, lag=lag)

        # Two possible error conditions: no data or too much data.
        if day_data["message"] == "no results":
            warnings.warn(f"No {data_source} {signal} data found on {day_str} "
                          f"for geography '{geo_type}'",
                          NoDataWarning)
        if day_data["message"] not in {"success", "no results"}:
            warnings.warn(f"Problem obtaining {data_source} {signal} data on {day_str} "
                          f"for geography '{geo_type}': {day_data['message']}",
                          RuntimeWarning)

        # In the too-much-data case, we continue to try putting the truncated
        # data in our results. In the no-data case, skip this day entirely,
        # since there is no "epidata" in the response.
        if day_data.get("epidata"):
            dfs.append(pd.DataFrame.from_dict(day_data["epidata"]))
        cur_day += timedelta(1) if time_type == "day" else timedelta(7)
    return dfs


def _async_fetch_epidata(data_source: str,
                         signal: str,  # pylint: disable=W0621
                         start_day: date,
                         end_day: date,
                         geo_type: str,
                         geo_value: Union[str, Iterable[str]],
                         as_of: date,
                         issues: Union[date, tuple, list],
                         lag: int,
                         time_type: str = "day") -> Union[pd.DataFrame, None]:
    """Fetch data from Epidata API asynchronously.

    signal() wraps this to support fetching data over a range of dates
    and stacks the resulting data frames.

    If no data is found, return None, so signal() can easily filter out these
    entries.
    """
    dfs = []
    params = []
    date_range = pd.date_range(start_day, end_day, freq="D" if time_type == "day" else "W")
    for day in date_range:
        day_param = {
            "source": "covidcast",
            "data_source": data_source,
            "signals": signal,
            "time_type": "day",
            "geo_type": geo_type,
            "geo_value": geo_value,
            "time_values": _date_to_api_string(day, time_type),
        }
        if as_of:
            day_param["as_of"] = _date_to_api_string(as_of, time_type)
        if issues:
            day_param["issues"] = _dates_to_api_strings(issues, time_type)
        if lag:
            day_param["lag"] = lag
        params.append(day_param)
    output = Epidata.async_epidata(params, batch_size=100)
    for day_data, params in output:
        if day_data["message"] == "no results":
            warnings.warn(f"No {data_source} {signal} data found on {params['time_values']} "
                          f"for geography '{geo_type}'", NoDataWarning)
        if day_data["message"] not in {"success", "no results"}:
            warnings.warn(f"Problem obtaining {data_source} {signal} "
                          f"data on {params['time_values']} "
                          f"for geography '{geo_type}': {day_data['message']}", RuntimeWarning)
        if day_data.get("epidata"):
            dfs.append(pd.DataFrame.from_dict(day_data["epidata"]))
    return dfs


def _signal_metadata(data_source: str,
                     signal: str,  # pylint: disable=W0621
                     geo_type: str) -> dict:
    """Fetch metadata for a single signal as a dict."""
    meta = metadata()

    mask = ((meta.data_source == data_source) &
            (meta.signal == signal) &
            (meta.time_type == "day") &
            (meta.geo_type == geo_type))

    matches = meta[mask]

    if matches.shape[0] == 0:
        raise ValueError(f"Unable to find metadata for source '{data_source}', "
                         f"signal '{signal}', at '{geo_type}' resolution.")

    assert matches.shape[0] == 1, "it should be impossible to have two identical signals"
    output: dict = matches.to_dict("records")[0]
    return output


def _date_to_api_string(date: date, time_type: str = "day") -> str:  # pylint: disable=W0621
    """Convert a date object to a YYYYMMDD or YYYYMM string expected by the API."""
    if time_type == "day":
        date_str = date.strftime("%Y%m%d")
    elif time_type == "week":
        date_str = Week.fromdate(date).cdcformat()
    return date_str


def _dates_to_api_strings(dates: Union[date, list, tuple], time_type: str = "day") -> str:
    """Convert a date object, or pair of (start, end) objects, to YYYYMMDD strings."""
    if not isinstance(dates, (list, tuple)):
        return _date_to_api_string(dates, time_type)

    return "-".join(_date_to_api_string(date, time_type) for date in dates)
