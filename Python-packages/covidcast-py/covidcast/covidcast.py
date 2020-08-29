"""This is the client side library for accessing the COVIDcast API."""
import warnings
from datetime import timedelta, date
from typing import Union, Iterable, Tuple, List

import pandas as pd
from delphi_epidata import Epidata

# Point API requests to the AWS endpoint
Epidata.BASE_URL = "https://api.covidcast.cmu.edu/epidata/api.php"

VALID_GEO_TYPES = {"county", "hrr", "msa", "dma", "state"}


def signal(data_source: str,
           signal: str,  # pylint: disable=W0621
           start_day: date = None,
           end_day: date = None,
           geo_type: str = "county",
           geo_values: Union[str, Iterable[str]] = "*",
           as_of: date = None,
           issues: Union[date, Tuple[date], List[date]] = None,
           lag: int = None) -> Union[pd.DataFrame, None]:
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
      first day data is available for this signal.
    :param end_day: Query data up to this date, inclusive. Provided as a
      ``datetime.date`` object. If ``end_day`` is ``None``, defaults to the most
      recent day data is available for this signal.
    :param geo_type: The geography type for which to request this data, such as
      ``"county"`` or ``"state"``. Available types are described in the
      COVIDcast signal documentation. Defaults to ``"county"``.
    :param geo_values: The geographies to fetch data for. The default, ``"*"``,
      fetches all geographies. To fetch one geography, specify its ID as a
      string; multiple geographies can be provided as an iterable (list, tuple,
      ...) of strings.
    :param as_of: Fetch only data that was available on or before this date,
      provided as a ``datetime.date`` object. If ``None``, the default, return
      the most recent available data.
    :param issues: Fetch only data that was published or updated ("issued") on
      these dates. Provided as either a single ``datetime.date`` object,
      indicating a single date to fetch data issued on, or a tuple or list
      specifying (start, end) dates. In this case, return all data issued in
      this range. There may be multiple rows for each observation, indicating
      several updates to its value. If ``None``, the default, return the most
      recently issued data.
    :param lag: Integer. If, for example, ``lag=3``, fetch only data that was
      published or updated exactly 3 days after the date. For example, a row
      with ``time_value`` of June 3 will only be included in the results if its
      data was issued or updated on June 6. If ``None``, the default, return the
      most recently issued data regardless of its lag.
    :returns: A Pandas data frame with matching data. Each row is one
      observation on one day in one geographic location. Contains the following
      columns:

      ``geo_value``
        identifies the location, such as a state name or county FIPS code. The
        geographic coding used by COVIDcast is described in the `API
        documentation here
        <https://cmu-delphi.github.io/delphi-epidata/api/covidcast_geography.html>`_.

      ``time_value``
        contains a `pandas Timestamp object
        <https://pandas.pydata.org/pandas-docs/stable/reference/api/pandas.Timestamp.html>`_
        identifying the date this estimate is for.

      ``issue``
        contains a `pandas Timestamp object
        <https://pandas.pydata.org/pandas-docs/stable/reference/api/pandas.Timestamp.html>`_
        identifying the date this estimate was issued. For example, an estimate
        with a ``time_value`` of June 3 might have been issued on June 5, after
        the data for June 3rd was collected and ingested into the API.

      ``lag``
        an integer giving the difference between ``issue`` and ``time_value``,
        in days.

      ``value``
        the signal quantity requested. For example, in a query for the
        ``confirmed_cumulative_num`` signal from the ``usa-facts`` source,
        this would be the cumulative number of confirmed cases in the area, as
        of the ``time_value``.

      ``stderr``
        the value's standard error, if available.

      ``sample_size``
        indicates the sample size available in that geography on that day;
        sample size may not be available for all signals, due to privacy or
        other constraints.

      ``direction``
        uses a local linear fit to estimate whether the signal in this region is
        currently increasing or decreasing (reported as -1 for decreasing, 1 for
        increasing, and 0 for neither).

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
                         "start_day = '{start}', end_day = '{end}'".format(
                             start=start_day, end=end_day))

    if isinstance(geo_values, str):
        # User only provided one, not a list
        geo_values = [geo_values]

    dfs = [
        _fetch_single_geo(
            data_source, signal, start_day, end_day, geo_type, geo_value,
            as_of, issues, lag)  # type: ignore
        for geo_value in geo_values
    ]

    try:
        # pd.concat automatically filters out None
        out = pd.concat(dfs)
    except ValueError:
        # pd.concat raises ValueError if all of the dfs are None, meaning we
        # found no data
        return None

    return out


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

      ``min_time``
        First day for which this signal is available.

      ``max_time``
        Most recent day for which this signal is available.

      ``geo_type``
        Geographic level for which this signal is available, such as county,
        state, msa, or hrr. Most signals are available at multiple geographic
        levels and will hence be listed in multiple rows with their own
        metadata.

      ``time_type``
        Temporal resolution at which this signal is reported. "day", for
        example, means the signal is reported daily.

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

    """

    meta = Epidata.covidcast_meta()

    if meta["result"] != 1:
        # Something failed in the API and we did not get real metadata
        raise RuntimeError("Error when fetching metadata from the API",
                           meta["message"])

    meta_df = pd.DataFrame.from_dict(meta["epidata"])
    meta_df["min_time"] = pd.to_datetime(meta_df["min_time"], format="%Y%m%d")
    meta_df["max_time"] = pd.to_datetime(meta_df["max_time"], format="%Y%m%d")

    return meta_df


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


def _fetch_single_geo(data_source: str,
                      signal: str,  # pylint: disable=W0621
                      start_day: date,
                      end_day: date,
                      geo_type: str,
                      geo_value: str,
                      as_of: date,
                      issues: Union[date, tuple, list],
                      lag: int) -> Union[pd.DataFrame, None]:
    """Fetch data for a single geo.

    signal() wraps this to support fetching data over an iterable of
    geographies, and stacks the resulting data frames.

    If no data is found, return None, so signal() can easily filter out these
    entries.

    """

    as_of_str = _date_to_api_string(as_of) if as_of is not None else None
    issues_strs = _dates_to_api_strings(issues) if issues is not None else None

    cur_day = start_day

    dfs = []

    while cur_day <= end_day:
        day_str = _date_to_api_string(cur_day)

        day_data = Epidata.covidcast(data_source, signal, time_type="day",
                                     geo_type=geo_type, time_values=day_str,
                                     geo_value=geo_value, as_of=as_of_str,
                                     issues=issues_strs, lag=lag)

        # Two possible error conditions: no data or too much data.
        if day_data["message"] != "success":
            warnings.warn("Problem obtaining data on {day}: {message}".format(
                day=day_str,
                message=day_data["message"]))

        # In the too-much-data case, we continue to try putting the truncated
        # data in our results. In the no-data case, skip this day entirely,
        # since there is no "epidata" in the response.
        if "epidata" in day_data:
            dfs.append(pd.DataFrame.from_dict(day_data["epidata"]))

        cur_day += timedelta(1)

    if len(dfs) > 0:
        out = pd.concat(dfs)

        out["time_value"] = pd.to_datetime(out["time_value"], format="%Y%m%d")
        out["issue"] = pd.to_datetime(out["issue"], format="%Y%m%d")
        out["geo_type"] = geo_type
        out["data_source"] = data_source
        out["signal"] = signal
        return out

    return None


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
        raise ValueError("Unable to find metadata for source '{source}', "
                         "signal '{signal}', at '{geo_type}' "
                         "resolution.".format(
                             source=data_source,
                             signal=signal,
                             geo_type=geo_type))

    assert matches.shape[0] == 1, "it should be impossible to have two identical signals"

    return matches.to_dict("records")[0]


def _date_to_api_string(date: date) -> str:  # pylint: disable=W0621
    """Convert a date object to a YYYYMMDD string expected by the API."""

    return date.strftime("%Y%m%d")


def _dates_to_api_strings(dates: Union[date, list, tuple]) -> str:
    """Convert a date object, or pair of (start, end) objects, to YYYYMMDD strings."""

    if not isinstance(dates, (list, tuple)):
        return _date_to_api_string(dates)

    return "-".join(_date_to_api_string(date) for date in dates)
