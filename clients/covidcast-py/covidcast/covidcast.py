
import warnings
from datetime import timedelta, datetime

from delphi_epidata import Epidata

import pandas as pd

VALID_GEO_TYPES = {"county", "hrr", "msa", "dma", "state"}

def signal(data_source, signal, start_day=None, end_day=None,
           geo_type="county", geo_values="*"):
    """Download a Pandas data frame for one signal.

    Obtains data for selected date ranges for all geographic regions of the
    United States. Available data sources and signals are documented in the
    `COVIDcast signal documentation
    <https://cmu-delphi.github.io/delphi-epidata/api/covidcast_signals.html>`_.
    Most (but not all) data sources are available at the county level, but the
    API can also return data aggregated to metropolitan statistical areas,
    hospital referral regions, or states, as desired, by using the ``geo_type``
    argument.

    See the `COVIDcast API documentation
    <https://cmu-delphi.github.io/delphi-epidata/api/covidcast.html>`_ for more
    information on available geography types, signals, and data formats.

    :param data_source: String identifying the data source to query, such as
      ``"fb-survey"``.
    :param signal: String identifying the signal from that source to query,
      such as ``"smoothed_cli"``.
    :param start_day: Query data beginning on this date. Provided as a
      ``datetime.date`` object. If ``start_day`` is ``None``, defaults to the
      first day data is available for this signal.
    :param end_day: Query data up to this date, inclusive. ``datetime.date``
      object. If ``end_day`` is ``None``, defaults to the most recent day data
      is available for this signal.
    :param geo_type: The geography type for which to request this data, such as
      ``"county"`` or ``"state"``. Available types are described in the
      COVIDcast signal documentation. Defaults to ``"county"``.
    :param geo_values: The geographies to fetch data for. The default, ``"*"``,
      fetches all geographies. To fetch one geography, specify its ID as a
      string; multiple geographies can be provided as an iterable (list, tuple,
      ...) of strings.
    :returns: A Pandas data frame with matching data. Contains ``geo_value``,
      ``time_value``, ``direction``, ``value``, ``stderr``, and ``sample_size``
      columns. ``geo_value`` identifies the location, such as a state name or
      county FIPS code; ``time_value`` contains pandas ``Timestamp`` objects.
      ``value`` is the signal quantity requested and ``stderr`` its standard
      error if available. ``sample_size`` indicates the sample size available in
      that geography on that day; sample size may not be available.
      ``direction`` uses a local linear fit to estimate whether the signal in
      this region is currently increasing or decreasing. Consult the signal
      documentation for more details.

    """

    if geo_type not in VALID_GEO_TYPES:
        raise ValueError("geo_type must be one of " + ", ".join(VALID_GEO_TYPES))

    if start_day is None or end_day is None:
        signal_meta = _signal_metadata(data_source, signal, geo_type)

    start_day = signal_meta["min_time"].to_pydatetime().date() \
        if start_day is None else start_day

    end_day = signal_meta["max_time"].to_pydatetime().date() \
        if end_day is None else end_day

    ## The YYYYMMDD format lets us use lexicographic ordering to test date order
    if start_day > end_day:
        raise ValueError("end_day must be on or after start_day, but "
                         "start_day = '{start}', end_day = '{end}'".format(
                             start=start_day, end=end_day))

    if isinstance(geo_values, str):
        ## User only provided one, not a list
        geo_values = [geo_values]

    dfs = [
        _fetch_single_geo(
            data_source, signal, start_day, end_day, geo_type, geo_value)
        for geo_value in geo_values
    ]

    try:
        # pd.concat automatically filters out None
        out = pd.concat(dfs)
    except ValueError:
        ## pd.concat raises ValueError if all of the dfs are None, meaning we
        ## found no data
        return None

    return out

def metadata():
    """Fetch COVIDcast surveillance stream metadata.

    Obtains a data frame of metadata describing all publicly available data
    streams from the COVIDcast API. See the `data source and signals
    documentation
    <https://cmu-delphi.github.io/delphi-epidata/api/covidcast_signals.html>`_
    for descriptions of the available sources.

    The returned data frame contains one row per available signal, with the
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
        ## Something failed in the API and we did not get real metadata
        raise RuntimeError("Error when fetching metadata from the API",
                           meta["message"])


    meta_df = pd.DataFrame.from_dict(meta["epidata"])
    meta_df["min_time"] = pd.to_datetime(meta_df["min_time"], format="%Y%m%d")
    meta_df["max_time"] = pd.to_datetime(meta_df["max_time"], format="%Y%m%d")

    return meta_df


def _fetch_single_geo(data_source, signal, start_day, end_day, geo_type,
                      geo_value):
    """Fetch data for a single geo.

    signal() wraps this to support fetching data over an iterable of
    geographies, and stacks the resulting data frames.

    If no data is found, return None, so signal() can easily filter out these
    entries.

    """

    cur_day = start_day

    dfs = []

    while cur_day <= end_day:
        day_str = _date_to_api_string(cur_day)

        day_data = Epidata.covidcast(data_source, signal, time_type="day",
                                     geo_type=geo_type, time_values=day_str,
                                     geo_value=geo_value)

        if day_data["message"] != "success":
            warnings.warn("Problem obtaining data on {day}: {message}".format(
                day=day_str,
                message=day_data["message"]))
        else:
            dfs.append(pd.DataFrame.from_dict(day_data["epidata"]))

        cur_day += timedelta(1)

    if len(dfs) > 0:
        out = pd.concat(dfs)

        out["time_value"] = pd.to_datetime(out["time_value"], format="%Y%m%d")

        return out

    return None

def _signal_metadata(data_source, signal, geo_type):
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

def _date_to_api_string(date):
    """Convert a date object to a YYYYMMDD string expected by the API."""

    return date.strftime("%Y%m%d")
