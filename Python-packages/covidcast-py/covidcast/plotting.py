"""This contains the plotting and geo data management methods for the COVIDcast signals."""

from datetime import date
from typing import Tuple

import geopandas as gpd
import numpy as np
import pandas as pd
import pkg_resources
from matplotlib import pyplot as plt
import matplotlib.figure

from .covidcast import _detect_metadata, _signal_metadata

SHAPEFILE_PATHS = {"county": "shapefiles/county/cb_2019_us_county_5m.shp",
                   "state": "shapefiles/state/cb_2019_us_state_5m.shp"}


def plot_choropleth(data: pd.DataFrame,
                    time_value: date = None,
                    **kwargs) -> matplotlib.figure.Figure:
    """Given the output data frame of :py:func:`covidcast.signal`, plot a choropleth map.

    Projections used for plotting:

    - ESRI:102003 (USA Contiguous Albers Equal Area Conic) for the contiguous US
      and Puerto Rico
    - ESRI:102006 (Alaska Albers Equal Area Conic) for Alaska
    - ESRI:102007 (Hawaii Albers Equal Area Conic) for Hawaii
      (Hawaii Albers Equal Area Conic) for Hawaii

    For visual purposes, Alaska and Hawaii are moved the lower left corner of the contiguous US
    and Puerto Rico is moved closer to Florida.

    By default, the `colormap
    <https://matplotlib.org/tutorials/colors/colormaps.html>`_ used is
    ``YlOrRd`` and is binned into the signal's historical mean value ± 3
    standard deviations. Custom arguments can be passed in as ``kwargs`` for
    customizability. These arguments will be passed to the GeoPandas ``plot``
    method; more information on these arguments can be found in `the GeoPandas
    documentation
    <https://geopandas.org/reference.html#geopandas.GeoDataFrame.plot>`_.

    :param data: Data frame of signal values, as returned from :py:func:`covidcast.signal`.
    :param time_value: If multiple days of data are present in ``data``, map only values from this
      day. Defaults to plotting the most recent day of data in ``data``.
    :param kwargs: Optional keyword arguments passed to ``GeoDataFrame.plot()``.
    :return: Matplotlib figure object.

    """

    data_source, signal, geo_type = _detect_metadata(data)  # pylint: disable=W0212
    meta = _signal_metadata(data_source, signal, geo_type)  # pylint: disable=W0212
    # use most recent date in data if none provided
    day_to_plot = time_value if time_value else max(data.time_value)
    day_data = data.loc[data.time_value == day_to_plot, :]
    data_w_geo = get_geo_df(day_data)

    kwargs["vmin"] = kwargs.get("vmin", 0)
    kwargs["vmax"] = kwargs.get("vmax", meta["mean_value"] + 3 * meta["stdev_value"])
    kwargs["cmap"] = kwargs.get("cmap", "YlOrRd")
    kwargs["figsize"] = kwargs.get("figsize", (12.8, 9.6))

    fig, ax = plt.subplots(1, figsize=kwargs["figsize"])
    ax.axis("off")
    sm = plt.cm.ScalarMappable(cmap=kwargs["cmap"],
                               norm=plt.Normalize(vmin=kwargs["vmin"], vmax=kwargs["vmax"]))
    # this is to remove the set_array error that occurs on some platforms
    sm._A = []  # pylint: disable=W0212
    plt.title(f"{data_source}: {signal}, {day_to_plot.strftime('%Y-%m-%d')}")
    for shape in _project_and_transform(data_w_geo):
        shape.plot("value", ax=ax, **kwargs)
    plt.colorbar(sm, ticks=np.linspace(kwargs["vmin"], kwargs["vmax"], 8), ax=ax,
                 orientation="horizontal", fraction=0.02, pad=0.05)
    return fig


def get_geo_df(data: pd.DataFrame,
               geo_value_col: str = "geo_value",
               geo_type_col: str = "geo_type",
               join_type: str = "right") -> gpd.GeoDataFrame:
    """Augment a :py:func:`covidcast.signal` data frame with the shape of each geography.

    This method takes in a pandas DataFrame object and returns a GeoDataFrame
    object from the `GeoPandas package <https://geopandas.org/>`_. The
    GeoDataFrame will contain the geographic shape corresponding to every row in
    its ``geometry`` colummn; for example, a data frame of county-level signal
    observations will be returned with the shape of every county.

    After detecting the geography type (only county and state are currently
    supported) of the input, this function builds a GeoDataFrame that contains
    state and geometry information from the Census for that geography type. By
    default, it will take the signal data (left side) and geo data (right side)
    and right join them, so all states/counties will always be present
    regardless of whether ``data`` contains values for those locations. ``left``,
    ``outer``, and ``inner`` joins are also supported and can be selected with
    the ``join_type`` argument.

    For right joins on counties, all counties without a signal value will be
    given the value of the megacounty (if present). Other joins will not use
    megacounties. See the `geographic coding documentation
    <https://cmu-delphi.github.io/delphi-epidata/api/covidcast_geography.html>`_
    for information about megacounties.

    By default, this function identifies the geography for each row of the input
    data frame using its ``geo_value`` column, matching data frames returned by
    :py:func:`covidcast.signal`, but the ``geo_value_col`` and ``geo_type_col``
    arguments can be provided to match geographies for data frames with
    different column names.

    Geographic data is sourced from 1:5,000,000-scale shapefiles from the `2019
    US Census Cartographic Boundary Files
    <https://www.census.gov/geographies/mapping-files/time-series/geo/cartographic-boundary.html>`_.

    :param data: DataFrame of values and geographies.
    :param geo_value_col: Name of column containing values of interest.
    :param geo_type_col: Name of column containing geography type.
    :param join_type: Type of join to do between input data (left side) and geo data (right side).
      Must be one of `right` (default), `left`, `outer`, or `inner`.
    :return: GeoDataFrame containing all columns from the input ``data``, along
      with a ``geometry`` column (containing a polygon) and a ``state_fips``
      column (a two-digit FIPS code identifying the US state containing this
      geography). The geometry is given in the GCS NAD83 coordinate system.

    """

    if join_type == "right" and any(data[geo_value_col].duplicated()):
        raise ValueError("join_type `right` is incompatible with duplicate values in a "
                         "given region. Use `left` or ensure your input data is a single signal for"
                         " a single date and geography type. ")
    geo_type = _detect_metadata(data, geo_type_col)[2]  # pylint: disable=W0212
    if geo_type not in ["state", "county"]:
        raise ValueError("Unsupported geography type; only state and county supported.")

    shapefile_path = pkg_resources.resource_filename(__name__, SHAPEFILE_PATHS[geo_type])
    geo_info = gpd.read_file(shapefile_path)

    if geo_type == "state":
        output = _join_state_geo_df(data, geo_value_col, geo_info, join_type)
    else:  # geo_type must be "county"
        output = _join_county_geo_df(data, geo_value_col, geo_info, join_type)
    return output


def _project_and_transform(data: gpd.GeoDataFrame) -> Tuple:
    """Segment and break GeoDF into Continental US, Alaska, Puerto Rico, and Hawaii for plotting.

    Given GeoDF with state fips column, break into Continental US, Alaska, Puerto Rico, and Hawaii
    GeoDFs with their own Albers Equal Area Conic Projections.

    Also scales and translates so Alaska and Hawaii are in the bottom left corner and Puerto Rico
    is closer to Hawaii.

    :param data: GeoDF with shape info and a column designating the state.
    :return: Tuple of four GeoDFs: Contiguous US, Alaska, Hawaii, and Puerto Rico.
    """
    cont = data.loc[[i not in ('02', '15', '72') for i in data.state_fips], :].to_crs(
        "ESRI:102003")
    alaska = data.loc[data.state_fips == '02', :].to_crs("ESRI:102006")
    pr = data.loc[data.state_fips == '72', :].to_crs("ESRI:102003")
    hawaii = data.loc[data.state_fips == '15', :].to_crs("ESRI:102007")

    alaska.geometry = alaska.geometry.scale(0.35, 0.35, origin=(0, 0)).translate(-1.8e6, -1.6e6)
    hawaii.geometry = hawaii.geometry.translate(-1e6, -2e6)
    pr.geometry = pr.geometry.translate(-0.9e6, 1e6).rotate(-16, origin=(0, 0))
    return cont, alaska, pr, hawaii


def _join_state_geo_df(data: pd.DataFrame,
                       state_col: str,
                       geo_info: gpd.GeoDataFrame,
                       join_type: str = "right") -> gpd.GeoDataFrame:
    """Join DF information to polygon information in a GeoDF at the state level.

    :param data: DF with state info
    :param state_col: cname of column in `data` containing state info to join on
    :param geo_info: GeoDF of state shape info read from Census shapefiles
    """
    input_cols = list(data.columns)
    geo_info.STUSPS = [i.lower() for i in geo_info.STUSPS]  # lowercase for consistency
    merged = data.merge(geo_info, how=join_type, left_on=state_col, right_on="STUSPS", sort=True)
    # use full state list in the return
    merged[state_col] = [j if pd.isna(i) else i for i, j in zip(merged.STUSPS, merged[state_col])]
    merged.rename(columns={"STATEFP": "state_fips"}, inplace=True)
    return gpd.GeoDataFrame(merged[input_cols + ["state_fips", "geometry"]])


def _join_county_geo_df(data: pd.DataFrame,
                        county_col: str,
                        geo_info: gpd.GeoDataFrame,
                        join_type: str = "right"
                        ) -> gpd.GeoDataFrame:
    """Join DF information to polygon information in a GeoDF at the county level.

    Counties with no direct key in the data DF will have the megacounty value joined.

    :param data: DF with county info
    :param county_col: name of column in `data` containing county info to join on
    :param geo_info: GeoDF of county shape info read from Census shapefiles
    """
    input_cols = list(data.columns)
    # create state FIPS code in copy, otherwise original gets modified
    data = data.assign(state=[i[:2] for i in data[county_col]])
    # join all counties with valid FIPS
    merged = data.merge(geo_info, how=join_type, left_on=county_col, right_on="GEOID",  sort=True)
    mega_county_df = data.loc[[i.endswith("000") for i in data[county_col]], :]
    if not mega_county_df.empty and join_type == "right":
        # if mega counties exist, join them on state
        merged = merged.merge(mega_county_df, how="left", left_on="STATEFP",
                              right_on="state", sort=True)
        # if no county value present, us the megacounty values
        for c in input_cols:
            merged[c] = [j if pd.isna(i) else i for i, j in zip(merged[f"{c}_x"], merged[f"{c}_y"])]
    # use the full county FIPS list in the return
    merged[county_col] = [j if pd.isna(i) else i for i, j in zip(merged.GEOID, merged[county_col])]
    merged.rename(columns={"STATEFP": "state_fips"}, inplace=True)
    return gpd.GeoDataFrame(merged[input_cols + ["state_fips", "geometry"]])
