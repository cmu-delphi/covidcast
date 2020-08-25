"""This contains the plotting and geo data management methods for the COVIDcast signals."""

from datetime import date
from typing import Tuple

import geopandas as gpd
import numpy as np
import pandas as pd
import pkg_resources
from matplotlib import pyplot as plt

from .covidcast import _detect_metadata, _signal_metadata

SHAPEFILE_PATHS = {"county": "shapefiles/cb_2019_us_county_5m.zip",
                   "state": "shapefiles/cb_2019_us_state_5m.zip"}


def plot_choropleth(data: pd.DataFrame,
                    time_value: date = None,
                    **kwargs) -> gpd.GeoDataFrame:
    """Given the output df of covidcast.signal(), plot a choropleth map.

    Projections:
    ESRI:102003 (USA Contiguous Albers Equal Area Conic) for the contiguous US and Puerto Rico
    ESRI:102006 (Alaska Albers Equal Area Conic) for Alaska
    ESRI:102007 (Hawaii Albers Equal Area Conic) for Hawaii

    For visual purposes, Alaska and Hawaii are moved the lower left corner of the contiguous US
    and Puerto Rico is moved closer to Florida.

    By default, the colormap used is ``YlOrRd`` and is binned into the signal's mean value +- 3
    standard deviations. Custom arguments can be passed in as kwargs for customizability. More
    information on these arguments can be found in `the GeoPandas documentation
    <https://geopandas.org/reference.html#geopandas.GeoSeries.plot>`__

    :param data: DataFrame of values and geographies.
    :param time_value: If multiple days of data are present in ``data``, map only values from this \
        day. Defaults to plotting the most recent day of data in ``data``.
    :param kwargs: Optional keyword arguments passed to plot().
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
    plt.title(f"{data_source}: {signal}, {day_to_plot.strftime('%Y-%m-%d')}")
    for shape in _project_and_transform(data_w_geo):
        shape.plot("value", ax=ax, **kwargs)
    plt.colorbar(sm, ticks=np.linspace(kwargs["vmin"], kwargs["vmax"], 8), ax=ax,
                 orientation="horizontal", fraction=0.02, pad=0.05)
    return fig


def get_geo_df(data: pd.DataFrame,
               geo_value_col: str = "geo_value",
               geo_type_col: str = "geo_type") -> gpd.GeoDataFrame:
    """Append signal info to a GeoDataFrame with county/state geometries.

    This method takes in a pandas DataFrame object and returns a GeoDataFrame object from the
    `geopandas package <https://geopandas.org/>`__.

    Shapefiles are 1:5,000,000 scale and sourced from the `2019 US Census Cartographic Boundary Files
    <https://www.census.gov/geographies/mapping-files/time-series/geo/cartographic-boundary.html>`__


    After detecting the geography type (either county or state) for the input, loads the
    GeoDataFrame which contains state and geometry information from the Census for that geography
    type. Left joins the input data to this, so all states/counties will always be present
    regardless of the input i.e. the output dimension for a given geo_type is always the same
    regardless of input. Finally, returns the columns containing the input columns along with a
    `geometry` (polygon for plotting) and `state_fips` (FIPS code which will be used in the
    plotting function to rearrange AK and HI) column. Coordinate system is GCS NAD83.

    Default arguments for column names correspond to return of :py:func:`covidcast.signal`.
    Currently only supports counties and states.

    :param data: DataFrame of values and geographies.
    :param geo_value_col: Name of column containing values of interest.
    :param geo_type_col: Name of column containing geography type.
    :return: GeoDataFrame of all state and geometry info for given geo type w/ input data appended.
    """
    geo_type = _detect_metadata(data, geo_type_col)[2]  # pylint: disable=W0212
    output_cols = list(data.columns) + ["geometry", "state_fips"]

    shapefile_path = pkg_resources.resource_filename(__name__, SHAPEFILE_PATHS[geo_type])
    geo_info = gpd.read_file("zip://" + shapefile_path)

    if geo_type == "state":
        output = _join_state_geo_df(data, geo_value_col, geo_info)
    elif geo_type == "county":
        output = _join_county_geo_df(data, geo_value_col, geo_info)
    else:
        raise ValueError("Unsupported geography type; only state and county supported.")
    output.rename(columns={"STATEFP": "state_fips"}, inplace=True)
    return output[output_cols]


def _project_and_transform(data: gpd.GeoDataFrame) -> Tuple:
    """Segment and break GeoDF into Continental US, Alaska, Puerto Rico, and Hawaii for plotting.

    Given GeoDF with state fips column, break into Continental US, Alaska, Puerto Rico, and Hawaii
    GeoDFs with their own Albers Equal Area Conic Projections.

    Also scales and translates so Alaska and Hawaii are in the bottom left corner and Puerto Rico
    is closer to Hawaii.

<<<<<<< HEAD
    :param data: GeoDF with shape info and a column designating the state
    :return: Tuple of four GeoDFs: Contiguous US, Alaska, Hawaii, and Puerto Rico
=======
    :param data: GeoDF with shape info and a column designating the state.
    :return: Tuple of four GeoDFs: Contiguous US, Alaska, Hawaii, and Puerto Rico.
>>>>>>> plot_functions
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
                       geo_info: gpd.GeoDataFrame) -> gpd.GeoDataFrame:
    """Left join DF information to polygon information in a GeoDF at the state level.

    :param data: DF with state info
    :param state_col: cname of column in `data` containing state info to join on
    :param geo_info: GeoDF of state shape info read from Census shapefiles
    """
    geo_info.STUSPS = [i.lower() for i in geo_info.STUSPS]  # lowercase for consistency
    merged = geo_info.merge(data, how="left", left_on="STUSPS", right_on=state_col)
    # use full state list in the return
    merged[state_col] = merged.STUSPS
    return merged


def _join_county_geo_df(data: pd.DataFrame,
                        county_col: str,
                        geo_info: gpd.GeoDataFrame) -> gpd.GeoDataFrame:
    """Left join DF information to polygon information in a GeoDF at the county level.

    Counties with no direct key in the data DF will have the megacounty value joined.

    :param data: DF with county info
    :param county_col: name of column in `data` containing county info to join on
    :param geo_info: GeoDF of county shape info read from Census shapefiles
    """
    # create state FIPS code in copy, otherwise original gets modified
    data = data.assign(state=[i[:2] for i in data[county_col]])
    # join all counties with valid FIPS
    merged = geo_info.merge(data, how="left", left_on="GEOID", right_on=county_col)
    mega_county_df = data.loc[[i.endswith('000') for i in data[county_col]],
                              ["state", "value"]]
    if not mega_county_df.empty:
        # if mega counties exist, join them on state, and then use that value if no original signal
        merged = merged.merge(mega_county_df, how="left", left_on="STATEFP", right_on="state")
        merged["value"] = [j if pd.isna(i) else i for i, j in zip(merged.value_x, merged.value_y)]
    # use the full county FIPS list in the return
    merged[county_col] = merged.GEOID
    return merged
