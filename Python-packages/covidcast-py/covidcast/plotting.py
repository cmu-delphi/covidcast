"""This contains the plotting and geo data management methods for the COVIDcast signals."""

import os
from datetime import date
from typing import Tuple

import geopandas as gpd
import numpy as np
import pandas as pd
from covidcast import covidcast
from matplotlib import pyplot as plt

SHAPEFILE_PATHS = {"county": "../shapefiles/county/cb_2019_us_county_5m.shp",
                   "state": "../shapefiles/state/cb_2019_us_state_5m.shp"}

CURRENT_PATH = os.path.dirname(os.path.realpath(__file__))


def plot_choropleth(data: pd.DataFrame,
                    time_value: date = None,
                    **kwargs) -> gpd.GeoDataFrame:
    """Given the output df of covidcast.signal(), plot the choropleth.

    Plot is in a style similar to the website https://covidcast.cmu.edu.

    :param data: DataFrame of values and geographies
    :param time_value: If multiple days of data are present in ``data``, map only values from this
    day. Defaults to plotting the most recent day of data in ``data``.
    :param kwargs: optional keyword arguments passed to plot()
    :return: GeoDF with polygon info. Same as get_geo_df()
    """
    data_source, signal, geo_type = covidcast._detect_metadata(data)  # pylint: disable=W0212
    meta = covidcast._signal_metadata(data_source, signal, geo_type)  # pylint: disable=W0212
    # use most recent date in data if none provided
    day_to_plot = time_value if time_value else max(data.time_value)
    day_data = data.loc[data.time_value == day_to_plot, :]
    data_w_geo = get_geo_df(day_data)
    fig, ax = plt.subplots(1, figsize=(12.8, 9.6))  # twice MPL default so text doesn't get squished
    ax.axis("off")

    kwargs["vmin"] = kwargs.get("vmin", 0)
    kwargs["vmax"] = kwargs.get("vmax", meta["mean_value"] + 3 * meta["stdev_value"])
    kwargs["cmap"] = kwargs.get("cmap", "YlOrRd")

    sm = plt.cm.ScalarMappable(cmap=kwargs["cmap"])
    plt.colorbar(sm, boundaries=np.linspace(kwargs["vmin"], kwargs["vmax"], 8),
                 orientation="horizontal", fraction=0.02, pad=0.05)
    plt.title(f"{data_source}: {signal}, {day_to_plot.strftime('%Y-%m-%d')}")
    for shape in _project_and_transform(data_w_geo):
        shape.plot("value", ax=ax, **kwargs)
    return fig


def get_geo_df(data: pd.DataFrame,
               geo_value_col: str = "geo_value",
               geo_type_col: str = "geo_type") -> gpd.GeoDataFrame:
    """Append polygons to a dataframe for a given geography.

    Default values correspond to return of
    covidcast.signal(). Currently only supports counties and states.

    :param data: DataFrame of values and geographies
    :param geo_value_col: name of column containing values of interest
    :param geo_type_col: name of column containing geography type
    """
    geo_type = covidcast._detect_metadata(data, geo_type_col)[2]  # pylint: disable=W0212
    output_cols = list(data.columns) + ["geometry", "state_fips"]
    shapefile_path = os.path.join(CURRENT_PATH, SHAPEFILE_PATHS[geo_type])
    geo_info = gpd.read_file(shapefile_path)

    if geo_type == "state":
        output = _join_state_geo_df(data, geo_value_col, geo_info)
    elif geo_type == "county":
        output = _join_county_geo_df(data, geo_value_col, geo_info)
    else:
        raise ValueError("Unsupported geography type.")
    output.rename(columns={"STATEFP": "state_fips"}, inplace=True)
    return output[output_cols]


def _project_and_transform(data: gpd.GeoDataFrame) -> Tuple:
    """Segment and break GeoDF into Continental US, Alaska, Puerto Rico, and Hawaii for plotting.

    Given GeoDF with state fips column, break into Continental US, Alaska, Puerto Rico, and Hawaii
    GeoDFs with their own Albers Equal Area Conic Projections.

    Projections:
    ESRI:102003 (USA Contiguous Albers Equal Area Conic) for the contiguous us and puerto rico
    ESRI:102006 (Alaska Albers Equal Area Conic) for alaska
    ESRI:102007 (Hawaii Albers Equal Area Conic) for hawaii

    Also scales and translates so Alaska and Hawiia are in the bottom left corner and Puerto Rico
    is closer to Hawaii.

    :param data: GeoDF with shape info and a column designating the state
    :return: tuple of GeoDFs, each value representing one region
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
