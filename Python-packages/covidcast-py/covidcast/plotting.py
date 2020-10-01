"""This contains the plotting and geo data management methods for the COVIDcast signals."""

import io
from datetime import date, timedelta
from typing import Tuple, Any


import geopandas as gpd
import imageio
import matplotlib.figure
import numpy as np
import pandas as pd
import pkg_resources
from matplotlib import pyplot as plt
from tqdm import tqdm

from .covidcast import _detect_metadata, _signal_metadata

SHAPEFILE_PATHS = {"county": "shapefiles/county/cb_2019_us_county_5m.shp",
                   "state": "shapefiles/state/cb_2019_us_state_5m.shp",
                   "msa": "shapefiles/msa/cb_2019_us_cbsa_5m.shp",
                   "hrr": "shapefiles/hrr/geo_export_ad86cff5-e5ed-432e-9ec2-2ce8732099ee.shp"}

STATE_ABBR_TO_FIPS = {"AL": "01", "MN": "27", "ME": "23", "WA": "53", "LA": "22", "PA": "42",
                      "MD": "24", "CO": "08", "TN": "47", "MI": "26", "FL": "12", "VA": "51",
                      "IN": "18", "AS": "60", "HI": "15", "AZ": "04", "MO": "29", "SC": "45",
                      "DC": "11", "NM": "35", "MA": "25", "OR": "41", "MS": "28", "WI": "55",
                      "PR": "72", "NH": "33", "NV": "32", "GA": "13", "KY": "21", "NE": "31",
                      "WY": "56", "AK": "02", "OK": "40", "GU": "66", "DE": "10", "IA": "19",
                      "CA": "06", "VI": "78", "OH": "39", "NY": "36", "CT": "09", "AR": "05",
                      "VT": "50", "MP": "69", "MT": "30", "RI": "44", "WV": "54", "IL": "17",
                      "TX": "48", "UT": "49", "ND": "38", "KS": "20", "SD": "46", "NC": "37",
                      "NJ": "34", "ID": "16"}

# states within the contiguous US
CONTIGUOUS_FIPS = {"01", "04", "05", "06", "08", "09", "10", "11", "12", "13", "16", "17", "18",
                   "19", "20", "21", "22", "23", "24", "25", "26", "27", "28", "29", "30", "31",
                   "32", "33", "34", "35", "36", "37", "38", "39", "40", "41", "42", "44", "45",
                   "46", "47", "48", "49", "50", "51", "53", "54", "55", "56"}


def plot_choropleth(data: pd.DataFrame,
                    time_value: date = None,
                    **kwargs: Any) -> matplotlib.figure.Figure:
    """Given the output data frame of :py:func:`covidcast.signal`, plot a choropleth map.

    Projections used for plotting:

    - ESRI:102003 (USA Contiguous Albers Equal Area Conic) for the contiguous US
      and Puerto Rico
    - ESRI:102006 (Alaska Albers Equal Area Conic) for Alaska
    - ESRI:102007 (Hawaii Albers Equal Area Conic) for Hawaii
      (Hawaii Albers Equal Area Conic) for Hawaii

    For visual purposes, Alaska and Hawaii are moved the lower left corner of the contiguous US
    and Puerto Rico is moved closer to Florida.

    By default, choropleths use the `colormap
    <https://matplotlib.org/tutorials/colors/colormaps.html>`_
    ``YlOrRd``, with colors scaled between 0 and the signal's historical mean value + 3
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
    day_data = data.loc[data.time_value == pd.to_datetime(day_to_plot), :]
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

    # plot all states as light grey first
    state_shapefile_path = pkg_resources.resource_filename(__name__, SHAPEFILE_PATHS["state"])
    state = gpd.read_file(state_shapefile_path)
    for state in _project_and_transform(state, "STATEFP"):
        state.plot(color="0.9", ax=ax)

    for shape in _project_and_transform(data_w_geo):
        if not shape.empty:  # only plot nonempty ones to avoid matplotlib warning.
            shape.plot("value", ax=ax, **kwargs)
    plt.colorbar(sm, ticks=np.linspace(kwargs["vmin"], kwargs["vmax"], 8), ax=ax,
                 orientation="horizontal", fraction=0.045, pad=0.04, format="%.2f")
    return fig


def get_geo_df(data: pd.DataFrame,
               geo_value_col: str = "geo_value",
               geo_type_col: str = "geo_type",
               join_type: str = "right") -> gpd.GeoDataFrame:
    """Augment a :py:func:`covidcast.signal` data frame with the shape of each geography.

    This method takes in a pandas DataFrame object and returns a GeoDataFrame
    object from the `GeoPandas package <https://geopandas.org/>`_. The
    GeoDataFrame will contain the geographic shape corresponding to every row in
    its ``geometry`` column; for example, a data frame of county-level signal
    observations will be returned with the shape of every county.

    After detecting the geography type (state, county, HRR, and MSA are currently
    supported) of the input, this function builds a GeoDataFrame that contains
    state and geometry information from the Census or CMS for that geography type. By
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
    <https://www.census.gov/geographies/mapping-files/time-series/geo/cartographic-boundary.html>`_
    and the `CMS Data Website <https://data.cms.gov/widgets/ia25-mrsk>`_.

    :param data: DataFrame of values and geographies.
    :param geo_value_col: Name of column containing values of interest.
    :param geo_type_col: Name of column containing geography type.
    :param join_type: Type of join to do between input data (left side) and geo data (right side).
      Must be one of `right` (default), `left`, `outer`, or `inner`.
    :return: GeoDataFrame containing all columns from the input ``data``, along
      with a ``geometry`` column (containing a polygon) and a ``state_fips``
      column (a two-digit FIPS code identifying the US state containing this
      geography). For MSAs that span multiple states, the first state in the MSA name is provided.
      The geometry is given in the GCS NAD83 coordinate system for states, counties, and MSAs, and
      WGS84 for HRRs.

    """

    if join_type == "right" and any(data[geo_value_col].duplicated()):
        raise ValueError("join_type `right` is incompatible with duplicate values in a "
                         "given region. Use `left` or ensure your input data is a single signal for"
                         " a single date and geography type. ")
    geo_type = _detect_metadata(data, geo_type_col)[2]  # pylint: disable=W0212
    if geo_type not in ["state", "county", "msa", "hrr"]:
        raise ValueError("Unsupported geography type; "
                         "only `state`, `county`, `hrr`, and `msa` supported.")

    shapefile_path = pkg_resources.resource_filename(__name__, SHAPEFILE_PATHS[geo_type])
    geo_info = gpd.read_file(shapefile_path)

    if geo_type == "state":
        output = _join_state_geo_df(data, geo_value_col, geo_info, join_type)
    elif geo_type == "msa":
        output = _join_msa_geo_df(data, geo_value_col, geo_info, join_type)
    elif geo_type == "hrr":
        geo_info["geometry"] = geo_info["geometry"].translate(0, -0.185)  # fix projection shift bug
        output = _join_hrr_geo_df(data, geo_value_col, geo_info, join_type)
    else:  # geo_type must be "county"
        output = _join_county_geo_df(data, geo_value_col, geo_info, join_type)
    return output


def animate(data: pd.DataFrame, filepath: str, fps: int = 3, dpi: int = 150, **kwargs: Any) -> None:
    """Generate an animated video file of a signal over time.

    Given a signal DataFrame, generates the choropleth for each day to form an animation of the
    signal. Accepts arguments for video parameters as well as optional plotting arguments.
    Supported output formats are listed in the
    `imageio ffmpeg documentation <https://imageio.readthedocs.io/en/stable/format_ffmpeg.html>`_.

    :param data: DataFrame for a single signal over time.
    :param filepath: Path where video will be saved. Filename must contain supported extension.
    :param fps: Frame rate in frames per second for animation. Defaults to 3.
    :param dpi: Dots per inch for output video. Defaults to 150 on a 12.8x9.6 figure (1920x1440).
    :param kwargs: Optional keyword arguments passed to :py:func:`covidcast.plot_choropleth`.
    :return: None
    """
    # probesize is set to avoid warning by ffmpeg on frame rate up to 4k resolution.
    writer = imageio.get_writer(filepath, fps=fps, input_params=["-probesize", "75M"])
    num_days = (max(data.time_value) - min(data.time_value)).days
    day_list = [min(data.time_value) + timedelta(days=x) for x in range(num_days+1)]
    for d in tqdm(day_list):
        buf = io.BytesIO()
        plot_choropleth(data, time_value=d, **kwargs)
        plt.savefig(buf, dpi=dpi)
        plt.close()
        buf.seek(0)
        writer.append_data(imageio.imread(buf))
    writer.close()


def _project_and_transform(data: gpd.GeoDataFrame,
                           state_col: str = "state_fips") -> Tuple:
    """Segment and break GeoDF into Contiguous US, Alaska, Puerto Rico, and Hawaii for plotting.

    Given GeoDF with state fips column, break into Contiguous US, Alaska, Puerto Rico, and Hawaii
    GeoDFs with their own Albers Equal Area Conic Projections.

    Also scales and translates so Alaska and Hawaii are in the bottom left corner and Puerto Rico
    is closer to Hawaii.

    :param data: GeoDF with shape info and a column designating the state.
    :param state_col: Name of column with state FIPS codes.
    :return: Tuple of four GeoDFs: Contiguous US, Alaska, Hawaii, and Puerto Rico.
    """
    cont = data.loc[[i in CONTIGUOUS_FIPS for i in data[state_col]], :].to_crs("ESRI:102003")
    alaska = data.loc[data[state_col] == "02", :].to_crs("ESRI:102006")
    pr = data.loc[data[state_col] == "72", :].to_crs("ESRI:102003")
    hawaii = data.loc[data[state_col] == "15", :].to_crs("ESRI:102007")

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
    :param state_col: name of column in `data` containing state info to join on
    :param geo_info: GeoDF of state shape info read from Census shapefiles
    :return: ``data`` with state polygon and state FIPS joined.
    """
    input_cols = list(data.columns)
    geo_info.STUSPS = [i.lower() for i in geo_info.STUSPS]  # lowercase for consistency
    merged = data.merge(geo_info, how=join_type, left_on=state_col, right_on="STUSPS", sort=True)
    # use full state list in the return
    merged[state_col] = merged.STUSPS.combine_first(merged[state_col])
    merged.rename(columns={"STATEFP": "state_fips"}, inplace=True)
    return gpd.GeoDataFrame(merged[input_cols + ["state_fips", "geometry"]])


def _join_county_geo_df(data: pd.DataFrame,
                        county_col: str,
                        geo_info: gpd.GeoDataFrame,
                        join_type: str = "right") -> gpd.GeoDataFrame:
    """Join DF information to polygon information in a GeoDF at the county level.

    Counties with no direct key in the data DF will have the megacounty value joined.

    :param data: DF with county info.
    :param county_col: name of column in `data` containing county info to join on.
    :param geo_info: GeoDF of county shape info read from Census shapefiles.
    :return: ``data`` with county polygon and state fips joined.
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
            merged[c] = merged[f"{c}_x"].combine_first(merged[f"{c}_y"])
    # use the full county FIPS list in the return
    merged[county_col] = merged.GEOID.combine_first(merged[county_col])
    merged.rename(columns={"STATEFP": "state_fips"}, inplace=True)
    return gpd.GeoDataFrame(merged[input_cols + ["state_fips", "geometry"]])


def _join_msa_geo_df(data: pd.DataFrame,
                     msa_col: str,
                     geo_info: gpd.GeoDataFrame,
                     join_type: str = "right") -> gpd.GeoDataFrame:
    """Join DF information to polygon information in a GeoDF at the MSA level.

    For MSAs which span multiple states, the first state in the name is returned for the state FIPS.

    :param data: DF with state info
    :param msa_col: cname of column in `data` containing state info to join on
    :param geo_info: GeoDF of state shape info read from Census shapefiles
    :return: ``data`` with cbsa polygon and state fips joined.
    """
    geo_info = geo_info[geo_info.LSAD == "M1"]  # only get metro and not micropolitan areas
    input_cols = list(data.columns)
    merged = data.merge(geo_info, how=join_type, left_on=msa_col, right_on="GEOID", sort=True)
    # use full state list in the return
    merged[msa_col] = merged.GEOID.combine_first(merged[msa_col])
    # get the first state, which will be the first two characters after the comma and whitespace
    merged["state_fips"] = [STATE_ABBR_TO_FIPS.get(i.split(",")[1][1:3]) for i in merged.NAME]
    return gpd.GeoDataFrame(merged[input_cols + ["state_fips", "geometry"]])


def _join_hrr_geo_df(data: pd.DataFrame,
                     msa_col: str,
                     geo_info: gpd.GeoDataFrame,
                     join_type: str = "right") -> gpd.GeoDataFrame:
    """Join DF information to polygon information in a GeoDF at the HRR level.

    :param data: DF with state info
    :param msa_col: cname of column in `data` containing state info to join on
    :param geo_info: GeoDF of state shape info read from Census shapefiles
    :return: ``data`` with HRR polygon and state fips joined.
    """
    geo_info["hrr_num"] = geo_info.hrr_num.astype("int").astype(str)  # original col was a float
    input_cols = list(data.columns)
    merged = data.merge(geo_info, how=join_type, left_on=msa_col, right_on="hrr_num", sort=True)
    # use full state list in the return
    merged[msa_col] = merged.hrr_num.combine_first(merged[msa_col])
    # get the first state, which will be the first two characters in the HRR name
    merged["state_fips"] = [STATE_ABBR_TO_FIPS.get(i[:2]) for i in merged.hrr_name]
    return gpd.GeoDataFrame(merged[input_cols + ["state_fips", "geometry"]])
