"""This contains the plotting and geo data management methods for the COVIDcast signals.

Shapefiles are sourced from the 2019 US Census Cartographic Boundary Files
https://www.census.gov/geographies/mapping-files/time-series/geo/cartographic-boundary.html
Scale is 1:5,000,000
"""

import geopandas as gpd
import pandas as pd

import pkg_resources
from covidcast.covidcast import _detect_metadata

SHAPEFILE_PATHS = {"county": "shapefiles/cb_2019_us_county_5m.zip",
                   "state": "shapefiles/cb_2019_us_state_5m.zip"}


def get_geo_df(data: pd.DataFrame,
               geo_value_col: str = "geo_value",
               geo_type_col: str = "geo_type",
               join_type: str = "right") -> gpd.GeoDataFrame:
    """Append polygons to a dataframe for a given geography and return a geoDF with this info.

    This method takes in a pandas DataFrame object and returns a GeoDataFrame object from the
    `geopandas package <https://geopandas.org/>`__.

    After detecting the geography type (either county or state) for the input, loads the
    GeoDataFrame which contains state and geometry information from the Census for that geography
    type. By default, it will take the input data (left side) and geo data (right side) and right
    join them, so all states/counties will always be present regardless of the input i.e. the
    output dimension for a given geo_type is always the same regardless of input.
    `left`, `outer`, `inner` joins are also supported.

    For right joins on counties, all counties without a signal value will be given the value of
    the megacounty (if present). Other joins will not use megacounties.

    Returns the columns containing the input columns along with a
    `geometry` (polygon for plotting) and `state_fips` (FIPS code which will be used in the
    plotting function to rearrange AK and HI) column. Coordinate system is GCS NAD83.

    Default arguments for column names correspond to return of :py:func:`covidcast.signal`.
    Currently only supports counties and states.

    :param data: DataFrame of values and geographies
    :param geo_value_col: name of column containing values of interest
    :param geo_type_col: name of column containing geography type
    :param join_type: Type of join to do between input data (left side) and geo data (right side).
      must be one of `right`(default), `left`, `outer`, `inner`
    :return: GeoDataFrame of all state and geometry info for given geo type w/ input data appended.
    """
    if join_type == "right" and any(data[geo_value_col].duplicated()):
        raise ValueError("join_type `right` is incompatible with duplicate values in a "
                         "given region. Use `left` or ensure your input data is a single signal for"
                         "a single date and geography type. ")
    geo_type = _detect_metadata(data, geo_type_col)[2]  # pylint: disable=W0212
    if geo_type not in ["state", "county"]:
        raise ValueError("Unsupported geography type; only state and county supported.")

    output_cols = list(data.columns) + ["geometry", "state_fips"]
    shapefile_path = pkg_resources.resource_filename(__name__, SHAPEFILE_PATHS[geo_type])
    geo_info = gpd.read_file("zip://" + shapefile_path)

    if geo_type == "state":
        output = _join_state_geo_df(data, geo_value_col, geo_info, join_type)
    else:  # geo_type must be "county"
        output = _join_county_geo_df(data, geo_value_col, geo_info, join_type)
    output.rename(columns={"STATEFP": "state_fips"}, inplace=True)
    return output[output_cols]


def _join_state_geo_df(data: pd.DataFrame,
                       state_col: str,
                       geo_info: gpd.GeoDataFrame,
                       join_type: str = "right") -> gpd.GeoDataFrame:
    """Join DF information to polygon information in a GeoDF at the state level.

    :param data: DF with state info
    :param state_col: cname of column in `data` containing state info to join on
    :param geo_info: GeoDF of state shape info read from Census shapefiles
    """
    geo_info.STUSPS = [i.lower() for i in geo_info.STUSPS]  # lowercase for consistency
    merged = data.merge(geo_info, how=join_type, left_on=state_col, right_on="STUSPS",)
    # use full state list in the return
    merged[state_col] = [j if pd.isna(i) else i for i, j in zip(merged.STUSPS, merged[state_col])]
    return merged


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
    # create state FIPS code in copy, otherwise original gets modified
    data = data.assign(state=[i[:2] for i in data[county_col]])
    # join all counties with valid FIPS
    merged = data.merge(geo_info, how=join_type, left_on=county_col, right_on="GEOID")
    mega_county_df = data.loc[[i.endswith("000") for i in data[county_col]],
                              ["state", "value"]]
    if not mega_county_df.empty and join_type == "right":
        # if mega counties exist, join them on state, and then use that value if no original signal
        merged = merged.merge(mega_county_df, how="left", left_on="STATEFP", right_on="state")
        merged["value"] = [j if pd.isna(i) else i for i, j in zip(merged.value_x, merged.value_y)]
    # use the full county FIPS list in the return
    merged[county_col] = [j if pd.isna(i) else i for i, j in zip(merged.GEOID, merged[county_col])]
    return merged
