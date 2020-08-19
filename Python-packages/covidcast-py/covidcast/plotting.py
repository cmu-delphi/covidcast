"""This contains the plotting and geo data management methods for the COVIDcast signals."""

import os

import geopandas as gpd
import pandas as pd

from covidcast import covidcast

SHAPEFILE_PATHS = {"county": "../shapefiles/county/cb_2019_us_county_5m.shp",
                   "state": "../shapefiles/state/cb_2019_us_state_5m.shp"}

CURRENT_PATH = os.path.dirname(os.path.realpath(__file__))


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
        raise ValueError("Unsupported geography type; only state and county supported.")
    output.rename(columns={"STATEFP": "state_fips"}, inplace=True)
    return output[output_cols]


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
