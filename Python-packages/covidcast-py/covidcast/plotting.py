"""This contains the plotting and geo data management methods for the COVIDcast signals."""

import os
from datetime import date

import geopandas as gpd
import pandas as pd

SHAPEFILE_PATHS = {"county": "../shapefiles/county/cb_2019_us_county_5m.shp",
                   "state": "../shapefiles/state/cb_2019_us_state_5m.shp"}

CURRENT_PATH = os.path.dirname(os.path.realpath(__file__))


def get_geo_df(data: pd.DataFrame, geo_type: str, time_value: date = None) -> gpd.GeoDataFrame:
    day_to_plot = time_value if time_value else max(data.time_value)
    day_data = data.loc[data.time_value == day_to_plot, :]
    output_cols = list(data.columns) + ["geometry"]
    shapefile_path = os.path.join(CURRENT_PATH, SHAPEFILE_PATHS[geo_type])
    geo_info = gpd.read_file(shapefile_path)
    if geo_type == "state":
        output = _join_state_geo_df(day_data, geo_info)
    elif geo_type == "county":
        output = _join_county_geo_df(day_data, geo_info)
    return output[output_cols]


def _join_state_geo_df(data: pd.DataFrame, geo_info: gpd.GeoDataFrame) -> gpd.GeoDataFrame:
    geo_info.STUSPS = [i.lower() for i in data.geo_value]  # lowercase for consistency
    merged = geo_info.merge(data, how="left", left_on="STUSPS", right_on="geo_value")
    # use full state list in the return
    merged.geo_value = merged.STUSPS
    return merged


def _join_county_geo_df(data: pd.DataFrame, geo_info: gpd.GeoDataFrame) -> gpd.GeoDataFrame:
    data["state_fips"] = [i[:2] for i in data.geo_value]  # create state FIPS code
    # join all counties with valid FIPS
    merged = geo_info.merge(data, how="left", left_on="GEOID", right_on="geo_value")
    mega_county_df = data.loc[[i.endswith('000') for i in data.geo_value], ["state_fips", "value"]]
    if not mega_county_df.empty:
        # if mega counties exist, join them on state, and then use that value is no original signal
        merged = merged.merge(mega_county_df, how="left", left_on="STATEFP", right_on="state_fips")
        merged["value"] = [j if pd.isna(i) else i for i, j in zip(merged.value_x, merged.value_y)]
    # use the full county FIPS list in the return
    merged.geo_value = merged.GEOID
    return merged
