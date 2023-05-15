"""Fetch data from Delphi's COVIDcast API.

The COVIDcast API provides daily updated data on the COVID-19 pandemic in the
United States, including cases, deaths, medical records, nationwide symptom
surveys, and other data collated by the Delphi research group at Carnegie Mellon
University.

Functions:

* signal - Fetch a Pandas data frame for one signal.
* metadata - Fetch metadata for all available signals.

"""

from .covidcast import signal, metadata, aggregate_signals, use_api_key
from .plotting import plot, plot_choropleth, get_geo_df, animate
from .geography import (fips_to_name, cbsa_to_name, abbr_to_name,
                        name_to_abbr, name_to_cbsa, name_to_fips,
                        fips_to_abbr, abbr_to_fips)
