"""Fetch data from Delphi's COVIDcast API.

The COVIDcast API provides daily updated data on the COVID-19 pandemic in the
United States, including cases, deaths, medical records, nationwide symptom
surveys, and other data collated by the Delphi research group at Carnegie Mellon
University.

Functions:

* signal - Fetch a Pandas data frame for one signal.
* metadata - Fetch metadata for all available signals.

"""

from .covidcast import signal, metadata
