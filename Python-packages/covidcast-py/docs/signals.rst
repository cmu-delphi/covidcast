Fetching Data
=============

Signals
-------

This package provides a key function to obtain any signal of interest as a
Pandas data frame. Detailed examples are provided in the :ref:`usage examples
<getting-started>`.

.. autofunction:: covidcast.signal

Sometimes you would like to work with multiple signals -- for example, to obtain
several signals at every location, as part of building models of features at
each location. For convenience, the package provides a function to produce a
single data frame containing multiple signals at each location.

.. autofunction:: covidcast.aggregate_signals

                  
Metadata
--------

Many data sources and signals are available, so one can also obtain a data frame
of all signals and their associated metadata:

.. autofunction:: covidcast.metadata

.. _working-with-geos:

Working with geographic identifiers
-----------------------------------

The COVIDcast API identifies each geographic region -- such as a county or state
-- using unique codes. For example, counties are identified by their FIPS codes,
while states are identified by a two-letter abbreviation; more detail is given
in the `geographic coding documentation
<https://cmu-delphi.github.io/delphi-epidata/api/covidcast_geography.html>`_.

When fetching data from the API, you may need to quickly convert between
human-readable names and unique identifiers. The following functions are
provided for your convenience.

Counties
^^^^^^^^

.. autofunction:: covidcast.fips_to_name

.. autofunction:: covidcast.name_to_fips

Metropolitan statistical areas
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. autofunction:: covidcast.cbsa_to_name

.. autofunction:: covidcast.name_to_cbsa

States
^^^^^^

.. autofunction:: covidcast.abbr_to_name

.. autofunction:: covidcast.name_to_abbr

.. autofunction:: covidcast.abbr_to_fips

.. autofunction:: covidcast.fips_to_abbr
