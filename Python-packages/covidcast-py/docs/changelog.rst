Changelog
=========

v0.0.10, TODO
-------------

- New feature: Functions :py:func:`covidcast.fips_to_name`,
  :py:func:`covidcast.cbsa_to_name`, and :py:func:`covidcast.abbr_to_name` are
  provided to convert FIPS codes, CBSA IDs, and two-lettr abbreviations to
  county names, meeteropolitan statistical area names, and state names,
  respectively.

- New feature: Functions :py:func:`covidcast.name_to_fips`,
  :py:func:`covidcast.name_to_cbsa`, and :py:func:`covidcast.name_to_fips` are
  provided to convert human-readable names into FIPS codes, CBSA IDs, and state
  abbreviations, for use when querying the COVIDcast API.

v0.0.9, August 30, 2020
-----------------------

- New feature: :py:func:`covidcast.plot_choropleth` and :py:func:`covidcast.get_geo_df`
  add mapping and plotting capabilities that can be used with the data returned by
  :py:func:`covidcast.signal`. See the :ref:`function documentation <plotting-data>`
  and :ref:`plotting examples <plotting-examples>` for more details.

v0.0.8, July 26, 2020
---------------------

- New feature: :py:func:`covidcast.signal` now accepts arguments ``as_of``,
  ``issues``, and ``lag`` to request data that was issued on a specific date, or
  only the data that was available on a certain date. The default behavior is to
  return the only most up-to-date data, matching the client's previous behavior,
  but users can now request prior versions of data when desired. See the
  function documentation for more details.

v0.0.7, June 29, 2020
---------------------

- Addition: :py:func:`covidcast.metadata` returns a data frame of all available
  sources and signals, with metadata about each.

v0.0.6, June 22, 2020
---------------------

- Minor API change: :py:func:`covidcast.signal` now takes a ``geo_values``
  argument that can include multiple geographies, rather than a single
  ``geo_value`` argument.

- Improved documentation.

v0.0.5, June 19, 2020
---------------------

First public usable release.
