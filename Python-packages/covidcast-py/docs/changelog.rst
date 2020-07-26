Changelog
=========

v0.0.8, TODO DATE
-----------------

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
