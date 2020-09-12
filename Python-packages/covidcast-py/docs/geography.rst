.. _plotting-examples:

Geographic Lookup
=================

County and MSA Lookup
---------------------

Functions are provided to map between county or metropolitan area names by FIPS or CBSA codes. By
default, they use regular expressions to match, but can use exact matches with the `fixed` argument.
Detailed examples are provided in the :ref:`usage examples <geographic-lookup>`.

.. autofunction:: covidcast.fips_to_name
.. autofunction:: covidcast.cbsa_to_name

If the opposite behavior is desired, use the following functions to map names to their respective
codes. These functions have an additional `state` argument that allows you to narrow you results to
a certain state.

.. autofunction:: covidcast.name_to_fips
.. autofunction:: covidcast.name_to_cbsa

State Abbreviation Lookup
-------------------------

Functions are also provided to map between state names and their two letter abbreviations.

.. autofunction:: covidcast.abbr_to_name
.. autofunction:: covidcast.name_to_abbr
