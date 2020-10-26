.. _plotting-data:

Plotting Reference
==================

Choropleth maps
---------------

This package provides a plotting function that takes a state, county, HRR, or MSA
signal and generates a choropleth map, using `matplotlib
<https://matplotlib.org/>`_ underneath. Detailed examples are provided in the
:ref:`usage examples <plotting-examples>`.

.. autofunction:: covidcast.plot

Animate a signal over time
--------------------------

A signal DataFrame can be used to generate an animated choropleth of the signal values over time.

.. autofunction:: covidcast.animate


Creating a GeoDataFrame
-----------------------

A function for generating a
`GeoPandas GeoDataFrame <https://geopandas.org/reference/geopandas.GeoDataFrame.html>`_ with signal information appended is also
provided if the user desires more control over their plotting.

.. autofunction:: covidcast.get_geo_df


