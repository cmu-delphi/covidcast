.. _plotting-data:

Plotting Reference
==================

Choropleth maps
---------------

This package provides a plotting function that takes a county or state level signal and
generates a choropleth map. Detailed examples are provided in the :ref:`usage examples
<plotting-examples>`.

.. autofunction:: covidcast.plot_choropleth

Creating a GeoDataFrame
-----------------------

A function for generating a
`GeoPandas GeoDataFrame <https://geopandas.org/reference/geopandas.GeoDataFrame.html>`_ with signal information appended is also
provided if the user desires more control over their plotting.

.. autofunction:: covidcast.get_geo_df
