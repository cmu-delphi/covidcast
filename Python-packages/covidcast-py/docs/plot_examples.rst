.. _plotting-examples:

Plotting Examples
=================

Built-in Functionality
----------------------
The returned DataFrame from :py:func:`covidcast.signal` can be plotted using the built-in
:py:func:`covidcast.plot_choropleth`:

>>> from covidcast import covidcast, plotting
>>> from datetime import date
>>> from matplotlib import pyplot as plt
>>> data = covidcast.signal("fb-survey", "smoothed_cli",
                             date(2020,8,3), date(2020,8,4),
                             geo_type = "county")
>>> plotting.plot_choropleth(data)
>>> plt.show()

.. plot::

    import covidcast
    from datetime import date
    from matplotlib import pyplot as plt
    data = covidcast.signal("fb-survey", "smoothed_cli", start_day = date(2020,8,3), end_day = date(2020,8,4), geo_type = "county")
    covidcast.plot_choropleth(data)
    plt.show()

Additional keyword arguments can also be provided. These correspond to most of the arguments
available for the
`GeoPandas plot() function <https://geopandas.org/reference.html#geopandas.GeoSeries.plot>`_.


>>> plotting.plot_choropleth(data,
                             cmap ="viridis",
                             figsize = (10,5),
                             edgecolor="0.8")
>>> plt.show()

.. plot::

    import covidcast
    from datetime import date
    from matplotlib import pyplot as plt
    data = covidcast.signal("fb-survey","smoothed_cli", start_day = date(2020,8,3), end_day = date(2020,8,4), geo_type = "county")
    covidcast.plot_choropleth(data, cmap ="viridis", figsize = (10,5), edgecolor="0.8")
    plt.show()

The function returns a
`Matplotlib Figure Object <https://matplotlib.org/api/_as_gen/matplotlib.figure.Figure.html#matplotlib.figure.Figure>`_
which can be stored and altered further.

>>> fig = plotting.plot_choropleth(data, cmap ="viridis", figsize = (10,5), edgecolor="0.8")
>>> fig.set_dpi(100)

Further Customization
---------------------
If more control is desired, the signal data can be passed to :py:func:`covidcast.get_geo_df`, which
will return  a
`GeoPandas GeoDataFrame <https://geopandas.org/reference/geopandas.GeoDataFrame.html>`_,
that can be used with the `mappings tools <https://geopandas.org/mapping.html>`_
provided by that package.

For example, plotting California with a Mercator projection:

>>> data = covidcast.signal("fb-survey", "smoothed_cli",
                             date(2020,8,3), date(2020,8,4),
                             geo_type = "county")
>>> geo_data =  covidcast.get_geo_df(data)
>>> CA = geo_data.loc[geo_data.state_fips == "06",:]
>>> CA.to_crs("EPSG:3395")
>>> CA.plot(column="true_value", figsize=(5,5), legend=True)
>>> plt.axis("off")
>>> plt.show()

.. plot::

    import covidcast
    from datetime import date
    from matplotlib import pyplot as plt
    data = covidcast.signal("fb-survey","smoothed_cli", start_day = date(2020,8,3), end_day = date(2020,8,4), geo_type = "county")
    geo_data = covidcast.get_geo_df(data)
    CA = geo_data.loc[geo_data.state_fips == "06",:]
    CA = CA.to_crs("EPSG:3395")
    CA.plot(column="value", figsize=(5,5), legend=True)
    plt.axis("off")
    plt.show()
