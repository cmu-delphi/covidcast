.. _plotting-examples:

Plotting Examples
=================

Built-in Functionality
----------------------
The returned DataFrame from :py:func:`covidcast.signal` can be plotted using the built-in
:py:func:`covidcast.plot_choropleth`. Currently only state and county geography types are supported.

>>> from covidcast import covidcast, plotting
>>> from datetime import date
>>> from matplotlib import pyplot as plt
>>> data = covidcast.signal("fb-survey", "smoothed_cli",
...                          date(2020, 8, 3), date(2020, 8, 4),
...                          geo_type="county")
>>> plotting.plot_choropleth(data)
>>> plt.show()

.. plot::

    import covidcast
    from datetime import date
    from matplotlib import pyplot as plt
    data = covidcast.signal("fb-survey", "smoothed_cli", start_day = date(2020,8,3), end_day = date(2020,8,4), geo_type = "county")
    covidcast.plot_choropleth(data)
    plt.show()

>>> data = covidcast.signal("fb-survey", "smoothed_cli",
...                          date(2020, 8, 3), date(2020, 8, 4),
...                          geo_type="state")
>>> plotting.plot_choropleth(data)
>>> plt.show()

.. plot::

    import covidcast
    from datetime import date
    from matplotlib import pyplot as plt
    data = covidcast.signal("fb-survey", "smoothed_cli", start_day = date(2020,8,3), end_day = date(2020,8,4), geo_type = "state")
    covidcast.plot_choropleth(data)
    plt.show()

Additional keyword arguments can also be provided. These correspond to most of the arguments
available for the
`GeoPandas plot() function <https://geopandas.org/reference.html#geopandas.GeoSeries.plot>`_.


>>> plotting.plot_choropleth(data,
...                          cmap="viridis",
...                          edgecolor="0.8")
>>> plt.show()

.. plot::

    import covidcast
    from datetime import date
    from matplotlib import pyplot as plt
    data = covidcast.signal("fb-survey", "smoothed_cli", start_day=date(2020,8,3), end_day=date(2020,8,4), geo_type="county")
    covidcast.plot_choropleth(data, cmap="viridis", edgecolor="0.8")
    plt.show()

The function returns a
`Matplotlib Figure object <https://matplotlib.org/api/_as_gen/matplotlib.figure.Figure.html#matplotlib.figure.Figure>`_
which can be stored and altered further.

>>> fig = plotting.plot_choropleth(data)
>>> fig.set_dpi(100)

Further Customization
---------------------
If more control is desired, the signal data can be passed to :py:func:`covidcast.get_geo_df`, which
will return a
`GeoPandas GeoDataFrame <https://geopandas.org/reference/geopandas.GeoDataFrame.html>`_ with
relevant polgons that can be used with the `mapping tools <https://geopandas.org/mapping.html>`_
provided by that package. The geometry information is sourced from the
`2019 US Census Cartographic Boundary Files <https://www.census.gov/geographies/mapping-files/time-series/geo/cartographic-boundary.html>`_.

The :py:func:`covidcast.get_geo_df` method can return different joins depending on your use case. By
default, it will try to compute the right join between the input data (left side of join) to the
geometry data (right side of join), so that the returned GeoDataFrame will contain all the possible
geometries with the signal values filled if present. Counties which do not have values but have
a corresponding megacounty will inherit the megacounty values. This operation depends on
having only one row of signal information per geographic region. If this is not the the case, you
must use another join.

>>> data = covidcast.signal("fb-survey", "smoothed_cli",
...                          date(2020, 8, 3), date(2020, 8, 4),
...                          geo_type = "county")
>>> covidcast.get_geo_df(data)
     geo_value time_value direction      issue  lag     value    stderr  sample_size geo_type data_source        signal                                           geometry state_fips
0        24510 2020-08-04      None 2020-08-06  2.0  0.375601  0.193356     587.6289   county   fb-survey  smoothed_cli  POLYGON ((-76.71131 39.37193, -76.62619 39.372...         24
1        31169        NaT      None        NaT  NaN  0.928208       NaN          NaN      NaN         NaN           NaN  POLYGON ((-97.82082 40.35054, -97.36869 40.350...         31
2        37077        NaT      None        NaT  NaN  0.627742       NaN          NaN      NaN         NaN           NaN  POLYGON ((-78.80252 36.21349, -78.80235 36.220...         37
3        46091        NaT      None        NaT  NaN  0.589745       NaN          NaN      NaN         NaN           NaN  POLYGON ((-97.97924 45.76257, -97.97878 45.935...         46
4        39075        NaT      None        NaT  NaN  0.785641       NaN          NaN      NaN         NaN           NaN  POLYGON ((-82.22066 40.66758, -82.12620 40.668...         39
...        ...        ...       ...        ...  ...       ...       ...          ...      ...         ...           ...                                                ...        ...
3228     53055        NaT      None        NaT  NaN  0.440817       NaN          NaN      NaN         NaN           NaN  MULTIPOLYGON (((-122.97714 48.79345, -122.9379...         53
3229     39133 2020-08-04      None 2020-08-06  2.0  0.040082  0.089324     310.8495   county   fb-survey  smoothed_cli  POLYGON ((-81.39328 41.02544, -81.39322 41.040...         39
3230     08025        NaT      None        NaT  NaN  0.440306       NaN          NaN      NaN         NaN           NaN  POLYGON ((-104.05840 38.26084, -104.05392 38.5...         08
3231     13227        NaT      None        NaT  NaN  1.009511       NaN          NaN      NaN         NaN           NaN  POLYGON ((-84.65437 34.54895, -84.52139 34.550...         13
3232     21145 2020-08-04      None 2020-08-06  2.0  1.257862  0.915558     150.4266   county   fb-survey  smoothed_cli  POLYGON ((-88.93308 37.22775, -88.93174 37.227...         21
[3233 rows x 13 columns]

Note that there are 3233 output rows for the 3233 counties present in the Census shapefiles.

>>> covidcast.get_geo_df(covid, join_type="left")
    geo_value time_value direction      issue  lag     value    stderr  sample_size geo_type data_source        signal                                           geometry state_fips
0       01000 2020-08-04      None 2020-08-06    2  1.153447  0.136070    1759.8539   county   fb-survey  smoothed_cli                                               None        NaN
1       01001 2020-08-04      None 2020-08-06    2  0.539568  0.450588     107.9345   county   fb-survey  smoothed_cli  POLYGON ((-86.91759 32.66417, -86.81657 32.660...         01
2       01003 2020-08-04      None 2020-08-06    2  1.625496  0.522036     455.2964   county   fb-survey  smoothed_cli  POLYGON ((-88.02927 30.22271, -88.02399 30.230...         01
3       01015 2020-08-04      None 2020-08-06    2  0.000000  0.378788     115.2302   county   fb-survey  smoothed_cli  POLYGON ((-86.14371 33.70913, -86.12388 33.710...         01
4       01051 2020-08-04      None 2020-08-06    2  0.786565  0.435877     112.5569   county   fb-survey  smoothed_cli  POLYGON ((-86.41333 32.75059, -86.37497 32.753...         01
..        ...        ...       ...        ...  ...       ...       ...          ...      ...         ...           ...                                                ...        ...
840     55141 2020-08-04      None 2020-08-06    2  1.190476  0.867751     144.3682   county   fb-survey  smoothed_cli  POLYGON ((-90.31605 44.42450, -90.31596 44.424...         55
841     56000 2020-08-04      None 2020-08-06    2  0.822092  0.254670     628.9937   county   fb-survey  smoothed_cli                                               None        NaN
842     56021 2020-08-04      None 2020-08-06    2  0.269360  0.315094     197.9646   county   fb-survey  smoothed_cli  POLYGON ((-105.28064 41.33100, -105.27824 41.6...         56
843     56025 2020-08-04      None 2020-08-06    2  0.170940  0.304654     192.0237   county   fb-survey  smoothed_cli  POLYGON ((-107.54353 42.78156, -107.50142 42.7...         56
844     72000 2020-08-04      None 2020-08-06    2  0.000000  0.228310     100.9990   county   fb-survey  smoothed_cli                                               None        NaN
[845 rows x 13 columns]

With the left join, there are 845 rows since the signal returned information for 845 counties and
megacounties.

With the GeoDataFrame, you can plot various data points in whatever style you prefer. For example,
plotting California with a Mercator projection:

>>> CA = geo_data.loc[geo_data.state_fips == "06",:]
>>> CA.to_crs("EPSG:3395")
>>> CA.plot(column="true_value", figsize=(5,5), legend=True)
>>> plt.axis("off")
>>> plt.show()

.. plot::

    import covidcast
    from datetime import date
    from matplotlib import pyplot as plt
    data = covidcast.signal("fb-survey", "smoothed_cli", start_day=date(2020, 8, 3), end_day=date(2020, 8, 4), geo_type="county")
    geo_data = covidcast.get_geo_df(data)
    CA = geo_data.loc[geo_data.state_fips == "06",:]
    CA = CA.to_crs("EPSG:3395")
    CA.plot(column="value", figsize=(5,5), legend=True)
    plt.axis("off")
    plt.show()
