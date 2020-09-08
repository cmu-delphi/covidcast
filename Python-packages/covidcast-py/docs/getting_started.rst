.. _getting-started:

Getting Started
===============

Basic examples
--------------

To obtain smoothed estimates of COVID-like illness from our Facebook survey for
every county in the United States between 2020-05-01 and 2020-05-07:

>>> from datetime import date
>>> import covidcast
>>> data = covidcast.signal("fb-survey", "smoothed_cli",
...                         date(2020, 5, 1), date(2020, 5, 7),
...                         "county")
>>> data.head()
   direction geo_value      issue  lag  sample_size    stderr time_value     value
0        0.0     01000 2020-05-23   22    1722.4551  0.125573 2020-05-01  0.823080
1        1.0     01001 2020-05-23   22     115.8025  0.800444 2020-05-01  1.261261
2        0.0     01003 2020-05-23   22     584.3194  0.308680 2020-05-01  0.665129
3        0.0     01015 2020-05-23   22     122.5577  0.526590 2020-05-01  0.574713
4        NaN     01031 2020-05-23   22     114.8318  0.347450 2020-05-01  0.408163

Each row represents one observation in one county on one day. The county FIPS
code is given in the ``geo_value`` column, the date in the ``time_value``
column. Here ``value`` is the requested signal---in this case, the smoothed
estimate of the percentage of people with COVID-like illness, based on the
symptom surveys. ``stderr`` is its standard error. The ``issue`` column
indicates when this data was reported; in this case, the survey estimates for
May 1st were updated on May 23rd based on new data, giving a ``lag`` of 22 days.
See the :py:func:`covidcast.signal` documentation for details on the returned
data frame.

We can also request all data on a signal after a specific date. Here, for
example, we obtain ``smoothed_cli`` in each state for every day since
2020-05-01:

>>> data = covidcast.signal("fb-survey", "smoothed_cli",
...                         date(2020, 5, 1), geo_type="state")
>>> data.head()
   direction geo_value      issue  lag  sample_size    stderr time_value     value
0       -1.0        ak 2020-05-23   22    1606.0000  0.158880 2020-05-01  0.460772
1        0.0        al 2020-05-23   22    7540.2437  0.082553 2020-05-01  0.699511
2       -1.0        ar 2020-05-23   22    4921.4827  0.103651 2020-05-01  0.759798
3        0.0        az 2020-05-23   22   11220.9587  0.061794 2020-05-01  0.566937
4        0.0        ca 2020-05-23   22   51870.1382  0.022803 2020-05-01  0.364908

Using the ``geo_values`` argument, we can request data for a specific geography,
such as the state of Pennsylvania for the month of May 2020:

>>> pa_data = covidcast.signal("fb-survey", "smoothed_cli",
...                            date(2020, 5, 1), date(2020, 5, 31),
...                            geo_type="state", geo_values="pa")
>>> pa_data.head()
   direction geo_value      issue  lag  sample_size    stderr time_value     value
0         -1        pa 2020-05-23   22   31576.0165  0.030764 2020-05-01  0.400011
0         -1        pa 2020-05-23   21   31344.0168  0.030708 2020-05-02  0.394774
0          0        pa 2020-05-23   20   30620.0162  0.031173 2020-05-03  0.396340
0         -1        pa 2020-05-23   19   30419.0163  0.029836 2020-05-04  0.357501
0          0        pa 2020-05-23   18   29245.0172  0.030176 2020-05-05  0.354521

We can request multiple states by providing a list, such as ``["pa", "ny",
"mo"]``.

Tracking issues and updates
---------------------------

The COVIDcast API records not just each signal's estimate for a given location
on a given day, but also *when* that estimate was made, and all updates to that
estimate.

For example, consider using our `doctor visits signal
<https://cmu-delphi.github.io/delphi-epidata/api/covidcast-signals/doctor-visits.html>`_,
which estimates the percentage of outpatient doctor visits that are
COVID-related, and consider a result row with ``time_value`` 2020-05-01 for
``geo_values = "pa"``. This is an estimate for the percentage in Pennsylvania on
May 1, 2020. That estimate was *issued* on May 5, 2020, the delay being due to
the aggregation of data by our source and the time taken by the COVIDcast API to
ingest the data provided. Later, the estimate for May 1st could be updated,
perhaps because additional visit data from May 1st arrived at our source and was
reported to us. This constitutes a new *issue* of the data.

By default, :py:func:`covidcast.signal` fetches the most recent issue available.
This is the best option for users who simply want to graph the latest data or
construct dashboards. But if we are interested in knowing *when* data was
reported, we can request specific data versions.

First, we can request the data that was available *as of* a specific date, using
the ``as_of`` argument:

>>> covidcast.signal("doctor-visits", "smoothed_cli",
...                  start_day=date(2020, 5, 1), end_day=date(2020, 5, 1),
...                  geo_type="state", geo_values="pa",
...                  as_of=date(2020, 5, 7))
   direction geo_value      issue  lag sample_size stderr time_value    value
0         -1        pa 2020-05-07    6        None   None 2020-05-01  2.32192

This shows that an estimate of about 2.3% was issued on May 7. If we don't
specify ``as_of``, we get the most recent estimate available:

>>> covidcast.signal("doctor-visits", "smoothed_cli",
...                  start_day=date(2020, 5, 1), end_day=date(2020, 5, 1),
...                  geo_type="state", geo_values="pa")
   direction geo_value      issue  lag sample_size stderr time_value     value
0         -1        pa 2020-07-04   64        None   None 2020-05-01  5.075015

Note the substantial change in the estimate, to over 5%, reflecting new data
that became available *after* May 7 about visits occurring on May 1. This
illustrates the importance of issue date tracking, particularly for forecasting
tasks. To backtest a forecasting model on past data, it is important to use the
data that would have been available *at the time*, not data that arrived much
later.

By using the ``issues`` argument, we can request all issues in a certain time
period:

>>> covidcast.signal("doctor-visits", "smoothed_cli",
...                  start_day=date(2020, 5, 1), end_day=date(2020, 5, 1),
...                  geo_type="state", geo_values="pa",
...                  issues=(date(2020, 5, 1), date(2020, 5, 15)))
   direction geo_value      issue  lag sample_size stderr time_value     value
0         -1        pa 2020-05-05    4        None   None 2020-05-01  1.693061
1         -1        pa 2020-05-06    5        None   None 2020-05-01  2.524167
2         -1        pa 2020-05-07    6        None   None 2020-05-01  2.321920
3          0        pa 2020-05-08    7        None   None 2020-05-01  2.897032
4          0        pa 2020-05-09    8        None   None 2020-05-01  2.956456
5          0        pa 2020-05-12   11        None   None 2020-05-01  3.190634
6          0        pa 2020-05-13   12        None   None 2020-05-01  3.220023
7          0        pa 2020-05-14   13        None   None 2020-05-01  3.231314
8          0        pa 2020-05-15   14        None   None 2020-05-01  3.239970

This estimate was clearly updated many times as new data for May 1st arrived.
Note that these results include only data issued or updated between 2020-05-01
and 2020-05-15. If a value was first reported on 2020-04-15, and never updated,
a query for issues between 2020-05-01 and 2020-05-15 will not include that value
among its results.

Finally, we can use the ``lag`` argument to request only data reported with a
certain lag. For example, requesting a lag of 7 days means to request only
issues 7 days after the corresponding ``time_value``:

>>> covidcast.signal("doctor-visits", "smoothed_cli",
...                  start_day=date(2020, 5, 1), end_day=date(2020, 5, 7),
...                  geo_type="state", geo_values="pa", lag=7)
   direction geo_value      issue  lag sample_size stderr time_value     value
0          0        pa 2020-05-08    7        None   None 2020-05-01  2.897032
0         -1        pa 2020-05-09    7        None   None 2020-05-02  2.802238
0          0        pa 2020-05-12    7        None   None 2020-05-05  3.483125
0          0        pa 2020-05-13    7        None   None 2020-05-06  2.968670
0          0        pa 2020-05-14    7        None   None 2020-05-07  2.400255

Note that though this query requested all values between 2020-05-01 and
2020-05-07, May 3rd and May 4th were *not* included in the results set. This is
because the query will only include a result for May 3rd if a value were issued
on May 10th (a 7-day lag), but in fact the value was not updated on that day:

>>> covidcast.signal("doctor-visits", "smoothed_cli",
...                  start_day=date(2020, 5, 3), end_day=date(2020, 5, 3),
...                  geo_type="state", geo_values="pa",
...                  issues=(date(2020, 5, 9), date(2020, 5, 15)))
   direction geo_value      issue  lag sample_size stderr time_value     value
0         -1        pa 2020-05-09    6        None   None 2020-05-03  2.749537
1         -1        pa 2020-05-12    9        None   None 2020-05-03  2.989626
2         -1        pa 2020-05-13   10        None   None 2020-05-03  3.006860
3         -1        pa 2020-05-14   11        None   None 2020-05-03  2.970561
4         -1        pa 2020-05-15   12        None   None 2020-05-03  3.038054

Dealing with geographies
------------------------

As seen above, the COVIDcast API identifies counties by their FIPS code and
states by two-letter abbreviations. Metropolitan statistical areas are also
identified by unique codes, called CBSA IDs. If you want to find a specific area
by name, this package provides convenience functions:

>>> covidcast.name_to_cbsa(["Houston", "San Antonio"])
{'Houston-The Woodlands-Sugar Land, TX': '26420', 'San Antonio-New Braunfels, TX': '41700'}

We can use these functions to quickly query data for specific regions:

>>> counties = covidcast.name_to_fips(["Allegheny", "Los Angeles", "Miami-Dade"])
>>> d = covidcast.signal("doctor-visits", "smoothed_cli",
...                      start_day=date(2020, 5, 1), end_day=date(2020, 5, 1),
...                      geo_values=counties.values())
>>> d
   direction geo_value      issue  lag sample_size        signal stderr time_value     value geo_type    data_source
0         -1     42003 2020-07-04   64        None  smoothed_cli   None 2020-05-01  1.336086   county  doctor-visits
0          0     06037 2020-07-04   64        None  smoothed_cli   None 2020-05-01  5.787655   county  doctor-visits
0         -1     12086 2020-07-04   64        None  smoothed_cli   None 2020-05-01  6.405477   county  doctor-visits

We can also quickly convert back from the IDs returned by the API to
human-readable names:

>>> covidcast.fips_to_name(d.geo_value)
{'42003': 'Allegheny County', '06037': 'Los Angeles County', '12086': 'Miami-Dade County'}

See :ref:`working-with-geos` for details on each of these functions and their
optional arguments.
