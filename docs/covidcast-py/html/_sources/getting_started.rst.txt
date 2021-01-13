.. _getting-started:

Getting Started
===============


Overview
------------

This package provides access to data from the `COVIDcast API
<https://cmu-delphi.github.io/delphi-epidata/api/covidcast.html>`_, which
provides numerous COVID-related data streams, updated daily. The data is retrieved
live from the server when you make a request, and is not stored within the package
itself. This means that each time you make a request, you will be receiving the latest
data available. If you are conducting an analysis or powering a service which will require
repeated access to the same signal, please download the data rather than making repeated
requests.


Installation
------------

This package is available on PyPI as `covidcast
<https://pypi.org/project/covidcast/>`_, and can be installed using ``pip`` or
your favorite Python package manager:

.. code-block:: sh

   pip install covidcast

This will install the package as well as all required dependencies.

Signal Overview
---------------
The `API documentation
<https://cmu-delphi.github.io/delphi-epidata/api/covidcast_signals.html>`_ lists
the available signals, including many not shown on the
`COVIDcast interactive map
<https://covidcast.cmu.edu/>`_.

The data come from a variety of sources and cover information including official case counts,
internet search trends, hospital encounters, survey responses, and more. Below is
a brief overview of each source, with links to their full descriptions.

- `Change Healthcare <https://cmu-delphi.github.io/delphi-epidata/api/covidcast-signals/chng.html>`_
    - Outpatient visits with COVID diagnostic codes, based on de-identified
      medical claims data.
- `Doctor Visits <https://cmu-delphi.github.io/delphi-epidata/api/covidcast-signals/doctor-visits.html>`_
    - Outpatient visits with COVID-related symptoms.
- `Google Health Trends <https://cmu-delphi.github.io/delphi-epidata/api/covidcast-signals/ght.html>`_
    - COVID-related Google search volume.
- `Hospital Admissions <https://cmu-delphi.github.io/delphi-epidata/api/covidcast-signals/hospital-admissions.html>`_
    - Hospital admissions with COVID-associated diagnoses.
- `Indicator Combination <https://cmu-delphi.github.io/delphi-epidata/api/covidcast-signals/indicator-combination.html>`_
    - Aggregated signal of other sources to provide a single COVID activity indicator.
- `JHU Cases and Deaths <https://cmu-delphi.github.io/delphi-epidata/api/covidcast-signals/jhu-csse.html>`_
    - Confirmed COVID cases and deaths based on reports made available by Johns Hopkins University.
- `Quidel <https://cmu-delphi.github.io/delphi-epidata/api/covidcast-signals/quidel.html>`_
    - Positive COVID antigen tests.
- `SafeGraph Mobility <https://cmu-delphi.github.io/delphi-epidata/api/covidcast-signals/safegraph.html>`_
    - Mobility (movement) data based on phone location data
- `Symptom Surveys <https://cmu-delphi.github.io/delphi-epidata/api/covidcast-signals/fb-survey.html>`_
    - Various responses to the CMU symptom survey.
- `USAFacts Cases and Deaths <https://cmu-delphi.github.io/delphi-epidata/api/covidcast-signals/usa-facts.html>`_
    - Confirmed COVID cases and deaths based on reports made available by USAFacts.

To specify a signal, you will need the "Source Name" and "Signal" value, which are
listed for each source/signal combination on their respective page.
For example, to obtain the raw Google search volume for COVID-related topics,
the source would be ``ght`` and the signal would be ``raw_search``, as shown on the
`Google Health Trends page
<https://cmu-delphi.github.io/delphi-epidata/api/covidcast-signals/ght.html>`_.
These values will be provided as the arguments for the :py:func:`covidcast.signal` function to
retrieve the desired data.


Basic examples
--------------

To obtain smoothed estimates of COVID-like illness from our symptom survey,
distributed through Facebook, for every county in the United States between
2020-05-01 and 2020-05-07:

>>> from datetime import date
>>> import covidcast
>>> data = covidcast.signal("fb-survey", "smoothed_cli",
...                         date(2020, 5, 1), date(2020, 5, 7),
...                         "county")
>>> data.head()
   geo_value      issue  lag  sample_size    stderr time_value     value
0      01000 2020-05-23   22    1722.4551  0.125573 2020-05-01  0.823080
1      01001 2020-05-23   22     115.8025  0.800444 2020-05-01  1.261261
2      01003 2020-05-23   22     584.3194  0.308680 2020-05-01  0.665129
3      01015 2020-05-23   22     122.5577  0.526590 2020-05-01  0.574713
4      01031 2020-05-23   22     114.8318  0.347450 2020-05-01  0.408163

Each row represents one observation in one county on one day. The county FIPS
code is given in the ``geo_value`` column, the date in the ``time_value``
column. Here ``value`` is the requested signal---in this case, the smoothed
estimate of the percentage of people with COVID-like illness, based on the
symptom surveys. ``stderr`` is its standard error. The ``issue`` column
indicates when this data was reported; in this case, the survey estimates for
May 1st were updated on May 23rd based on new data, giving a ``lag`` of 22 days.
See the :py:func:`covidcast.signal` documentation for details on the returned
data frame.

The API documentation lists each available signal and provides technical details
on how it is estimated and how its standard error is calculated. In this case,
for example, the `symptom surveys documentation page
<https://cmu-delphi.github.io/delphi-epidata/api/covidcast-signals/fb-survey.html>`_
explains the definition of "COVID-like illness", links to the exact survey text,
and describes the mathematical derivation of the estimates.

We can also request all data on a signal after a specific date. Here, for
example, we obtain ``smoothed_cli`` in each state for every day since
2020-05-01:

>>> data = covidcast.signal("fb-survey", "smoothed_cli",
...                         date(2020, 5, 1), geo_type="state")
>>> data.head()
   geo_value      issue  lag  sample_size    stderr time_value     value
0         ak 2020-05-23   22    1606.0000  0.158880 2020-05-01  0.460772
1         al 2020-05-23   22    7540.2437  0.082553 2020-05-01  0.699511
2         ar 2020-05-23   22    4921.4827  0.103651 2020-05-01  0.759798
3         az 2020-05-23   22   11220.9587  0.061794 2020-05-01  0.566937
4         ca 2020-05-23   22   51870.1382  0.022803 2020-05-01  0.364908

Using the ``geo_values`` argument, we can request data for a specific geography,
such as the state of Pennsylvania for the month of May 2020:

>>> pa_data = covidcast.signal("fb-survey", "smoothed_cli",
...                            date(2020, 5, 1), date(2020, 5, 31),
...                            geo_type="state", geo_values="pa")
>>> pa_data.head()
   geo_value      issue  lag  sample_size    stderr time_value     value
0         pa 2020-05-23   22   31576.0165  0.030764 2020-05-01  0.400011
0         pa 2020-05-23   21   31344.0168  0.030708 2020-05-02  0.394774
0         pa 2020-05-23   20   30620.0162  0.031173 2020-05-03  0.396340
0         pa 2020-05-23   19   30419.0163  0.029836 2020-05-04  0.357501
0         pa 2020-05-23   18   29245.0172  0.030176 2020-05-05  0.354521

We can request multiple states by providing a list, such as ``["pa", "ny",
"mo"]``.

Sometimes it may be useful to join multiple signals into a single data frame.
For example, suppose I'd like to look at the relationships between cases at each
location and the number of deaths three days later. The
:py:func:`covidcast.aggregate_signals` function can combine multiple data frames
into a single one, optionally with lag. In this case, I use it as follows:

>>> cases = covidcast.signal("indicator-combination", "confirmed_incidence_num",
...                          date(2020, 5, 1), date(2020, 5, 31),
...                          geo_type="state", geo_values="pa")
>>> deaths = covidcast.signal("indicator-combination", "deaths_incidence_num",
...                           date(2020, 5, 1), date(2020, 5, 31),
...                           geo_type="state", geo_values="pa")
>>> cases_v_deaths = covidcast.aggregate_signals([cases, deaths], dt=[3, 0])
>>> cases_v_deaths = cases_v_deaths.rename(
...     columns={"indicator-combination_confirmed_incidence_num_0_value": "cases",
...              "indicator-combination_deaths_incidence_num_1_value": "deaths"})
>>> cases_v_deaths[["time_value", "geo_value", "cases", "deaths"]].head()
  time_value geo_value   cases  deaths
0 2020-05-01        pa     NaN    62.0
1 2020-05-02        pa     NaN    65.0
2 2020-05-03        pa     NaN    24.0
3 2020-05-04        pa  1209.0    13.0
4 2020-05-05        pa  1332.0   547.0

The resulting ``cases_v_deaths`` data frame contains one row per location per
day. The death value is the number of deaths on that day; the cases value is the
number of cases *3 days prior*, matching the ``dt`` provided to
:py:func:`covidcast.aggregate_signals`. The first three case values shown above
are ``NaN`` because the input data frame did not contain case numbers for late
April.

Note the long column names used by default to prevent ambiguity or name
collisions.

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
   geo_value      issue  lag sample_size stderr time_value    value
0         pa 2020-05-07    6        None   None 2020-05-01  2.32192

This shows that an estimate of about 2.3% was issued on May 7. If we don't
specify ``as_of``, we get the most recent estimate available:

>>> covidcast.signal("doctor-visits", "smoothed_cli",
...                  start_day=date(2020, 5, 1), end_day=date(2020, 5, 1),
...                  geo_type="state", geo_values="pa")
   geo_value      issue  lag sample_size stderr time_value     value
0         pa 2020-07-04   64        None   None 2020-05-01  5.075015

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
   geo_value      issue  lag sample_size stderr time_value     value
0         pa 2020-05-05    4        None   None 2020-05-01  1.693061
1         pa 2020-05-06    5        None   None 2020-05-01  2.524167
2         pa 2020-05-07    6        None   None 2020-05-01  2.321920
3         pa 2020-05-08    7        None   None 2020-05-01  2.897032
4         pa 2020-05-09    8        None   None 2020-05-01  2.956456
5         pa 2020-05-12   11        None   None 2020-05-01  3.190634
6         pa 2020-05-13   12        None   None 2020-05-01  3.220023
7         pa 2020-05-14   13        None   None 2020-05-01  3.231314
8         pa 2020-05-15   14        None   None 2020-05-01  3.239970

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
   geo_value      issue  lag sample_size stderr time_value     value
0         pa 2020-05-08    7        None   None 2020-05-01  2.897032
0         pa 2020-05-09    7        None   None 2020-05-02  2.802238
0         pa 2020-05-12    7        None   None 2020-05-05  3.483125
0         pa 2020-05-13    7        None   None 2020-05-06  2.968670
0         pa 2020-05-14    7        None   None 2020-05-07  2.400255

Note that though this query requested all values between 2020-05-01 and
2020-05-07, May 3rd and May 4th were *not* included in the results set. This is
because the query will only include a result for May 3rd if a value were issued
on May 10th (a 7-day lag), but in fact the value was not updated on that day:

>>> covidcast.signal("doctor-visits", "smoothed_cli",
...                  start_day=date(2020, 5, 3), end_day=date(2020, 5, 3),
...                  geo_type="state", geo_values="pa",
...                  issues=(date(2020, 5, 9), date(2020, 5, 15)))
   geo_value      issue  lag sample_size stderr time_value     value
0         pa 2020-05-09    6        None   None 2020-05-03  2.749537
1         pa 2020-05-12    9        None   None 2020-05-03  2.989626
2         pa 2020-05-13   10        None   None 2020-05-03  3.006860
3         pa 2020-05-14   11        None   None 2020-05-03  2.970561
4         pa 2020-05-15   12        None   None 2020-05-03  3.038054

Dealing with geographies
------------------------

As seen above, the COVIDcast API identifies counties by their FIPS code and
states by two-letter abbreviations. Metropolitan statistical areas are also
identified by unique codes, called CBSA IDs. (Exact details and exceptions are
given in the `geographic coding documentation
<https://cmu-delphi.github.io/delphi-epidata/api/covidcast_geography.html>`_.) If
you want to find a specific area by name, this package provides convenience
functions:

>>> covidcast.name_to_cbsa(["Houston", "San Antonio"])
['26420', '41700']

We can use these functions to quickly query data for specific regions:

>>> counties = covidcast.name_to_fips(["Allegheny", "Los Angeles", "Miami-Dade"])
>>> df = covidcast.signal("doctor-visits", "smoothed_cli",
...                       start_day=date(2020, 5, 1), end_day=date(2020, 5, 1),
...                       geo_values=counties)
>>> df
  geo_value        signal time_value      issue  lag     value stderr sample_size geo_type    data_source
0     42003  smoothed_cli 2020-05-01 2020-07-04   64  1.336086   None        None   county  doctor-visits
0     06037  smoothed_cli 2020-05-01 2020-07-04   64  5.787655   None        None   county  doctor-visits
0     12086  smoothed_cli 2020-05-01 2020-07-04   64  6.405477   None        None   county  doctor-visits


We can also quickly convert back from the IDs returned by the API to
human-readable names:

>>> covidcast.fips_to_name(df.geo_value)
['Allegheny County', 'Los Angeles County', 'Miami-Dade County']

Because the functions support regular expression matching, we can quickly find
all regions meeting certain criteria. For example, the five-digit FIPS codes
used to identify counties use their first two digits to identify the state. We
can find all counties in the state of Pennsylvania by querying for FIPS codes
beginning with 42 and requesting all matches:

>>> pa_counties = covidcast.fips_to_name("^42.*", ties_method="all")
>>> pa_counties[0]
{'42000': ['Pennsylvania'], '42001': ['Adams County'], '42003': ['Allegheny County'], '42005': ['Armstrong County'], '42007': ['Beaver County'], '42009': ['Bedford County'], '42011': ['Berks County'], '42013': ['Blair County'], '42015': ['Bradford County'], '42017': ['Bucks County'], '42019': ['Butler County'], '42021': ['Cambria County'], '42023': ['Cameron County'], '42025': ['Carbon County'], '42027': ['Centre County'], '42029': ['Chester County'], '42031': ['Clarion County'], '42033': ['Clearfield County'], '42035': ['Clinton County'], '42037': ['Columbia County'], '42039': ['Crawford County'], '42041': ['Cumberland County'], '42043': ['Dauphin County'], '42045': ['Delaware County'], '42047': ['Elk County'], '42049': ['Erie County'], '42051': ['Fayette County'], '42053': ['Forest County'], '42055': ['Franklin County'], '42057': ['Fulton County'], '42059': ['Greene County'], '42061': ['Huntingdon County'], '42063': ['Indiana County'], '42065': ['Jefferson County'], '42067': ['Juniata County'], '42069': ['Lackawanna County'], '42071': ['Lancaster County'], '42073': ['Lawrence County'], '42075': ['Lebanon County'], '42077': ['Lehigh County'], '42079': ['Luzerne County'], '42081': ['Lycoming County'], '42083': ['McKean County'], '42085': ['Mercer County'], '42087': ['Mifflin County'], '42089': ['Monroe County'], '42091': ['Montgomery County'], '42093': ['Montour County'], '42095': ['Northampton County'], '42097': ['Northumberland County'], '42099': ['Perry County'], '42101': ['Philadelphia County'], '42103': ['Pike County'], '42105': ['Potter County'], '42107': ['Schuylkill County'], '42109': ['Snyder County'], '42111': ['Somerset County'], '42113': ['Sullivan County'], '42115': ['Susquehanna County'], '42117': ['Tioga County'], '42119': ['Union County'], '42121': ['Venango County'], '42123': ['Warren County'], '42125': ['Washington County'], '42127': ['Wayne County'], '42129': ['Westmoreland County'], '42131': ['Wyoming County'], '42133': ['York County']}

See :ref:`working-with-geos` for details on each of these functions and their
optional arguments.
