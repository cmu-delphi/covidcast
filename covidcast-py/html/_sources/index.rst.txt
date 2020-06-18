.. COVIDcast API client documentation master file, created by
   sphinx-quickstart on Tue Jun 16 18:10:00 2020.
   You can adapt this file completely to your liking, but it should at least
   contain the root `toctree` directive.

COVIDcast API client
====================

This package provides Python access to the `COVIDcast API
<https://cmu-delphi.github.io/delphi-epidata/api/covidcast.html>`_ published by
the `Delphi group <https://delphi.cmu.edu>`_ at `Carnegie Mellon University
<https://www.cmu.edu>`_. This API provides daily access to a range of
COVID-related signals Delphi collects from a variety of sources, including
cases, deaths, symptom surveys, healthcare data, and other information.

Consult the `COVIDcast API documentation
<https://cmu-delphi.github.io/delphi-epidata/api/covidcast.html>`_ for details
on the data included in the API, licensing, and how to cite this data in your
work. The `signals documentation
<https://cmu-delphi.github.io/delphi-epidata/api/covidcast_signals.html>`_ lists
all the data sources and signals available through this API.


.. note :: **You should consider subscribing** to the `API mailing list
   <https://lists.andrew.cmu.edu/mailman/listinfo/delphi-covidcast-api>`_ to be
   notified of package updates, new data sources, corrections, and other
   updates.

This package provides one key function to obtain any signal of interest as a
Pandas data frame:

.. autofunction:: covidcast.signal

Examples
--------

To obtain smoothed estimates of COVID-like illness from our Facebook survey for
every county in the United Statees between 2020-05-01 and 2020-05-07:

>>> from datetime import date
>>> import covidcast
>>> data = covidcast.signal("fb-survey", "smoothed_cli",
...                         date(2020, 5, 1), date(2020, 5, 7),
...                         "county")
>>> data.head()
   direction geo_value  sample_size    stderr time_value     value
0        0.0     01000    1722.4551  0.125573 2020-05-01  0.823080
1        1.0     01001     115.8025  0.800444 2020-05-01  1.261261
2        0.0     01003     584.3194  0.308680 2020-05-01  0.665129
3        0.0     01015     122.5577  0.526590 2020-05-01  0.574713
4        NaN     01031     114.8318  0.347450 2020-05-01  0.408163

We can also request all data on a signal after a specific date. Here, for
example, we obtain ``smoothed_cli`` in each state for every day since
2020-05-01:

>>> data = covidcast.signal("fb-survey", "smoothed_cli", 
...                         date(2020, 5, 1), geo_type="state")
>>> data.head()
   direction geo_value  sample_size    stderr time_value     value
0       -1.0        ak    1606.0000  0.158880 2020-05-01  0.460772
1        0.0        al    7540.2437  0.082553 2020-05-01  0.699511
2       -1.0        ar    4921.4827  0.103651 2020-05-01  0.759798
3        0.0        az   11220.9587  0.061794 2020-05-01  0.566937
4        0.0        ca   51870.1382  0.022803 2020-05-01  0.364908

