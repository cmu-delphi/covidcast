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

.. warning :: If you use data from the COVIDcast API to power a product,
   dashboard, app, or other service, please download the data you need and store
   it centrally rather than making API requests for every user. Our server
   resources are limited and cannot support high-volume interactive use.

   See also the `COVIDcast Terms of Use
   <https://covidcast.cmu.edu/terms-of-use.html>`_, noting that the data is a
   research product and not warranted for a particular purpose.


Installation
------------

This package is available on PyPI as `covidcast
<https://pypi.org/project/covidcast/>`_, and can be installed using ``pip`` or
your favorite Python package manager:

.. code-block:: sh

   pip3 install covidcast

The package requires `pandas <https://pandas.pydata.org/>`_ and `requests
<https://requests.readthedocs.io/en/master/>`_; these should be installed
automatically. It also uses the `delphi-epidata
<https://pypi.org/project/delphi-epidata/>`_ package to access Delphi's Epidata
API.

Usage
-----

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

Using the ``geo_value`` argument, we can request data for a specific geography,
such as the state of Pennsylvania for the month of May 2020:

>>> pa_data = covidcast.signal("fb-survey", "smoothed_cli",
...                            date(2020, 5, 1), date(2020, 5, 31),
...                            geo_type="state", geo_value="pa")
>>> pa_data.head()
   direction geo_value  sample_size    stderr time_value     value
0         -1        pa   31576.0165  0.030764 2020-05-01  0.400011
0         -1        pa   31344.0168  0.030708 2020-05-02  0.394774
0          0        pa   30620.0162  0.031173 2020-05-03  0.396340
0         -1        pa   30419.0163  0.029836 2020-05-04  0.357501
0          0        pa   29245.0172  0.030176 2020-05-05  0.354521
