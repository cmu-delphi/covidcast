Example usage
=============

To obtain smoothed estimates of COVID-like illness from our Facebook survey for
every county in the United States between 2020-05-01 and 2020-05-07:

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

Using the ``geo_values`` argument, we can request data for a specific geography,
such as the state of Pennsylvania for the month of May 2020:

>>> pa_data = covidcast.signal("fb-survey", "smoothed_cli",
...                            date(2020, 5, 1), date(2020, 5, 31),
...                            geo_type="state", geo_values="pa")
>>> pa_data.head()
   direction geo_value  sample_size    stderr time_value     value
0         -1        pa   31576.0165  0.030764 2020-05-01  0.400011
0         -1        pa   31344.0168  0.030708 2020-05-02  0.394774
0          0        pa   30620.0162  0.031173 2020-05-03  0.396340
0         -1        pa   30419.0163  0.029836 2020-05-04  0.357501
0          0        pa   29245.0172  0.030176 2020-05-05  0.354521

We can request multiple states by providing a list, such as ``["pa", "ny",
"mo"]``.
