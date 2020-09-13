Fetching Data
=============

Signals
-------

This package provides a key function to obtain any signal of interest as a
Pandas data frame. Detailed examples are provided in the :ref:`usage examples
<getting-started>`.

.. autofunction:: covidcast.signal

Sometimes you would like to work with multiple signals -- for example, to obtain
several signals at every location, as part of building models of features at
each location. For convenience, the package provides a function to produce a
single data frame containing multiple signals at each location.

.. autofunction:: covidcast.aggregate_signals

                  
Metadata
--------

Many data sources and signals are available, so one can also obtain a data frame
of all signals and their associated metadata:

.. autofunction:: covidcast.metadata
