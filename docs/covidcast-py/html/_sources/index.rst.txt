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

The package source code and bug tracker can be found `on GitHub
<https://github.com/cmu-delphi/covidcast>`_.


.. note :: **You should consider subscribing** to the `API mailing list
   <https://lists.andrew.cmu.edu/mailman/listinfo/delphi-covidcast-api>`_ to be
   notified of package updates, new data sources, corrections, and other
   updates.

.. warning :: If you use data from the COVIDcast API to power a public product,
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

   pip install covidcast

The package requires `pandas <https://pandas.pydata.org/>`_, `requests
<https://requests.readthedocs.io/en/master/>`_, and several other packages;
these should be installed automatically. It also uses the `delphi-epidata
<https://pypi.org/project/delphi-epidata/>`_ package to access Delphi's Epidata
API.

Contents
--------

.. toctree::
   :maxdepth: 2

   getting_started
   signals
   plot_examples
   plotting
   changelog
