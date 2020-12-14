.. COVIDcast API client documentation master file, created by
   sphinx-quickstart on Tue Jun 16 18:10:00 2020.
   You can adapt this file completely to your liking, but it should at least
   contain the root `toctree` directive.

COVIDcast API client
====================

This package provides Python access to the `COVIDcast Epidata API
<https://cmu-delphi.github.io/delphi-epidata/api/covidcast.html>`_ published by
the `Delphi group <https://delphi.cmu.edu>`_ at `Carnegie Mellon University
<https://www.cmu.edu>`_. This API provides daily access to a range of
COVID-related signals Delphi collects from a variety of sources, including
cases, deaths, symptom surveys, healthcare data, and other information.

Consult the `COVIDcast Epidata API documentation
<https://cmu-delphi.github.io/delphi-epidata/api/covidcast.html>`_ for details
on the data included in the API, licensing, and how to cite this data in your
work. The `signals documentation
<https://cmu-delphi.github.io/delphi-epidata/api/covidcast_signals.html>`_ lists
all the data sources and signals available through this API.

The package source code and bug tracker can be found `on GitHub
<https://github.com/cmu-delphi/covidcast>`_.

To get started, check out :ref:`getting started <getting-started>`.


.. note :: **You should consider subscribing** to the `API mailing list
   <https://lists.andrew.cmu.edu/mailman/listinfo/delphi-covidcast-api>`_ to be
   notified of package updates, new data sources, corrections, and other
   updates.

.. warning :: If you use data from the COVIDcast Epidata API to power a public
   product, dashboard, app, or other service, please download the data you need
   and store it centrally rather than making API requests for every user. Our
   server resources are limited and cannot support high-volume interactive use.

   See also the `COVIDcast Terms of Use
   <https://covidcast.cmu.edu/terms-of-use.html>`_, noting that the data is a
   research product and not warranted for a particular purpose.


Contents
--------

.. toctree::
   :maxdepth: 2

   getting_started
   signals
   plot_examples
   plotting
   changelog
