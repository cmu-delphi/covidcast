.. _geographic-lookup:

Geographic Lookup Examples
==========================

Converting names to FIPS or CBSA codes
--------------------------------------
To map a county or MSA name to their respective FIPS or CBSA codes, simply provide regular
expression pattern to the functions, or provide the exact string and use the ``fixed=True``
argument to do a direct string comparison

>>> covidcast.name_to_fips("Allegheny")
['42003']


Casing can be ignored by specifying ``ignore_case=True``
>>> covidcast.name_to_fips("Allegheny")
['42003']
>>> covidcast.name_to_fips("ALLegheny")
[None]
>>> covidcast.name_to_fips("ALLegheny", ignore_case=True)
['42003']

If an input yields multiple results, it will return the first match and warn.

>>> covidcast.name_to_fips("All")
UserWarning: Some inputs were not uniquely matched; returning only the first match in each case. To return all matches, set `ties_method='all'`
  warnings.warn("Some inputs were not uniquely matched; returning only the first match "
['18003']

To obtain all the matches, use the `ties_method` argument.

>>> covidcast.name_to_fips("Alle", ties_method="all")
[{'Allen County': ['18003', '20001', '21003', '39003'], 'Allen Parish': ['22003'], 'Allegany County': ['24001', '36003'], 'Allegan County': ['26005'], 'Alleghany County': ['37005', '51005'], 'Allegheny County': ['42003'], 'Allendale County': ['45005']}]

Some inputs may not only correspond to multiple regions due to the regular expressions, but a
region may also contain multiple codes.

>>> covidcast.name_to_fips(["Allegheny", "Miami", "New "], ties_method = "all")
[{'Allegheny County': ['42003']}, {'Miami-Dade County': ['12086'], 'Miami County': ['18103', '20121', '39109']}, {'New Haven County': ['09009'], 'New London County': ['09011'], 'New Castle County': ['10003'], 'New Madrid County': ['29143'], 'New Hampshire': ['33000'], 'New Jersey': ['34000'], 'New Mexico': ['35000'], 'New York': ['36000'], 'New York County': ['36061'], 'New Hanover County': ['37129'], 'New Kent County': ['51127']}]


If no result is found for a given input, `None` or an empty dictionary will be returned for
``ties_method="first"`` and ``ties_method="all"``. This is done so that the returned list will
always be the same length as the number of inputs. However, note that this means a completely
empty return will have a `True` boolean value. An empty return can be detected by using a
list comprehension.

>>> covidcast.name_to_fips(["Allegheny", "Not a FIPS"])
['42003', None]
>>> output = covidcast.name_to_fips("Not a FIPS")
>>> output
[None]
>>> bool(output)
True
>>> any(i for i in output)
False

When converting names, a state can also be specified to restrict the search.

>>> covidcast.name_to_fips("Allegheny", state="PA")
['42003']
>>> covidcast.name_to_fips("Allegheny", state="OK")
[None]


Converting FIPS or CBSA codes to names
--------------------------------------
These functions have the same interface, with the exception that they cannot be filtered by a state.

>>> covidcast.fips_to_name("42003")
['Allegheny County']
>>> covidcast.cbsa_to_name(["38300", "39300"])
['Pittsburgh, PA', 'Providence-Warwick, RI-MA']


State Examples
--------------
The state abbreviation functions also maintain the same interface.

>>> covidcast.abbr_to_name("CA")
['California']
>>> covidcast.name_to_abbr("California")
['CA']
>>> covidcast.abbr_to_name(["CA", "PA", "XX"])
['California', 'Pennsylvania', None]

