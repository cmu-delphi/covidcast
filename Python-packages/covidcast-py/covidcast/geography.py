import re
import warnings
from typing import Union, Iterable
import pandas as pd
import pkg_resources

COUNTY_CENSUS = pd.read_csv(
    pkg_resources.resource_filename(__name__, "geo_mappings/county_census.csv"), dtype=str)
MSA_CENSUS = pd.read_csv(
    pkg_resources.resource_filename(__name__, "geo_mappings/msa_census.csv"), dtype=str)
STATE_CENSUS = pd.read_csv(
    pkg_resources.resource_filename(__name__, "geo_mappings/state_census.csv"), dtype=str)

# Filter undesired rows from CSVs.
# They're not removed from the files to keep them identical to rda files.
STATE_CENSUS = STATE_CENSUS.loc[STATE_CENSUS.STATE != "0"]
MSA_CENSUS = MSA_CENSUS.loc[MSA_CENSUS.LSAD == "Metropolitan Statistical Area"]


def fips_to_name(code: Union[str, Iterable],
                 ignore_case: bool = False,
                 fixed: bool = False,
                 ties_method: str = "first"):
    """Look up county names by FIPS codes with regular expression support.

    Given an individual or list of FIPS codes or regular expressions, look up the corresponding
    county names.

    :param code: Individual or list of FIPS codes or regular expressions.
    :param ignore_case: Boolean for whether or not to be case insensitive in the regular
      expression. If ``fixed=True``, this argument is ignored. Defaults to `False`.
    :param fixed: Conduct an exact case sensitive match with the input string. Defaults to `False`.
    :param ties_method: Method for determining how to deal with multiple outputs for a given input.
      Must be one of `"all"` or `"first"`. If `"first"`, then only the first match for each code is
      returned. If `"all"`, then all matches for each code are returned. Defaults to `first`.
    :return: If ``ties_method="first"``, returns a dictionary of each input code to the first
      corresponding name found. If `ties_method="all"`, returns a list of dicts, one for each input,
      with keys corresponding to all matched input keys and values corresponding to the list of
      county names.
    """
    return _lookup(code, COUNTY_CENSUS.FIPS, COUNTY_CENSUS.CTYNAME, ignore_case, fixed, ties_method)


def cbsa_to_name(code: Union[str, Iterable],
                 ignore_case: bool = False,
                 fixed: bool = False,
                 ties_method: str = "first") -> Union[dict, list]:
    """Look up MSA names by codes with regular expression support.

    Given an individual or list of FIPS codes or regular expressions, look up the corresponding
    MSA names.

    :param code: Individual or list of FIPS codes or regular expressions.
    :param ignore_case: Boolean for whether or not to be case insensitive in the regular
      expression. If ``fixed=True``, this argument is ignored. Defaults to `False`.
    :param fixed: Conduct an exact case sensitive match with the input string. Defaults to `False`.
    :param ties_method: Method for determining how to deal with multiple outputs for a given input.
      Must be one of `"all"` or `"first"`. If `"first"`, then only the first match for each code is
      returned. If `"all"`, then all matches for each code are returned. Defaults to `first`.
    :return: If ``ties_method="first"``, returns a dictionary of each input code to the first
      corresponding name found. If `ties_method="all"`, returns a list of dicts, one for each input,
      with keys corresponding to all matched input keys and values corresponding to the list of
      MSA names.
    """
    return _lookup(code, MSA_CENSUS.CBSA, MSA_CENSUS.NAME, ignore_case, fixed, ties_method)


def abbr_to_name(abbr: Union[str, Iterable],
                 ignore_case: bool = False,
                 fixed: bool = False,
                 ties_method: str = "first") -> Union[dict, list]:
    """Look up state name by abbreviation with regular expression support.

    Given an individual or list of state abbreviations or regular expressions, look up the
    corresponding state names.

    :param abbr: Individual or list of state abbreviations or regular expressions.
    :param ignore_case: Boolean for whether or not to be case insensitive in the regular
      expression. If ``fixed=True``, this argument is ignored. Defaults to `False`.
    :param fixed: Conduct an exact case sensitive match with the input string. Defaults to `False`.
    :param ties_method: Method for determining how to deal with multiple outputs for a given input.
      Must be one of `"all"` or `"first"`. If `"first"`, then only the first match for each code is
      returned. If `"all"`, then all matches for each code are returned. Defaults to `first`.
    :return: If ``ties_method="first"``, returns a dictionary of each input code to the first
      corresponding name found. If `ties_method="all"`, returns a list of dicts, one for each input,
      with keys corresponding to all matched input keys and values corresponding to the list of
      state names.
    """
    return _lookup(abbr, STATE_CENSUS.ABBR, STATE_CENSUS.NAME, ignore_case, fixed, ties_method)


def name_to_abbr(name: Union[str, Iterable],
                 ignore_case: bool = False,
                 fixed: bool = False,
                 ties_method: str = "first") -> Union[dict, list]:
    """Look up state abbreviation by name with regular expression support.

    Given an individual or list of state names or regular expressions, look up the
    corresponding state abbreviations.

    :param name: Individual or list of state names or regular expressions.
    :param ignore_case: Boolean for whether or not to be case insensitive in the regular
      expression. If ``fixed=True``, this argument is ignored. Defaults to `False`.
    :param fixed: Conduct an exact case sensitive match with the input string. Defaults to `False`.
    :param ties_method: Method for determining how to deal with multiple outputs for a given input.
      Must be one of `"all"` or `"first"`. If `"first"`, then only the first match for each code is
      returned. If `"all"`, then all matches for each code are returned. Defaults to `first`.
    :return: If ``ties_method="first"``, returns a dictionary of each input code to the first
      corresponding name found. If `ties_method="all"`, returns a list of dicts, one for each input,
      with keys corresponding to all matched input keys and values corresponding to the list of
      state abbreviations.
    """
    return _lookup(name, STATE_CENSUS.NAME, STATE_CENSUS.ABBR, ignore_case, fixed, ties_method)


def name_to_cbsa(name: Union[str, Iterable],
                 ignore_case: bool = False,
                 fixed: bool = False,
                 ties_method: str = "first",
                 state: str = None) -> Union[dict, list]:
    """Look up MSA codes by names with regular expression support.

    Given an individual or list of names or regular expressions, look up the corresponding
    MSA codes.

    :param name: Individual or list of MSA names or regular expressions.
    :param ignore_case: Boolean for whether or not to be case insensitive in the regular
      expression. If ``fixed=True``, this argument is ignored. Defaults to `False`.
    :param fixed: Conduct an exact case sensitive match with the input string. Defaults to `False`.
    :param ties_method: Method for determining how to deal with multiple outputs for a given input.
      Must be one of `"all"` or `"first"`. If `"first"`, then only the first match for each code is
      returned. If `"all"`, then all matches for each code are returned. Defaults to `first`.
    :param state: 2 letter state code, case insensitive, to restrict results to.
    :return: If ``ties_method="first"``, returns a dictionary of each input name to the first
      corresponding name found. If `ties_method="all"`, returns a list of dicts, one for each input,
      with keys corresponding to all matched input keys and values corresponding to the list of
      MSA codes.
    """
    if state:
        state = state.upper()
        df = MSA_CENSUS.loc[MSA_CENSUS.STATE == state]
    else:
        df = MSA_CENSUS
    return _lookup(name, df.NAME, df.CBSA, ignore_case, fixed, ties_method)


def name_to_fips(name: Union[str, Iterable],
                 ignore_case: bool = False,
                 fixed: bool = False,
                 ties_method: str = "first",
                 state: str = None) -> Union[dict, list]:
    """Look up FIPS codes by county names with regular expression support.

    Given an individual or list of county names or regular expressions, look up the corresponding
    FIPS codes.

    :param name: Individual or list of county names or regular expressions.
    :param ignore_case: Boolean for whether or not to be case insensitive in the regular
      expression. If ``fixed=True``, this argument is ignored. Defaults to `False`.
    :param fixed: Conduct an exact case sensitive match with the input string. Defaults to `False`.
    :param ties_method: Method for determining how to deal with multiple outputs for a given input.
      Must be one of `"all"` or `"first"`. If `"first"`, then only the first match for each code is
      returned. If `"all"`, then all matches for each code are returned. Defaults to `first`.
    :param state: 2 letter state code, case insensitive, to restrict results to.
    :return: If ``ties_method="first"``, returns a dictionary of each input name to the first
      corresponding name found. If `ties_method="all"`, returns a list of dicts, one for each input,
      with keys corresponding to all matched input keys and values corresponding to the list of
      FIPS.
    """
    if state:
        state = state.upper()
        df = COUNTY_CENSUS.loc[COUNTY_CENSUS.STNAME == abbr_to_name(state)[state]]  # type:ignore
    else:
        df = COUNTY_CENSUS
    return _lookup(name, df.CTYNAME, df.FIPS, ignore_case, fixed, ties_method)


def _lookup(key: Union[str, Iterable],
            keys: Iterable,
            values: Iterable,
            ignore_case: bool = False,
            fixed: bool = False,
            ties_method: str = "first") -> Union[dict, list]:
    """Given an input, search for it in a list of keys and return the corresponding values.

    :param key: Individual or list of search strings or regular expression patterns.
    :param keys: List of keys to be searched
    :param values: List of values that correspond to keys.
    :param ignore_case: Boolean for whether or not to be case insensitive in the regular expression.
      If ``fixed=True``, this argument is ignored. Defaults to `False`.
    :param fixed: Conduct an exact case sensitive match with the input string. Defaults to `False`.
    :param ties_method: Method for determining how to deal with multiple outputs for a given input.
      Must be one of `"all"` or `"first"`. If `"first"`, then only the first match for each code is
      returned. If `"all"`, then all matches for each code are returned. Defaults to `first`.
    :return: If ``ties_method="first"``, returns a dictionary of each input code to the first
      corresponding key found. If `ties_method="all"`, returns a list of dicts, one for each input,
      with keys corresponding to all matched input keys and values corresponding to the list of
      values.
    """
    if ties_method not in ("first", "all"):
        raise ValueError("Invalid `ties_method`. Must be one of `first` or `all`.")
    key = [key] if isinstance(key, str) else key
    case = re.IGNORECASE if (ignore_case and not fixed) else 0
    output = []
    for i in key:
        result: dict = {}
        for k, v in zip(keys, values):
            if i == k if fixed else re.search(i, k, case):
                result[k] = result.get(k, []) + [v]
        if result:  # this prevents an empty output of [{}], which is Truthy
            output.append(result)
    if ties_method == "first":
        return _get_first_tie(output)
    return output


def _get_first_tie(dict_list: list) -> dict:
    """Return a dict with the first key and first value for that key for each of the input dicts

    Needs to be Python 3.6+ for this to work, since earlier versions don't preserve insertion order.

    :param dict_list: List of str:list dicts.
    :return: Dictionary of the first key and first value for that key for each of the input dicts.
    """
    for d in dict_list:
        if len(d) > 1 or any(len(val) > 1 for val in d.values()):
            warnings.warn("Some inputs were not uniquely matched; "
                          "returning only the first match in each case.")
    first_items = [list(d.items())[0] for d in dict_list]
    return {i[0]: i[1][0] for i in first_items}
