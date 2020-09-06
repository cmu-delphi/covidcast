import re
import warnings

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


def fips_to_name(code, ignore_case=False, fixed=False, ties_method="first"):
    return _lookup(code, COUNTY_CENSUS.FIPS, COUNTY_CENSUS.CTYNAME, ignore_case, fixed, ties_method)


def cbsa_to_name(code, ignore_case=False, fixed=False, ties_method="first"):
    return _lookup(code, MSA_CENSUS.CBSA, MSA_CENSUS.NAME, ignore_case, fixed, ties_method)


def abbr_to_name(name, ignore_case=False, fixed=False, ties_method="first"):
    return _lookup(name, STATE_CENSUS.ABBR, STATE_CENSUS.NAME, ignore_case, fixed, ties_method)


def name_to_abbr(abbr, ignore_case=False, fixed=False, ties_method="first"):
    return _lookup(abbr, STATE_CENSUS.NAME, STATE_CENSUS.ABBR, ignore_case, fixed, ties_method)


def name_to_cbsa(name, ignore_case=False, fixed=False, ties_method="first", state=None):
    if state:
        state = state.upper()
        df = MSA_CENSUS.loc[MSA_CENSUS.STATE == state]
    else:
        df = MSA_CENSUS
    return _lookup(name, df.NAME, df.CBSA, ignore_case, fixed, ties_method)


def name_to_fips(name, ignore_case=False, fixed=False, ties_method="first", state=None):
    if state:
        state = state.upper()
        df = COUNTY_CENSUS.loc[COUNTY_CENSUS.STNAME == abbr_to_name(state)[state]]
    else:
        df = COUNTY_CENSUS
    return _lookup(name, df.CTYNAME, df.FIPS, ignore_case, fixed, ties_method)


def _lookup(key, keys, values, ignore_case=False, fixed=False, ties_method="first"):
    if ties_method not in ("first", "all"):
        raise ValueError("Invalid `ties_method`. Must be one of `first` or `all`.")
    key = [key] if isinstance(key, str) else key
    re_func = re.fullmatch if fixed else re.search
    case = re.IGNORECASE if (ignore_case and not fixed) else 0
    output = []
    for i in key:
        result = {}
        for k, v in zip(keys, values):
            if re_func(i, k, case):
                result[k] = result.get(k, []) + [v]
        if result:  # this prevents an empty output of [{}], which is Truthy
            output.append(result)
    if ties_method == "first":
        return _get_first_tie(output)
    return output


def _get_first_tie(dict_list: list):
    """Needs to be Python 3.6+ for this to work. Earlier versions don't preserve insertion order."""
    for d in dict_list:
        if len(d) > 1 or any(len(val) > 1 for val in d.values()):
            warnings.warn("Some inputs were not uniquely matched; "
                          "returning only the first match in each case.")
    first_items = [list(d.items())[0] for d in dict_list]
    return {i[0]: i[1][0] for i in first_items}
