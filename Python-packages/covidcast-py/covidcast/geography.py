import re

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
    df = MSA_CENSUS.loc[MSA_CENSUS.STATE == state.upper()] if state else MSA_CENSUS
    return _lookup(name, df.NAME, df.CBSA, ignore_case, fixed, ties_method)


def name_to_fips(name, ignore_case=False, fixed=False, ties_method="first", state=None):
    df = COUNTY_CENSUS.loc[COUNTY_CENSUS.STNAME == abbr_to_name(state)[state].upper()] if state \
        else COUNTY_CENSUS
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
                if ties_method == "first":
                    return {k: v}
                result[k] = result.get(k, []) + [v]
        if result:  # this prevents an empty output of [{}], which is Truthy
            output.append(result)
    return output
