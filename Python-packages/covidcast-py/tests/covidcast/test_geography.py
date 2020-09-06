import pytest

from covidcast import geography


def test_fips_to_name():
    assert geography.fips_to_name("not a fips") == {}
    assert geography.fips_to_name("42003") == {"42003": "Allegheny County"}
    assert geography.fips_to_name("4200", ties_method="all") == [{"42000": ["Pennsylvania"],
                                                                  "42001": ["Adams County"],
                                                                  "42003": ["Allegheny County"],
                                                                  "42005": ["Armstrong County"],
                                                                  "42007": ["Beaver County"],
                                                                  "42009": ["Bedford County"]}]


def test_cbsa_to_name():
    assert geography.cbsa_to_name("38300") == {"38300": "Pittsburgh, PA"}
    assert geography.cbsa_to_name("not a cbsa") == {}


def test_abbr_to_name():
    assert geography.abbr_to_name("CA") == {"CA": "California"}
    assert geography.abbr_to_name("CAA") == {}
    assert geography.abbr_to_name(["CA", "PA"]) == {"CA": "California", "PA": "Pennsylvania"}
    assert geography.abbr_to_name(["CAAA", "PA"]) == {"PA": "Pennsylvania"}


def test_name_to_abbr():
    assert geography.name_to_abbr("California") == {"California": "CA"}
    assert geography.name_to_abbr("Californiaaaaaa") == {}
    assert geography.name_to_abbr(["California", "Pennsylvania"]) == \
           {"California": "CA", "Pennsylvania": "PA"}
    assert geography.name_to_abbr(["California", "Pennsylvaniaa"]) == {"California": "CA"}


def test_name_to_cbsa():
    assert geography.name_to_cbsa("Pittsburgh") == {"Pittsburgh, PA": "38300"}
    assert geography.name_to_cbsa("New", state="CA") == {}


def test_name_to_fips():
    assert geography.name_to_fips("Allegheny") == {"Allegheny County": "42003"}
    assert geography.name_to_fips("Miami") == {"Miami-Dade County": "12086"}
    assert geography.name_to_fips("Miami", ties_method="all") == \
           [{"Miami-Dade County": ["12086"], "Miami County": ["18103", "20121", "39109"]}]
    assert geography.name_to_fips(["Allegheny", "Miami", "New "], ties_method="all") == \
           [
                {"Allegheny County": ["42003"]},
                {"Miami-Dade County": ["12086"], "Miami County": ["18103", "20121", "39109"]},
                {"New Haven County": ["09009"], "New London County": ["09011"],
                 "New Castle County": ["10003"], "New Madrid County": ["29143"],
                 "New York County": ["36061"], "New Hanover County": ["37129"],
                 "New Kent County": ["51127"], "New Hampshire": ["33000"],
                 "New Jersey": ["34000"], "New Mexico": ["35000"],
                 "New York": ["36000"]}
           ]
    assert geography.name_to_fips(["Allegheny", "Miami", "New "]) == {"Allegheny County": "42003",
                                                                      "Miami-Dade County": "12086",
                                                                      "New Haven County": "09009"}
    assert geography.name_to_fips("New ", ties_method="all", state="ny") == \
           [{'New York': ['36000'], 'New York County': ['36061']}]


def test__lookup():
    # test invalid ties method
    with pytest.raises(ValueError):
        geography._lookup(None, None, None, ties_method="not a real method")
    assert geography._lookup(["a", "b"], ["a", "b", "c"], ["x", "y", "z"]) == {"a": "x", "b": "y"}
    assert geography._lookup(["A"], ["a", "a"], ["x", "y"]) == {}
    assert geography._lookup(["A"], ["a", "a"], ["x", "y"], ignore_case=True) == {"a": "x"}
    assert geography._lookup(["a"], ["a", "a"], ["x", "y"],
                             ties_method="all") == [{"a": ["x", "y"]}]
    assert geography._lookup(["A"], ["a", "a"], ["x", "y"],
                             ignore_case=True, ties_method="all") == [{"a": ["x", "y"]}]


def test__get_first_tie():
    with pytest.warns(UserWarning):
        assert geography._get_first_tie([{"a": ["x", "y"]}]) == {"a": "x"}
    with pytest.warns(UserWarning):
        assert geography._get_first_tie([{"a": ["x", "y"], "b": ["i", "j", "k"]}]) == {"a": "x"}
    with pytest.warns(UserWarning):
        assert geography._get_first_tie([{"a": ["x", "y"]},
                                         {"b": ["i", "j", "k"]}]) == {"a": "x", "b": "i"}
    assert geography._get_first_tie([{"a": ["x"]}]) == {"a": "x"}
