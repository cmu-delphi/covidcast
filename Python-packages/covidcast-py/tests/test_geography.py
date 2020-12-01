import pytest

from covidcast import geography


@pytest.mark.parametrize("test_key, test_kwargs, expected", [
    (
            "not a fips",
            {},
            [None]
    ),
    (
            "42003",
            {},
            ["Allegheny County"],
    ),
    (
            "4200",
            {"ties_method": "all"},
            [{"42000": ["Pennsylvania"], "42001": ["Adams County"],
              "42003": ["Allegheny County"], "42005": ["Armstrong County"],
              "42007": ["Beaver County"], "42009": ["Bedford County"]}]
    ),
    (
            "4200",
            {},
            ["Pennsylvania"]
    )
])
def test_fips_to_name(test_key, test_kwargs, expected):
    assert geography.fips_to_name(test_key, **test_kwargs) == expected


@pytest.mark.parametrize("test_key, test_kwargs, expected", [
    (
            "not a cbsa",
            {},
            [None]
    ),
    (
            "38300",
            {},
            ["Pittsburgh, PA"]
    ),
    (
            "389",
            {"ties_method": "all"},
            [{"38900": ["Portland-Vancouver-Hillsboro, OR-WA"], "38940": ["Port St. Lucie, FL"]}]
    ),
    (
            ["38300", "389"],
            {"ties_method": "all"},
            [
                {"38300": ["Pittsburgh, PA"]},
                {"38900": ["Portland-Vancouver-Hillsboro, OR-WA"], "38940": ["Port St. Lucie, FL"]}
            ]

    )
])
def test_cbsa_to_name(test_key, test_kwargs, expected):
    assert geography.cbsa_to_name(test_key, **test_kwargs) == expected


@pytest.mark.parametrize("test_key, test_kwargs, expected", [
    (
            "CA",
            {},
            ["California"]
    ),
    (
            "CAA",
            {},
            [None]
    ),
    (
            ["CA", "PA"],
            {},
            ["California", "Pennsylvania"]
    ),
    (
            ["CAAA", "PA"],
            {},
            [None, "Pennsylvania"]
    ),
])
def test_abbr_to_name(test_key, test_kwargs, expected):
    assert geography.abbr_to_name(test_key, **test_kwargs) == expected


@pytest.mark.parametrize("test_key, test_kwargs, expected", [
    (
            "California",
            {},
            ["CA"]
    ),
    (
            "Californiaaaaa",
            {},
            [None]
    ),
    (
            ["California", "Pennsylvania"],
            {},
            ["CA", "PA"]
    ),
    (
            ["California", "Pennsylvaniaa"],
            {},
            ["CA", None]
    ),
    (
            ["California", "Pennsylvania", "California"],
            {},
            ["CA", "PA", "CA"]
    ),
])
def test_name_to_abbr(test_key, test_kwargs, expected):
    assert geography.name_to_abbr(test_key, **test_kwargs) == expected


@pytest.mark.parametrize("test_key, test_kwargs, expected", [
    (
            "12",
            {},
            ["FL"]
    ),
    (
            "7",
            {"ties_method": "all"},
            [{'17000': ['IL'],
              '27000': ['MN'],
              '37000': ['NC'],
              '47000': ['TN'],
              '72000': ['PR']}]

    ),
    (
            ["ABC"],
            {},
            [None]
    ),
])
def test_fips_to_abbr(test_key, test_kwargs, expected):
    assert geography.fips_to_abbr(test_key, **test_kwargs) == expected


@pytest.mark.parametrize("test_key, test_kwargs, expected", [
    (
            "Pittsburgh",
            {},
            ["38300"]
    ),
    (
            "New",
            {"state": "CA"},
            [None]
    ),
    (
            "New",
            {},
            ["14460"]
    )
])
def test_name_to_cbsa(test_key, test_kwargs, expected):
    assert geography.name_to_cbsa(test_key, **test_kwargs) == expected


@pytest.mark.parametrize("test_key, test_kwargs, expected", [
    (
            "PA",
            {},
            ["42000"]
    ),
    (
            "New",
            {},
            [None]
    ),
    (
            ["PA", "ca"],
            {"ignore_case": True},
            ["42000", "06000"]
    )
])
def test_abbr_to_fips(test_key, test_kwargs, expected):
    assert geography.abbr_to_fips(test_key, **test_kwargs) == expected


@pytest.mark.parametrize("test_key, test_kwargs, expected", [
    (
            "Allegheny",
            {},
            ["42003"]
    ),
    (
            "Miami",
            {},
            ["12086"]
    ),
    (
            "Miami",
            {"ties_method": "all"},
            [{"Miami-Dade County": ["12086"], "Miami County": ["18103", "20121", "39109"]}]
     ),
    (
            ["Allegheny", "Miami", "New "],
            {"ties_method": "all"},
            [
                {"Allegheny County": ["42003"]},
                {"Miami-Dade County": ["12086"],
                 "Miami County": ["18103", "20121", "39109"]},
                {"New Haven County": ["09009"], "New London County": ["09011"],
                 "New Castle County": ["10003"], "New Madrid County": ["29143"],
                 "New York County": ["36061"], "New Hanover County": ["37129"],
                 "New Kent County": ["51127"], "New Hampshire": ["33000"],
                 "New Jersey": ["34000"], "New Mexico": ["35000"],
                 "New York": ["36000"]}
            ]
    ),
    (
            ["Allegheny", "Miami", "New "],
            {},
            ["42003", "12086", "09009"]
    ),
    (
            "New ",
            {"ties_method": "all", "state": "ny"},
            [{"New York": ["36000"], "New York County": ["36061"]}]
    ),
])
def test_name_to_fips(test_key, test_kwargs, expected):
    assert geography.name_to_fips(test_key, **test_kwargs) == expected


@pytest.mark.parametrize("test_args, test_kwargs, expected", [
    (
            (["a", "b"], ["a", "b", "c"], ["x", "y", "z"]),
            {},
            ["x", "y"]
    ),
    (
            (["a", "b"], ["a", "a", "aa", "b"], ["w", "x", "y", "z"]),
            {"ties_method": "all"},
            [{"a": ["w", "x"], "aa": ["y"]}, {"b": ["z"]}]
    ),
    (
            (["a", "b"], ["A", "aa", "b"], ["x", "y", "z"]),
            {"ignore_case": True},
            ["x", "z"]
    ),
    (
            (["a", "b"], ["a", "aa", "b"], ["x", "y", "z"]),
            {"fixed": True},
            ["x", "z"]
    ),
    (
            (["a", "b"], ["a", "aa", "b"], ["x", "y", "z"]),
            {"ties_method": "all", "fixed": True},
            [{"a": ["x"]}, {"b": ["z"]}]
    ),
    (
            (["a", "b"], ["A", "aa", "b"], ["x", "y", "z"]),
            {"ties_method": "all", "ignore_case": True},
            [{"A": ["x"], "aa": ["y"]}, {"b": ["z"]}]
    ),
    (
            (["a", "b"], ["A", "aa", "b"], ["x", "y", "z"]),
            {"ties_method": "all", "fixed": True, "ignore_case": True},
            [{}, {"b": ["z"]}]
    ),
    (
            ("A", ["aa", "a"], ["x", "y"]),
            {},
            [None]
    ),
    (
            ("A", ["aa", "a"], ["x", "y"]),
            {"ignore_case": True},
            ["x"]
    ),
    (
            (["A"], ["aa", "a"], ["x", "y"]),
            {"ignore_case": True},
            ["x"]
    ),
    (
            ("A", ["aa", "a"], ["x", "y"]),
            {"ignore_case": True, "ties_method": "all"},
            [{"aa": ["x"], "a": ["y"]}]
    ),
    (
            (["A"], ["aa", "a"], ["x", "y"]),
            {"ignore_case": True, "ties_method": "all"},
            [{"aa": ["x"], "a": ["y"]}]
    ),
    (
            ("A", ["aa", "a"], ["x", "y"]),
            {"ignore_case": True, "ties_method": "all"},
            [{"aa": ["x"], "a": ["y"]}]
    ),
    (
            ("A", ["aa", "a"], ["x", "y"]),
            {"ignore_case": True},
            ["x"]
    ),
    (
            ("a", ["a", "a"], ["x", "y"]),
            {"ties_method": "all"},
            [{"a": ["x", "y"]}]
    ),
    (
            ("a", ["aa", "a"], ["x", "y"]),
            {"ties_method": "all"},
            [{"aa": ["x"], "a": ["y"]}]
    ),
    (
            ("a", ["aa", "a"], ["x", "y"]),
            {"ties_method": "all", "fixed": True},
            [{"a": ["y"]}]
    ),
    (
            ("A", ["aa", "a"], ["x", "y"]),
            {"ties_method": "all", "fixed": True, "ignore_case": True},
            [{}]
    ),
    (
            ("A", ["a", "a"], ["x", "y"]),
            {"ties_method": "all", "ignore_case": True},
            [{"a": ["x", "y"]}]
    )
])
def test__lookup(test_args, test_kwargs, expected):
    assert geography._lookup(*test_args, **test_kwargs) == expected
    with pytest.raises(ValueError):
        geography._lookup(None, None, None, ties_method="not a real method")


@pytest.mark.parametrize("test_dict_list, expected_return, warn, expected_warning", [
    (
            [{"a": ["x", "y"]}],
            ["x"],
            1,
            "Some inputs were not uniquely matched; returning only the first match in "
            "each case. To return all matches, set `ties_method='all'`"
    ),
    (
            [{"a": ["x", "y"], "b": ["i", "j", "k"]}],
            ["x"],
            1,
            "Some inputs were not uniquely matched; returning only the first match in "
            "each case. To return all matches, set `ties_method='all'`"
    ),
    (
            [{"a": ["x", "y"]}, {"b": ["i", "j", "k"]}],
            ["x", "i"],
            1,
            "Some inputs were not uniquely matched; returning only the first match in "
            "each case. To return all matches, set `ties_method='all'`"
    ),
    (
            [{"a": ["x"]}],
            ["x"],
            0,
            None
    )
])

def test__get_first_tie(test_dict_list, expected_return, warn, expected_warning):
    if warn:
        with pytest.warns(UserWarning) as record:
            assert geography._get_first_tie(test_dict_list) == expected_return
            assert record[0].message.args[0] == expected_warning
    else:
        assert geography._get_first_tie(test_dict_list) == expected_return
