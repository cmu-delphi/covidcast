import pytest

from covidcast import geography


@pytest.mark.parametrize("test_key, test_kwargs, expected", [
    (
            "not a fips",
            {},
            {}
    ),
    (
            "42003",
            {},
            {"42003": "Allegheny County"}),
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
            {"42000": "Pennsylvania"}
    )
])
def test_fips_to_name(test_key, test_kwargs, expected):
    assert geography.fips_to_name(test_key, **test_kwargs) == expected


@pytest.mark.parametrize("test_key, test_kwargs, expected", [
    (
            "not a cbsa",
            {},
            {}
    ),
    (
            "38300",
            {},
            {"38300": "Pittsburgh, PA"}
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
            {"CA": "California"}
    ),
    (
            "CAA",
            {},
            {}
    ),
    (
            ["CA",
             "PA"],
            {},
            {"CA": "California", "PA": "Pennsylvania"}
    ),
    (
            ["CAAA", "PA"],
            {},
            {"PA": "Pennsylvania"}
    ),
])
def test_abbr_to_name(test_key, test_kwargs, expected):
    assert geography.abbr_to_name(test_key, **test_kwargs) == expected


@pytest.mark.parametrize("test_key, test_kwargs, expected", [
    (
            "California",
            {},
            {"California": "CA"}
    ),
    (
            "Californiaaaaa",
            {},
            {}
    ),
    (
            ["California", "Pennsylvania"],
            {},
            {"California": "CA", "Pennsylvania": "PA"}
    ),
    (
            ["California", "Pennsylvaniaa"],
            {},
            {"California": "CA"}
    ),
])
def test_name_to_abbr(test_key, test_kwargs, expected):
    assert geography.name_to_abbr(test_key, **test_kwargs) == expected


@pytest.mark.parametrize("test_key, test_kwargs, expected", [
    (
            "Pittsburgh",
            {},
            {"Pittsburgh, PA": "38300"}
    ),
    (
            "New",
            {"state": "CA"},
            {}
    ),
    (
            "New",
            {},
            {"Boston-Cambridge-Newton, MA-NH": "14460"}
    )
])
def test_name_to_cbsa(test_key, test_kwargs, expected):
    assert geography.name_to_cbsa(test_key, **test_kwargs) == expected


@pytest.mark.parametrize("test_key, test_kwargs, expected", [
    (
            "Allegheny",
            {},
            {"Allegheny County": "42003"}
    ),
    (
            "Miami",
            {},
            {"Miami-Dade County": "12086"}
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
            {"Allegheny County": "42003", "Miami-Dade County": "12086", "New Haven County": "09009"}
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
            {"a": "x", "b": "y"}
    ),
    (
            (["a", "b"], ["a", "a", "aa", "b"], ["w", "x", "y", "z"]),
            {"ties_method": "all"},
            [{"a": ["w", "x"], "aa": ["y"]}, {"b": ["z"]}]
    ),
    (
            (["a", "b"], ["A", "aa", "b"], ["x", "y", "z"]),
            {"ignore_case": True},
            {"A": "x", "b": "z"}
    ),
    (
            (["a", "b"], ["a", "aa", "b"], ["x", "y", "z"]),
            {"fixed": True},
            {"a": "x", "b": "z"}
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
            [{"b": ["z"]}]
    ),
    (
            ("A", ["aa", "a"], ["x", "y"]),
            {},
            {}
    ),
    (
            ("A", ["aa", "a"], ["x", "y"]),
            {"ignore_case": True},
            {"aa": "x"}
    ),
    (
            (["A"], ["aa", "a"], ["x", "y"]),
            {"ignore_case": True},
            {"aa": "x"}
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
            {"aa": "x"}
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
            []
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


@pytest.mark.parametrize("test_dict_list, expected_return, expected_stdout", [
    (
            [{"a": ["x", "y"]}],
            {"a": "x"},
            "Some inputs were not uniquely matched; returning only the first match in "
            "each case. To return all matches, set `ties_method='all'`\n"
    ),
    (
            [{"a": ["x", "y"], "b": ["i", "j", "k"]}],
            {"a": "x"},
            "Some inputs were not uniquely matched; returning only the first match in "
            "each case. To return all matches, set `ties_method='all'`\n"
    ),
    (
            [{"a": ["x", "y"]}, {"b": ["i", "j", "k"]}],
            {"a": "x", "b": "i"},
            "Some inputs were not uniquely matched; returning only the first match in "
            "each case. To return all matches, set `ties_method='all'`\n"
    ),
    (
            [{"a": ["x"]}],
            {"a": "x"},
            ""
    )
])
def test__get_first_tie(test_dict_list, expected_return, expected_stdout, capfd):
    assert geography._get_first_tie(test_dict_list) == expected_return
    out, _ = capfd.readouterr()
    assert out == expected_stdout
