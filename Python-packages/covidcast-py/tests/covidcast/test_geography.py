from covidcast import geography

def test__lookup():
    assert 1

def test_fips_to_name():
    assert geography.fips_to_name("42003") == {"42003": "Allegheny County"}
    assert geography.fips_to_name("4200", ties_method="all") == [{"42001": ["Adams County"],
                                                                  "42003": ["Allegheny County"],
                                                                  "42005": ["Armstrong County"],
                                                                  "42007": ["Beaver County"],
                                                                  "42009": ["Bedford County"]}]


def test_cbsa_to_name():
    assert geography.cbsa_to_name("38300") == {"38300": "Pittsburgh, PA"}


def test_abbr_to_name():
    assert 1

def test_name_to_abbr():
    assert 1


def test_name_to_cbsa():
    assert geography.name_to_cbsa("Pittsburgh") == {"Pittsburgh, PA": "38300"}


def test_name_to_fips():
    assert geography.name_to_fips("Allegheny") == {"Allegheny County": "42003"}
    assert geography.name_to_fips("Miami") == {"Miami-Dade County": "12086"}
    # TODO test uniqe warning once it's implemented
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
                 "New York": ["36000"]
                 }
           ]
    assert geography.name_to_fips(["Allegheny", "Miami", "New "]) == {"Allegheny County": "42003",
                                                                      "Miami-Dade County": "12086",
                                                                      "New Haven County": "09009"}

