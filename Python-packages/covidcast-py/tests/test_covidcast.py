import pytest

from covidcast import covidcast

def test_wrong_geo_type():
    with pytest.raises(ValueError):
        covidcast.signal("source", "signal", geo_type="not_a_real_geo")
