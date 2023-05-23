import covidcast
from datetime import date
from matplotlib import pyplot as plt
data = covidcast.signal("fb-survey", "smoothed_cli", start_day=date(2020, 8, 4), end_day=date(2020, 8, 4), geo_type="county")
geo_data = covidcast.get_geo_df(data)
CA = geo_data.loc[geo_data.state_fips == "06",:]
CA = CA.to_crs("EPSG:3395")
CA.plot(column="value", figsize=(5,5), legend=True)
plt.axis("off")
plt.show()