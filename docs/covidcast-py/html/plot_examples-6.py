import covidcast
from datetime import date
from matplotlib import pyplot as plt
data = covidcast.signal("fb-survey", "smoothed_cli", start_day=date(2020,8,3), end_day=date(2020,8,4), geo_type="county")
covidcast.plot(data, cmap="viridis", edgecolor="0.8")
plt.show()