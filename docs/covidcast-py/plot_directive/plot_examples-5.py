import covidcast
from datetime import date
from matplotlib import pyplot as plt
data = covidcast.signal("fb-survey", "smoothed_cli", start_day = date(2020,8,4), end_day = date(2020,8,4), geo_type = "msa")
covidcast.plot(data, plot_type="bubble")
plt.show()