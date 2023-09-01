library(myClim)
library(lubridate)

load("examples/data/chmi/airHumid_merged.RData")
load("examples/data/chmi/airTmax_merged.RData")
load("examples/data/chmi/airTmin_merged.RData")

airHumid$date <- as.POSIXct(ymd(airHumid$date, tz="UTC"))
airTmin$date <- as.POSIXct(ymd(airTmin$date, tz="UTC"))
airTmax$date <- as.POSIXct(ymd(airTmax$date, tz="UTC"))
data_items <- list()
data_items$humidity <- mc_read_wide(airHumid, sensor_id = "RH", "humidity")
data_items$tmin <- mc_read_wide(airTmin, sensor_id = "T_C", "T_min")
data_items$tmax <- mc_read_wide(airTmax, sensor_id = "T_C", "T_max")

data <- mc_prep_merge(data_items)
cropped_data <- mc_prep_crop(data, ymd("2015-01-01"))
cleaned_data <- mc_prep_clean(cropped_data)
agg_data <- mc_agg(cleaned_data)

