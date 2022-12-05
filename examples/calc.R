library(myClim)

data <- mc_read_data("examples/data/calc/files_table.csv")
cleaned_data <- mc_prep_clean(data)
# user defined tz offset - 60 minutes for both localities
cleaned_data <- mc_prep_meta_locality(cleaned_data, list(LOC1=60, LOC2=60), "tz_offset")
# renaming sensors
cleaned_data <- mc_prep_rename_sensor(cleaned_data, list(TMS_T1="TMS_T1_secondary",
                                                         TMS_T2="TMS_T2_secondary",
                                                         TMS_T3="TMS_T3_secondary",
                                                         TMS_TMSmoisture="TMS_TMSmoisture_secondary"),
                                      serial_numbers=c("94184103", "94184105"))
# loading calibration values to TS_T
calib_table <- as.data.frame(tibble::tribble(
    ~serial_number,          ~sensor_id,                             ~datetime, ~cor_factor, ~cor_slope,
        "94184102",            "TMS_T1", lubridate::ymd_hm("2020-10-28 08:45"),         0.1,          1,
        "94184102",            "TMS_T1",          lubridate::ymd("2021-01-01"),       -0.05,       0.98,
        "94184102",   "TMS_TMSmoisture",     lubridate::ymd_h("2021-01-01 09"),        -0.5,       -1.2,
))
cleaned_data <- mc_prep_calib_load(cleaned_data, calib_table)
# calibrating of sensor TS_T
cleaned_data <- mc_prep_calib(cleaned_data, sensors = "TMS_T1")

# flatting data - loggers are deleted and sensors are moved under locality
agg_data <- mc_agg(cleaned_data)

# calculating vwc_moisture sensor from TMS_TMSmoisture and TMS_T1
agg_data <- mc_calc_vwc(agg_data, soiltype = "universal")

# calculating new snow sensor
agg_data <- mc_calc_snow(agg_data, "TMS_T2", output_sensor="snow")
agg_data <- mc_calc_snow(agg_data, "TMS_T2_secondary", output_sensor="snow_secondary", localities="LOC1")

# sensors overview
mc_info(agg_data)
# calculating snow day overview
mc_calc_snow_agg(agg_data, snow_sensor="snow")
mc_calc_snow_agg(agg_data, snow_sensor="snow_secondary", localities="LOC1")
# add new aggregated localities with median of day values
agg_data <- mc_agg(agg_data, c("min", "percentile"), "day", percentiles=c(50, 95), na.rm=TRUE, use_utc=FALSE)
# sensors overview
mc_info(agg_data)
