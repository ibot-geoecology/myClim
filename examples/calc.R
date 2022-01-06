library(microclim)

data <- mc_read_csv("examples/data/calc/files_table.csv")
cleaned_data <- mc_prep_clean(data)
# user defined tz offset - 60 minutes for both localities
cleaned_data <- mc_prep_user_tz(cleaned_data, list(LOC1=60, LOC2=60))
# renaming sensors
cleaned_data <- mc_prep_rename_sensor(cleaned_data, list(TMS_T1="TMS_T1_secondary",
                                                         TMS_T2="TMS_T2_secondary",
                                                         TMS_T3="TMS_T3_secondary",
                                                         TMS_TMSmoisture="TMS_TMSmoisture_secondary"),
                                      serial_numbers=c("94184103", "94184105"))
# flatting data - loggers are deleted and sensors are moved under locality
calc_data <- mc_agg(cleaned_data)
# calculating new snow sensor
calc_data <- mc_calc_snow(calc_data, "TMS_T2", output_sensor="snow")
calc_data <- mc_calc_snow(calc_data, "TMS_T2_secondary", output_sensor="snow_secondary", localities="LOC1")
# sensors overview
mc_info(calc_data)
# calculating snow day overview
mc_calc_snow_agg(calc_data, snow_sensor="snow")
mc_calc_snow_agg(calc_data, snow_sensor="snow_secondary", localities="LOC1")
# add new aggregated localities with median of day values
calc_data <- mc_agg(calc_data, c("min", "percentile"), "day", percentiles=c(50, 95), na.rm=TRUE, use_utc=FALSE)
# sensors overview
mc_info(calc_data)
