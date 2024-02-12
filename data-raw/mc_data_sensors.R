source("R/model.R")

mc_data_sensors <- new.env()

# logger sensors ================================================================================

mc_data_sensors[[mc_const_SENSOR_TMS_T1]] <- new("mc_Sensor")
mc_data_sensors[[mc_const_SENSOR_TMS_T1]]@sensor_id <- mc_const_SENSOR_TMS_T1
mc_data_sensors[[mc_const_SENSOR_TMS_T1]]@logger <- .model_const_LOGGER_TOMST_TMS
mc_data_sensors[[mc_const_SENSOR_TMS_T1]]@physical <- .model_const_PHYSICAL_T_C
mc_data_sensors[[mc_const_SENSOR_TMS_T1]]@description <- "Temperature of soil"
mc_data_sensors[[mc_const_SENSOR_TMS_T1]]@min_value <- -40
mc_data_sensors[[mc_const_SENSOR_TMS_T1]]@max_value <- 60
mc_data_sensors[[mc_const_SENSOR_TMS_T1]]@plot_color <- "#2000EEB0"
mc_data_sensors[[mc_const_SENSOR_TMS_T1]]@plot_line_width <- 2

mc_data_sensors[[mc_const_SENSOR_TMS_T2]] <- new("mc_Sensor")
mc_data_sensors[[mc_const_SENSOR_TMS_T2]]@sensor_id <- mc_const_SENSOR_TMS_T2
mc_data_sensors[[mc_const_SENSOR_TMS_T2]]@logger <- .model_const_LOGGER_TOMST_TMS
mc_data_sensors[[mc_const_SENSOR_TMS_T2]]@physical <- .model_const_PHYSICAL_T_C
mc_data_sensors[[mc_const_SENSOR_TMS_T2]]@description <- "Temperature on surface of soil"
mc_data_sensors[[mc_const_SENSOR_TMS_T2]]@min_value <- -50
mc_data_sensors[[mc_const_SENSOR_TMS_T2]]@max_value <- 60
mc_data_sensors[[mc_const_SENSOR_TMS_T2]]@plot_color <- "green4"

mc_data_sensors[[mc_const_SENSOR_TMS_T3]] <- new("mc_Sensor")
mc_data_sensors[[mc_const_SENSOR_TMS_T3]]@sensor_id <- mc_const_SENSOR_TMS_T3
mc_data_sensors[[mc_const_SENSOR_TMS_T3]]@logger <- .model_const_LOGGER_TOMST_TMS
mc_data_sensors[[mc_const_SENSOR_TMS_T3]]@physical <- .model_const_PHYSICAL_T_C
mc_data_sensors[[mc_const_SENSOR_TMS_T3]]@description <- "Temperature of air"
mc_data_sensors[[mc_const_SENSOR_TMS_T3]]@min_value <- -40
mc_data_sensors[[mc_const_SENSOR_TMS_T3]]@max_value <- 60
mc_data_sensors[[mc_const_SENSOR_TMS_T3]]@plot_color <- "red3"

mc_data_sensors[[mc_const_SENSOR_TMS_moist]] <- new("mc_Sensor")
mc_data_sensors[[mc_const_SENSOR_TMS_moist]]@sensor_id <- mc_const_SENSOR_TMS_moist
mc_data_sensors[[mc_const_SENSOR_TMS_moist]]@logger <- .model_const_LOGGER_TOMST_TMS
mc_data_sensors[[mc_const_SENSOR_TMS_moist]]@physical <- .model_const_PHYSICAL_moisture_raw
mc_data_sensors[[mc_const_SENSOR_TMS_moist]]@value_type <- .model_const_VALUE_TYPE_INTEGER
mc_data_sensors[[mc_const_SENSOR_TMS_moist]]@min_value <- 0
mc_data_sensors[[mc_const_SENSOR_TMS_moist]]@max_value <- 4000
mc_data_sensors[[mc_const_SENSOR_TMS_moist]]@plot_color <- "steelblue"
mc_data_sensors[[mc_const_SENSOR_TMS_moist]]@plot_line_width <- 2

mc_data_sensors[[mc_const_SENSOR_Thermo_T]] <- new("mc_Sensor")
mc_data_sensors[[mc_const_SENSOR_Thermo_T]]@sensor_id <- mc_const_SENSOR_Thermo_T
mc_data_sensors[[mc_const_SENSOR_Thermo_T]]@logger <- .model_const_LOGGER_TOMST_THERMODATALOGGER
mc_data_sensors[[mc_const_SENSOR_Thermo_T]]@physical <- .model_const_PHYSICAL_T_C
mc_data_sensors[[mc_const_SENSOR_Thermo_T]]@min_value <- -40
mc_data_sensors[[mc_const_SENSOR_Thermo_T]]@max_value <- 60
mc_data_sensors[[mc_const_SENSOR_Thermo_T]]@plot_color <- "#2000EEB0"
mc_data_sensors[[mc_const_SENSOR_Thermo_T]]@plot_line_width <- 2

mc_data_sensors[[mc_const_SENSOR_Dendro_T]] <- new("mc_Sensor")
mc_data_sensors[[mc_const_SENSOR_Dendro_T]]@sensor_id <- mc_const_SENSOR_Dendro_T
mc_data_sensors[[mc_const_SENSOR_Dendro_T]]@logger <- .model_const_LOGGER_TOMST_DENDROMETER
mc_data_sensors[[mc_const_SENSOR_Dendro_T]]@physical <- .model_const_PHYSICAL_T_C
mc_data_sensors[[mc_const_SENSOR_Dendro_T]]@min_value <- -40
mc_data_sensors[[mc_const_SENSOR_Dendro_T]]@max_value <- 60
mc_data_sensors[[mc_const_SENSOR_Dendro_T]]@plot_color <- "orange"
mc_data_sensors[[mc_const_SENSOR_Dendro_T]]@plot_line_width <- 2

mc_data_sensors[[mc_const_SENSOR_Dendro_raw]] <- new("mc_Sensor")
mc_data_sensors[[mc_const_SENSOR_Dendro_raw]]@sensor_id <- mc_const_SENSOR_Dendro_raw
mc_data_sensors[[mc_const_SENSOR_Dendro_raw]]@logger <- .model_const_LOGGER_TOMST_DENDROMETER
mc_data_sensors[[mc_const_SENSOR_Dendro_raw]]@physical <- .model_const_PHYSICAL_radius_raw
mc_data_sensors[[mc_const_SENSOR_Dendro_raw]]@min_value <- 1279
mc_data_sensors[[mc_const_SENSOR_Dendro_raw]]@max_value <- 34000
mc_data_sensors[[mc_const_SENSOR_Dendro_raw]]@value_type <- .model_const_VALUE_TYPE_INTEGER
mc_data_sensors[[mc_const_SENSOR_Dendro_raw]]@plot_color <- "brown"

mc_data_sensors[[mc_const_SENSOR_HOBO_T]] <- new("mc_Sensor")
mc_data_sensors[[mc_const_SENSOR_HOBO_T]]@sensor_id <- mc_const_SENSOR_HOBO_T
mc_data_sensors[[mc_const_SENSOR_HOBO_T]]@logger <- c(.model_const_LOGGER_HOBO_U23_001A, .model_const_LOGGER_HOBO_U23_004)
mc_data_sensors[[mc_const_SENSOR_HOBO_T]]@physical <- .model_const_PHYSICAL_T_C
mc_data_sensors[[mc_const_SENSOR_HOBO_T]]@min_value <- -40
mc_data_sensors[[mc_const_SENSOR_HOBO_T]]@max_value <- 70
mc_data_sensors[[mc_const_SENSOR_HOBO_T]]@plot_color <- "salmon"

mc_data_sensors[[mc_const_SENSOR_HOBO_RH]] <- new("mc_Sensor")
mc_data_sensors[[mc_const_SENSOR_HOBO_RH]]@sensor_id <- mc_const_SENSOR_HOBO_RH
mc_data_sensors[[mc_const_SENSOR_HOBO_RH]]@logger <- .model_const_LOGGER_HOBO_U23_001A
mc_data_sensors[[mc_const_SENSOR_HOBO_RH]]@physical <- .model_const_PHYSICAL_RH
mc_data_sensors[[mc_const_SENSOR_HOBO_RH]]@min_value <- 0
mc_data_sensors[[mc_const_SENSOR_HOBO_RH]]@max_value <- 100
mc_data_sensors[[mc_const_SENSOR_HOBO_RH]]@plot_color <- "skyblue"

mc_data_sensors[[mc_const_SENSOR_HOBO_EXTT]] <- new("mc_Sensor")
mc_data_sensors[[mc_const_SENSOR_HOBO_EXTT]]@sensor_id <- mc_const_SENSOR_HOBO_EXTT
mc_data_sensors[[mc_const_SENSOR_HOBO_EXTT]]@logger <- .model_const_LOGGER_HOBO_U23_004
mc_data_sensors[[mc_const_SENSOR_HOBO_EXTT]]@physical <- .model_const_PHYSICAL_T_C
mc_data_sensors[[mc_const_SENSOR_HOBO_EXTT]]@min_value <- -40
mc_data_sensors[[mc_const_SENSOR_HOBO_EXTT]]@max_value <- 100
mc_data_sensors[[mc_const_SENSOR_HOBO_EXTT]]@plot_color <- "red3"

# universal sensors ================================================================================

mc_data_sensors[[mc_const_SENSOR_count]] <- new("mc_Sensor")
mc_data_sensors[[mc_const_SENSOR_count]]@sensor_id <- mc_const_SENSOR_count
mc_data_sensors[[mc_const_SENSOR_count]]@value_type <- .model_const_VALUE_TYPE_INTEGER

mc_data_sensors[[mc_const_SENSOR_coverage]] <- new("mc_Sensor")
mc_data_sensors[[mc_const_SENSOR_coverage]]@sensor_id <- mc_const_SENSOR_coverage
mc_data_sensors[[mc_const_SENSOR_coverage]]@description <- "Ratio of valid values"
mc_data_sensors[[mc_const_SENSOR_coverage]]@min_value <- 0
mc_data_sensors[[mc_const_SENSOR_coverage]]@max_value <- 1

mc_data_sensors[[mc_const_SENSOR_snow_bool]] <- new("mc_Sensor")
mc_data_sensors[[mc_const_SENSOR_snow_bool]]@sensor_id <- mc_const_SENSOR_snow_bool
mc_data_sensors[[mc_const_SENSOR_snow_bool]]@value_type <- .model_const_VALUE_TYPE_LOGICAL
mc_data_sensors[[mc_const_SENSOR_snow_bool]]@description <- "Sensor detects snow"
mc_data_sensors[[mc_const_SENSOR_snow_bool]]@plot_color <- "lightskyblue"

mc_data_sensors[[mc_const_SENSOR_snow_fresh]] <- new("mc_Sensor")
mc_data_sensors[[mc_const_SENSOR_snow_fresh]]@sensor_id <- mc_const_SENSOR_snow_fresh
mc_data_sensors[[mc_const_SENSOR_snow_fresh]]@value_type <- .model_const_VALUE_TYPE_REAL
mc_data_sensors[[mc_const_SENSOR_snow_fresh]]@physical <- .model_const_PHYSICAL_l_cm
mc_data_sensors[[mc_const_SENSOR_snow_fresh]]@description <- "Height of newly fallen snow"

mc_data_sensors[[mc_const_SENSOR_snow_total]] <- new("mc_Sensor")
mc_data_sensors[[mc_const_SENSOR_snow_total]]@sensor_id <- mc_const_SENSOR_snow_total
mc_data_sensors[[mc_const_SENSOR_snow_total]]@value_type <- .model_const_VALUE_TYPE_REAL
mc_data_sensors[[mc_const_SENSOR_snow_total]]@physical <- .model_const_PHYSICAL_l_cm
mc_data_sensors[[mc_const_SENSOR_snow_total]]@description <- "Height of snow"

mc_data_sensors[[mc_const_SENSOR_precipitation]] <- new("mc_Sensor")
mc_data_sensors[[mc_const_SENSOR_precipitation]]@sensor_id <- mc_const_SENSOR_precipitation
mc_data_sensors[[mc_const_SENSOR_precipitation]]@value_type <- .model_const_VALUE_TYPE_REAL
mc_data_sensors[[mc_const_SENSOR_precipitation]]@physical <- .model_const_PHYSICAL_l_mm
mc_data_sensors[[mc_const_SENSOR_precipitation]]@description <- "Precipitation"

mc_data_sensors[[mc_const_SENSOR_sun_shine]] <- new("mc_Sensor")
mc_data_sensors[[mc_const_SENSOR_sun_shine]]@sensor_id <- mc_const_SENSOR_sun_shine
mc_data_sensors[[mc_const_SENSOR_sun_shine]]@value_type <- .model_const_VALUE_TYPE_REAL
mc_data_sensors[[mc_const_SENSOR_sun_shine]]@physical <- .model_const_PHYSICAL_t_h
mc_data_sensors[[mc_const_SENSOR_sun_shine]]@description <- "Time of sun shine"

mc_data_sensors[[mc_const_SENSOR_wind_speed]] <- new("mc_Sensor")
mc_data_sensors[[mc_const_SENSOR_wind_speed]]@sensor_id <- mc_const_SENSOR_wind_speed
mc_data_sensors[[mc_const_SENSOR_wind_speed]]@value_type <- .model_const_VALUE_TYPE_REAL
mc_data_sensors[[mc_const_SENSOR_wind_speed]]@physical <- .model_const_PHYSICAL_v
mc_data_sensors[[mc_const_SENSOR_wind_speed]]@description <- "Speed of wind"

mc_data_sensors[[mc_const_SENSOR_GDD]] <- new("mc_Sensor")
mc_data_sensors[[mc_const_SENSOR_GDD]]@sensor_id <- mc_const_SENSOR_GDD
mc_data_sensors[[mc_const_SENSOR_GDD]]@description <- "Growing degree days"

mc_data_sensors[[mc_const_SENSOR_FDD]] <- new("mc_Sensor")
mc_data_sensors[[mc_const_SENSOR_FDD]]@sensor_id <- mc_const_SENSOR_FDD
mc_data_sensors[[mc_const_SENSOR_FDD]]@description <- "Freezing degree days"

mc_data_sensors[[mc_const_SENSOR_VWC]] <- new("mc_Sensor")
mc_data_sensors[[mc_const_SENSOR_VWC]]@sensor_id <- mc_const_SENSOR_VWC
mc_data_sensors[[mc_const_SENSOR_VWC]]@physical <- .model_const_PHYSICAL_VWC
mc_data_sensors[[mc_const_SENSOR_VWC]]@min_value <- 0
mc_data_sensors[[mc_const_SENSOR_VWC]]@max_value <- 1
mc_data_sensors[[mc_const_SENSOR_VWC]]@plot_color <- "steelblue"
mc_data_sensors[[mc_const_SENSOR_VWC]]@plot_line_width <- 2

mc_data_sensors[[mc_const_SENSOR_T_C]] <- new("mc_Sensor")
mc_data_sensors[[mc_const_SENSOR_T_C]]@sensor_id <- mc_const_SENSOR_T_C
mc_data_sensors[[mc_const_SENSOR_T_C]]@physical <- .model_const_PHYSICAL_T_C
mc_data_sensors[[mc_const_SENSOR_T_C]]@plot_color <- "red"

mc_data_sensors[[mc_const_SENSOR_RH]] <- new("mc_Sensor")
mc_data_sensors[[mc_const_SENSOR_RH]]@sensor_id <- mc_const_SENSOR_RH
mc_data_sensors[[mc_const_SENSOR_RH]]@physical <- .model_const_PHYSICAL_RH
mc_data_sensors[[mc_const_SENSOR_RH]]@min_value <- 0
mc_data_sensors[[mc_const_SENSOR_RH]]@max_value <- 100

mc_data_sensors[[mc_const_SENSOR_dendro_l_um]] <- new("mc_Sensor")
mc_data_sensors[[mc_const_SENSOR_dendro_l_um]]@sensor_id <- mc_const_SENSOR_dendro_l_um
mc_data_sensors[[mc_const_SENSOR_dendro_l_um]]@physical <- .model_const_PHYSICAL_l_um
mc_data_sensors[[mc_const_SENSOR_dendro_l_um]]@description <- "Radius difference"

mc_data_sensors[[mc_const_SENSOR_VPD]] <- new("mc_Sensor")
mc_data_sensors[[mc_const_SENSOR_VPD]]@sensor_id <- mc_const_SENSOR_VPD
mc_data_sensors[[mc_const_SENSOR_VPD]]@value_type <- .model_const_VALUE_TYPE_REAL
mc_data_sensors[[mc_const_SENSOR_VPD]]@physical <- .model_const_PHYSICAL_p_kPa
mc_data_sensors[[mc_const_SENSOR_VPD]]@description <- "Vapor Pressure Deficit"

mc_data_sensors[[mc_const_SENSOR_real]] <- new("mc_Sensor")
mc_data_sensors[[mc_const_SENSOR_real]]@sensor_id <- mc_const_SENSOR_real
mc_data_sensors[[mc_const_SENSOR_real]]@value_type <- .model_const_VALUE_TYPE_REAL

mc_data_sensors[[mc_const_SENSOR_integer]] <- new("mc_Sensor")
mc_data_sensors[[mc_const_SENSOR_integer]]@sensor_id <- mc_const_SENSOR_integer
mc_data_sensors[[mc_const_SENSOR_integer]]@value_type <- .model_const_VALUE_TYPE_INTEGER

mc_data_sensors[[mc_const_SENSOR_logical]] <- new("mc_Sensor")
mc_data_sensors[[mc_const_SENSOR_logical]]@sensor_id <- mc_const_SENSOR_logical
mc_data_sensors[[mc_const_SENSOR_logical]]@value_type <- .model_const_VALUE_TYPE_LOGICAL

usethis::use_data(mc_data_sensors, overwrite = TRUE)
