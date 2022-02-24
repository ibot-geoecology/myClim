source("R/model.R")

mc_data_sensors <- new.env()

# logger sensors ================================================================================

mc_data_sensors[[.model_const_SENSOR_TMS_T1]] <- new("mc_Sensor")
mc_data_sensors[[.model_const_SENSOR_TMS_T1]]@sensor_id <- .model_const_SENSOR_TMS_T1
mc_data_sensors[[.model_const_SENSOR_TMS_T1]]@logger <- .model_const_LOGGER_TOMST_TMS
mc_data_sensors[[.model_const_SENSOR_TMS_T1]]@physical <- .model_const_PHYSICAL_T_C
mc_data_sensors[[.model_const_SENSOR_TMS_T1]]@default_height <- -0.08
mc_data_sensors[[.model_const_SENSOR_TMS_T1]]@min_value <- -40
mc_data_sensors[[.model_const_SENSOR_TMS_T1]]@max_value <- 60
mc_data_sensors[[.model_const_SENSOR_TMS_T1]]@plot_color <- "#2000EEB0"
mc_data_sensors[[.model_const_SENSOR_TMS_T1]]@plot_line_width <- 2

mc_data_sensors[[.model_const_SENSOR_TMS_T2]] <- new("mc_Sensor")
mc_data_sensors[[.model_const_SENSOR_TMS_T2]]@sensor_id <- .model_const_SENSOR_TMS_T2
mc_data_sensors[[.model_const_SENSOR_TMS_T2]]@logger <- .model_const_LOGGER_TOMST_TMS
mc_data_sensors[[.model_const_SENSOR_TMS_T2]]@physical <- .model_const_PHYSICAL_T_C
mc_data_sensors[[.model_const_SENSOR_TMS_T2]]@default_height <- 0
mc_data_sensors[[.model_const_SENSOR_TMS_T2]]@min_value <- -50
mc_data_sensors[[.model_const_SENSOR_TMS_T2]]@max_value <- 60
mc_data_sensors[[.model_const_SENSOR_TMS_T2]]@plot_color <- "green4"

mc_data_sensors[[.model_const_SENSOR_TMS_T3]] <- new("mc_Sensor")
mc_data_sensors[[.model_const_SENSOR_TMS_T3]]@sensor_id <- .model_const_SENSOR_TMS_T3
mc_data_sensors[[.model_const_SENSOR_TMS_T3]]@logger <- .model_const_LOGGER_TOMST_TMS
mc_data_sensors[[.model_const_SENSOR_TMS_T3]]@physical <- .model_const_PHYSICAL_T_C
mc_data_sensors[[.model_const_SENSOR_TMS_T3]]@default_height <- 0.15
mc_data_sensors[[.model_const_SENSOR_TMS_T3]]@min_value <- -40
mc_data_sensors[[.model_const_SENSOR_TMS_T3]]@max_value <- 60
mc_data_sensors[[.model_const_SENSOR_TMS_T3]]@plot_color <- "red3"

mc_data_sensors[[.model_const_SENSOR_TMS_TMSmoisture]] <- new("mc_Sensor")
mc_data_sensors[[.model_const_SENSOR_TMS_TMSmoisture]]@sensor_id <- .model_const_SENSOR_TMS_TMSmoisture
mc_data_sensors[[.model_const_SENSOR_TMS_TMSmoisture]]@logger <- .model_const_LOGGER_TOMST_TMS
mc_data_sensors[[.model_const_SENSOR_TMS_TMSmoisture]]@physical <- .model_const_PHYSICAL_TMSmoisture
mc_data_sensors[[.model_const_SENSOR_TMS_TMSmoisture]]@default_height <- -0.07
mc_data_sensors[[.model_const_SENSOR_TMS_TMSmoisture]]@value_type <- .model_const_VALUE_TYPE_INTEGER
mc_data_sensors[[.model_const_SENSOR_TMS_TMSmoisture]]@min_value <- 0
mc_data_sensors[[.model_const_SENSOR_TMS_TMSmoisture]]@max_value <- 4000
mc_data_sensors[[.model_const_SENSOR_TMS_TMSmoisture]]@plot_color <- "steelblue"
mc_data_sensors[[.model_const_SENSOR_TMS_TMSmoisture]]@plot_line_width <- 2

mc_data_sensors[[.model_const_SENSOR_TM_T]] <- new("mc_Sensor")
mc_data_sensors[[.model_const_SENSOR_TM_T]]@sensor_id <- .model_const_SENSOR_TM_T
mc_data_sensors[[.model_const_SENSOR_TM_T]]@logger <- .model_const_LOGGER_TOMST_THERMODATALOGGER
mc_data_sensors[[.model_const_SENSOR_TM_T]]@physical <- .model_const_PHYSICAL_T_C
mc_data_sensors[[.model_const_SENSOR_TM_T]]@min_value <- -40
mc_data_sensors[[.model_const_SENSOR_TM_T]]@max_value <- 60
mc_data_sensors[[.model_const_SENSOR_TM_T]]@plot_color <- "#2000EEB0"
mc_data_sensors[[.model_const_SENSOR_TM_T]]@plot_line_width <- 2

# universal sensors ================================================================================

mc_data_sensors[[.model_const_SENSOR_snow_bool]] <- new("mc_Sensor")
mc_data_sensors[[.model_const_SENSOR_snow_bool]]@sensor_id <- .model_const_SENSOR_snow_bool
mc_data_sensors[[.model_const_SENSOR_snow_bool]]@value_type <- .model_const_VALUE_TYPE_LOGICAL

mc_data_sensors[[.model_const_SENSOR_count]] <- new("mc_Sensor")
mc_data_sensors[[.model_const_SENSOR_count]]@sensor_id <- .model_const_SENSOR_count
mc_data_sensors[[.model_const_SENSOR_count]]@value_type <- .model_const_VALUE_TYPE_INTEGER

mc_data_sensors[[.model_const_SENSOR_coverage]] <- new("mc_Sensor")
mc_data_sensors[[.model_const_SENSOR_coverage]]@sensor_id <- .model_const_SENSOR_coverage

mc_data_sensors[[.model_const_SENSOR_GDD]] <- new("mc_Sensor")
mc_data_sensors[[.model_const_SENSOR_GDD]]@sensor_id <- .model_const_SENSOR_GDD

mc_data_sensors[[.model_const_SENSOR_FDD]] <- new("mc_Sensor")
mc_data_sensors[[.model_const_SENSOR_FDD]]@sensor_id <- .model_const_SENSOR_FDD

# physical sensors ================================================================================

mc_data_sensors[[.model_const_SENSOR_T_C]] <- new("mc_Sensor")
mc_data_sensors[[.model_const_SENSOR_T_C]]@sensor_id <- .model_const_SENSOR_T_C
mc_data_sensors[[.model_const_SENSOR_T_C]]@physical <- .model_const_PHYSICAL_T_C

mc_data_sensors[[.model_const_SENSOR_moisture]] <- new("mc_Sensor")
mc_data_sensors[[.model_const_SENSOR_moisture]]@sensor_id <- .model_const_SENSOR_moisture
mc_data_sensors[[.model_const_SENSOR_moisture]]@physical <- .model_const_PHYSICAL_moisture
mc_data_sensors[[.model_const_SENSOR_moisture]]@min_value <- 0
mc_data_sensors[[.model_const_SENSOR_moisture]]@max_value <- 1
mc_data_sensors[[.model_const_SENSOR_moisture]]@plot_color <- "steelblue"
mc_data_sensors[[.model_const_SENSOR_moisture]]@plot_line_width <- 2

mc_data_sensors[[.model_const_SENSOR_RH_perc]] <- new("mc_Sensor")
mc_data_sensors[[.model_const_SENSOR_RH_perc]]@sensor_id <- .model_const_SENSOR_RH_perc
mc_data_sensors[[.model_const_SENSOR_RH_perc]]@physical <- .model_const_PHYSICAL_RH_perc
mc_data_sensors[[.model_const_SENSOR_RH_perc]]@min_value <- 0
mc_data_sensors[[.model_const_SENSOR_RH_perc]]@max_value <- 100

mc_data_sensors[[.model_const_SENSOR_l_cm]] <- new("mc_Sensor")
mc_data_sensors[[.model_const_SENSOR_l_cm]]@sensor_id <- .model_const_SENSOR_l_cm
mc_data_sensors[[.model_const_SENSOR_l_cm]]@physical <- .model_const_PHYSICAL_l_cm

mc_data_sensors[[.model_const_SENSOR_l_mm]] <- new("mc_Sensor")
mc_data_sensors[[.model_const_SENSOR_l_mm]]@sensor_id <- .model_const_SENSOR_l_mm
mc_data_sensors[[.model_const_SENSOR_l_mm]]@physical <- .model_const_PHYSICAL_l_mm

mc_data_sensors[[.model_const_SENSOR_v]] <- new("mc_Sensor")
mc_data_sensors[[.model_const_SENSOR_v]]@sensor_id <- .model_const_SENSOR_v
mc_data_sensors[[.model_const_SENSOR_v]]@physical <- .model_const_PHYSICAL_v

mc_data_sensors[[.model_const_SENSOR_t_h]] <- new("mc_Sensor")
mc_data_sensors[[.model_const_SENSOR_t_h]]@sensor_id <- .model_const_SENSOR_t_h
mc_data_sensors[[.model_const_SENSOR_t_h]]@physical <- .model_const_PHYSICAL_t_h

usethis::use_data(mc_data_sensors, overwrite = TRUE)
