#' Example data in prep-format. Data aren't cleaned.
"mc_data_example_source"

#' Example cleaned data in prep-format.
"mc_data_example_clean"

#' Example data in calc-format and 15 minutes step.
"mc_data_example_calc"

#' Formats of source data files
#' 
#' @description
#' R object of class environment with the definitions how to
#' parse specific microclimatic logger files. In case you would like to add
#' new, unsupported logger, this is the place where the reading key is stored.
#'
#' @details
#' Package myClim support formats TOMST, TOMST_join and HOBO
#'
#' **TOMST**
#'
#' TOMST data format has stable structure. Datetime is in UTC. Name of data file is in format data_<serial_number>_<x>.csv.
#' Value serial_number can be automatically detected from file name. Supported loggers are TMS and ThermoDataLogger.
#'
#' **TOMST_join**
#'
#' TOMST_join data format is custom format for internal using of Institute of Botany of the Czech Academy of Sciences. 
#' It is the output of joinTMS.exe modified, checked, curated and validated by Lucia.  
#'
#' **HOBO**
#'
#' HOBO data format is export format from software HOBOware of Onset company. Format is very variable
#' and can be adjusted by user in preferences of HOBOware. Strucuture of HOBO files format can be partly detected automatically from header of data.
#' Except of  format of date-time (`date_format`) which must be set manually in myClim reading functions ([mc_read_files()], [mc_read_data()]).
#' Date and time separated in more columns is not supported in myClim reading. If time zone is not defined in header of HOBO txt or csv file
#' and is not UTC, then `tz_offset` must be filled in while reading. UTF-8 encoding of HOBO file is required for reding to myClim.
#'
#' @seealso [myClim::mc_DataFormat], [mc_TOMSTDataFormat-class], [mc_TOMSTJoinDataFormat-class], [mc_HOBODataFormat-class]
"mc_data_formats"

#' Default heights of sensors
#'
#' This table is used to set the default heights in metadata of sensors based on logger type.
#' The defaults were set based on the most common uses in community,
#' defaults can overwrite be user. see [mc_prep_meta_sensor] 
#'
#' data.frame with columns:
#' - logger_type
#' - sensor_name
#' - height - character representation of height
#' - suffix - suffix for sensor_name. If suffix is NA, then sensor_name is not modified.
#'
#' @seealso [myClim::mc_read_files()], [myClim::mc_read_data()]
"mc_data_heights"

#' Sensors definition.
#' 
#' R object of class environment with the definitions of (micro)climatic sensors.
#' see [mc_Sensor-class]. Similarly as in case of logger format definitions [mc_DataFormat-class] it is easy
#' to add new, unsupported sensor here. There is also universal sensor `real` where you can store any real values.
#'
#' Names of items are sensor_ids.
#' Currently supported sensors:
#' * count - result of `count` function [myClim::mc_agg()]
#' * coverage - result of `coverage` function [myClim::mc_agg()]
#' * DEND_T - temperature in TOMST dendrometer (°C)
#' * DEND_TOMSTdendro - change in stem size in TOMST dendrometer (raw units) [myClim::mc_calc_tomst_dendro()]
#' * dendro_l_um - change in stem size (μm) [myClim::mc_calc_tomst_dendro()]
#' * FDD - result of function [myClim::mc_calc_fdd()]
#' * GDD - result of function [myClim::mc_calc_gdd()]
#' * HOBO_RH - relative humidity in HOBO logger (%)
#' * HOBO_T_C - temperature in HOBO logger (°C)
#' * HOBO_T_F - temperature in HOBO logger (°F)
#' * integer - universal sensor with integer values
#' * logical - universal sensor with logical values
#' * moisture - volumetric soil moisture (ratio)
#' * precipitation (mm)
#' * real - universal sensor with real values
#' * RH_perc - Relative humidity sensor (%)
#' * snow_bool - result of function [myClim::mc_calc_snow()]
#' * snow_fresh - new snow (cm)
#' * snow_total - total hight snow (cm)
#' * sun_shine - time of sun shine (hours)
#' * T_C - universal temperature sensor (°C)
#' * TS_T - temperature sensor in TOMST ThermoDatalogger (°C)
#' * TMS_T1 - soil temperature sensor in TOMST TMS (°C)
#' * TMS_T2 - surface temperature sensor in TOMST TMS (°C)
#' * TMS_T3 - air temperature sensor in TOMST TMS (°C)
#' * TMS_TMSmoisture - soil moisture sensor in TOMST TMS (TDT signal)
#' * wind - speed of wind (m/s)
"mc_data_sensors"

#' Physical quantities definition
#' 
#' R object of class environment with the definitions of physical elements
#' for recording the microclimate e.g. temperature, speed, depth, volumetric water content...
#' see [mc_Physical-class]. Similarly as in case of logger format definitions [mc_DataFormat-class] it is easy
#' to add new, unsupported physical here.
#' @seealso [mc_Physical-class]
#'
#' Currently supported physical quantities:
#' * l_cm - length in cm
#' * l_mm - lenght in mm
#' * l_um - lenght in μm
#' * moisture - moisture in ratio 0-1
#' * RH_perc - Relative humidity in %
#' * T_C - Temperature in °C
#' * T_F - Temperature in °F
#' * t_h - time in hours
#' * TMSmoisture - TDT signal from TMS moisture sensor
#' * TOMSTdendro - radius difference in raw units
#' * v - speed in m/s
"mc_data_physical"

#' Volumetric water content parameters
#'
#' Data frame hosting the coefficients enable the conversion of TMS raw moisture data to
#' volumetric warer content. The coefficients come from laboratory calibration for several
#' soil types. For the best performance you should specify the soil type in case you know it
#' and in case it could be approximated to the available calibration e.g sand, loam, loamy sand....
#' See [myClim::mc_calc_vwc()]
#'
#' data.frame with columns:
#' - soiltype
#' - a
#' - b
#' - c
#' - rho
#' - clay
#' - silt
#' - sand
#' - ref
#'
"mc_data_vwc_parameters"
