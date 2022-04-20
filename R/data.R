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
#' TOMST
#'
#' TOMST data format has stable structure. Datetime is in UTC. Name of data file is in format data_<serial_number>_<x>.csv.
#' Value serial_number can be automatically detected from file name. Supported loggers are TMS and ThermoDataLogger.
#'
#' TOMST_join
#'
#' TOMST_join data format is custome format for internal using of Institute of Botany of the Czech Academy of Sciences.
#'
#' HOBO
#'
#' HOBO data format is export format from software HOBOware of Onset company. Format is very variable
#' and can be changed in preferences form. Strucuture of format can be partly detected automatically from header of data.
#' It is required manually define format of date-time (`date_format`) in reading functions ([mc_read_files()], [mc_read_data()]).
#' Separated date and time is not supported. If time zone is not defined in header and is other than UTC, then `tz_offset`
#' must be filled in.
#'
#' @seealso [myClim::mc_DataFormat], [mc_TOMSTDataFormat-class], [mc_TOMSTJoinDataFormat-class], [mc_HOBODataFormat-class]
"mc_data_formats"

#' Sensors definition.
#' 
#' R object of class environment with the definitions of (micro)climatic sensors.
#' see [mc_Sensor-class]. Similarly as in case of logger format definitions [mc_DataFormat-class] it is easy
#' to add new, unsupported sensor here. There is also universal sensor `real` where you can store any real values.
#'
#' Names of items are sensor_ids.
#' Currently supported sensors:
#' * count - result of `count` function [myClim::mc_agg()]
#' * coverage - result of `count` function [myClim::mc_agg()]
#' * FDD - result of function [myClim::mc_calc_fdd()]
#' * GDD - result of function [myClim::mc_calc_gdd()]
#' * integer - universal sensor with integer values
#' * logical - universal sensor with logical values
#' * moisture - volumetric soil moisture (ratio)
#' * precipitation in (mm)
#' * real - universal sensor with real values
#' * RH_perc - Relative humidity sensor (%)
#' * snow_bool - result of function [myClim::mc_calc_snow()]
#' * snow_fresh - new snow in (cm)
#' * snow_total - total hight snow (cm)
#' * sun_shine - time of sun shine (hours)
#' * T_C - universal temperature sensor (°C)
#' * TM_T - temperature sensor in TOMST ThermoDatalogger (°C)
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
#' * T_C - Temperature in °C
#' * moisture - moisture in ratio 0-1
#' * TMSmoisture - TDT signal from TMS moisture sensor
#' * RH_perc - Relative humidity in %
#' * l_cm - length in cm
#' * l_mm - lenght in mm
#' * v - speed in m/s
#' * t_h - time in hours
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
