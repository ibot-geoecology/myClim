#' Example data in prep-format. Data aren't cleaned.
"mc_data_example_source"

#' Example cleaned data in prep-format.
"mc_data_example_clean"

#' Example data in calc-format and 15 minutes step.
"mc_data_example_calc"

#' Formats of source data files
"mc_data_formats"

#' Sensors definition.
#'
#' Names of items are sensor_ids.
#'
#' Sensors:
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
"mc_data_physical"

#' Volumetric water content parameters
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
"mc_data_vwc_parameters"
