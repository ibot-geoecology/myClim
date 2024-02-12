#' Example data in Raw-format
#' @description
#' Raw data, not cleaned. Three example localities situated
#' in Saxon Switzerland National Park.
#' myClim object has metadata and covers time period from 2020-10 to 2021-02.
#'
#' Data includes time-series from 4 loggers:
#' * Tomst TMS4 with 4 sensors ("TMS_T1", "TMS_T2", "TMS_T3", "TMS_moist")
#' * Tomst Thermologger with 1 sensor ("Thermo_T)
#' * Tomst Point Dendrometer with 2 sensors ("Dendro_T", "Dendro_raw")
#' * HOBO U23 with 2 sensors ("HOBO_T", "HOBO_RH")
"mc_data_example_raw"

#' Example cleaned data in Raw-format.
#' @description
#' Cleaned data. Three example localities situated in Saxon Switzerland National Park.
#' myClim object has metadata and covers time period from 2020-10 to 2021-02.
#'
#' Data includes time-series from 4 loggers:
#' * Tomst TMS4 with 4 sensors ("TMS_T1", "TMS_T2", "TMS_T3", "TMS_moist")
#' * Tomst Thermologger with 1 sensor ("Thermo_T)
#' * Tomst Point Dendrometer with 2 sensors ("Dendro_T", "Dendro_raw")
#' * HOBO U23 with 2 sensors ("HOBO_T", "HOBO_RH")
"mc_data_example_clean"

#' Example data in Agg-format.
#' @description
#' Cleaned data in Agg-format. Three example localities situated in
#' Saxon Switzerland National Park.
#' myClim object has metadata and covers time period from 2020-10 to 2021-02.
#'
#' Data includes time-series from 4 loggers:
#' * Tomst TMS4 with 4 sensors ("TMS_T1", "TMS_T2", "TMS_T3", "TMS_moist")
#' * Tomst Thermologger with 1 sensor ("Thermo_T)
#' * Tomst Point Dendrometer with 2 sensors ("Dendro_T", "Dendro_raw")
#' * HOBO U23 with 2 sensors ("HOBO_T", "HOBO_RH")
"mc_data_example_agg"

#' Formats of source data files
#' 
#' @description
#' R object of class environment with the definitions how to
#' parse specific microclimatic logger files. In case you would like to add
#' new, unsupported logger, this is the place where the reading key is stored.
#'
#' @details
#' Package myClim support formats TOMST, TOMST_join and HOBO.
#' The environment object is stored in `./data/mc_data_formats.rda`.
#'
#' **TOMST**
#'
#' TOMST data format has defined structure. Expected name of data file is in format data_\<serial_number\>_\<x\>.csv.
#' Value serial_number can be automatically detected from file name. 
#' Datetime is in UTC and is stored in col 2. Temperature values are stored in col 3-5. Moisture ()
#' Supported logger types are TMS (for TMS-3/TMS-4), ThermoDataLogger (for Thermologger), Dendrometer and TMS_L45 (for TMS-4 Long 45cm).
#'
#' **TOMST_join**
#'
#' TOMST_join data format is used by output files from JoinTMS.exe software and from tupomanager.exe (TMS-1). 
#' Datetime in col 4, temperatures in col 5-7, moisture in col 8.
#'
#' **HOBO**
#'
#' HOBO data format is export format from software HOBOware of Onset company for HOBO U23 Pro v2 loggers (Temperature/RH). 
#' Format is very variable and can be adjusted by user in preferences of HOBOware. Strucuture of HOBO files format can be partly 
#' detected automatically from header of data.
#' Format of date-time (`date_format`) must be set manually in myClim reading functions ([mc_read_files()], [mc_read_data()]).
#' Date and time separated in more columns is not supported in myClim reading. If time zone is not defined in header of HOBO txt or csv file
#' and is not UTC, then `tz_offset` must be filled in while reading. UTF-8 encoding of HOBO file is required for reding to myClim.
#'
#' @seealso [myClim::mc_DataFormat], [mc_TOMSTDataFormat-class], [mc_TOMSTJoinDataFormat-class], [mc_HOBODataFormat-class]
"mc_data_formats"

#' Default heights of sensors
#'
#' This table is used to set the default heights in metadata of sensors based on logger type.
#' The defaults were set based on the most common uses,
#' defaults can be overwrite be user. see [mc_prep_meta_sensor]
#'
#' data.frame with columns:
#' - logger_type
#' - sensor_name
#' - height - character representation of height
#' - suffix - suffix for sensor_name. If suffix is NA, then sensor_name is not modified.
#'
#' Default heights are:
#' 
#' TOMST - Thermo
#' * Thermo_T = air 200 cm
#' 
#' TOMST - TMS
#' * TMS_T1 = soil 8 cm
#' * TMS_T2 = air 2 cm
#' * TMS_T3 = air 15 cm
#' * TMS_moist = soil 0-15 cm
#' 
#' TOMST - Dendro
#' * Dendro_T = 130 cm
#' * Dendro_raw = 130 cm
#' 
#' TOMST - TMS_L45
#' * TMS_T1 = soil 40 cm
#' * TMS_T2 = soil 30 cm
#' * TMS_T3 = air 15 cm
#' * TMS_moist = soil 30-44 cm
#' 
#' HOBO - HOBO_U23-001A
#' * HOBO_T = air 150 cm
#' * HOBO_RH = air 150 cm
#'
#' HOBO - HOBO_U23-004
#' * HOBO_T = air 2 cm
#' * HOBO_extT = soil 8 cm
#'
#' @seealso [myClim::mc_read_files()], [myClim::mc_read_data()]
"mc_data_heights"

#' Sensors definition.
#' 
#' R object of class environment with the definitions of (micro)climatic sensors.
#' see [mc_Sensor-class]. Similarly as in case of logger format definitions [mc_DataFormat-class] it is easy
#' to add new, sensor here. There is also universal sensor `real` where you can store any real values.
#'
#' Names of items are sensor_ids.
#' Currently supported sensors:
#' * count - result of `count` function [myClim::mc_agg()]
#' * coverage - result of `coverage` function [myClim::mc_agg()]
#' * Dendro_T - temperature in Tomst dendrometer (°C)
#' * Dendro_raw - change in stem size in Tomst dendrometer (raw units) [myClim::mc_calc_tomst_dendro()]
#' * dendro_l_um - change in stem size (um) [myClim::mc_calc_tomst_dendro()]
#' * FDD - result of function [myClim::mc_calc_fdd()]
#' * GDD - result of function [myClim::mc_calc_gdd()]
#' * HOBO_RH - relative humidity in HOBO U23-001A logger (%)
#' * HOBO_T - temperature in HOBO U23 logger (°C)
#' * HOBO_extT - external temperature in HOBO U23-004 logger (°C)
#' * integer - universal sensor with integer values
#' * logical - universal sensor with logical values
#' * VWC - volumetric water content in soil (m3/m3)
#' * precipitation - (mm)
#' * real - universal sensor with real values
#' * RH - relative humidity sensor (%)
#' * snow_bool - result of function [myClim::mc_calc_snow()]
#' * snow_fresh - fresh snow height (cm)
#' * snow_total - total snow height (cm)
#' * sun_shine - time of sun shine (hours)
#' * T_C - universal temperature sensor (°C)
#' * Thermo_T - temperature sensor in Tomst Thermologger (°C)
#' * TMS_T1 - soil temperature sensor in Tomst TMS (°C)
#' * TMS_T2 - surface temperature sensor in Tomst TMS (°C)
#' * TMS_T3 - air temperature sensor in Tomst TMS (°C)
#' * TMS_moist - soil moisture sensor in Tomst TMS (raw TMS units)
#' * wind - wind speed (m/s)
"mc_data_sensors"

#' Physical quantities definition
#' 
#' R object of class environment with the definitions of physical elements
#' for recording the microclimate e.g. temperature, speed, depth, volumetric water content...
#' see [mc_Physical-class]. Similarly as in case of logger format definitions [mc_DataFormat-class] it is easy
#' to add new, physical here.
#' @seealso [mc_Physical-class]
#'
#' Currently supported physical elements:
#' * l_cm - length in cm
#' * l_mm - length in mm
#' * l_um - length in um
#' * VWC - volumetric moisture in m3/m3
#' * RH - relative humidity in %
#' * T_C - temperature in °C
#' * t_h - time in hours
#' * moisture_raw - raw TMS moisture sensor values
#' * radius_raw - radius difference in raw units
#' * v - speed in m/s
"mc_data_physical"

#' Volumetric water content parameters
#'
#' Data frame hosting the coefficients for the conversion of TMS raw moisture units to
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
#' @references
#' Wild, J., Kopecky, M., Macek, M., Sanda, M., Jankovec, J., Haase, T., 2019. Climate at ecologically relevant scales:
#' A new temperature and soil moisture logger for long-term microclimate measurement. Agric. For. Meteorol. 268, 40-47.
#' https://doi.org/10.1016/j.agrformet.2018.12.018
#'
#' Kopecky, M., Macek, M., Wild, J., 2021. Topographic Wetness Index calculation guidelines based on measured soil
#' moisture and plant species composition. Sci. Total Environ. 757, 143785. https://doi.org/10.1016/j.scitotenv.2020.143785
"mc_data_vwc_parameters"
