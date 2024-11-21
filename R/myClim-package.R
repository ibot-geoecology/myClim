if(getRversion() >= "2.15.1")  globalVariables(c(".data"))

#' @description
#'
#' Package myClim was designed for microclimate data processing, storing, and analyzing.
#' The myClim wofrkflow consists of reading logger files, pre-processing the time-series,
#' time-series aggregation, and microclimatic variables calculation. The microclimatic
#' data are stored in size-efficient hierarchical structure which respects the
#' hierarchy of field microclimate measurement (locality>loggers>sensors).
#'
#' After data import, myClim can summarize the data and automatically correct
#' for the most common problems.   The myClim package provides functions to
#' calculate aggregated microclimate statistics as well as methods for data
#' calibration, conversion and calculation of derived microclimatic variables
#' like growing degree days, freezing degree days, snow cover duration,
#' volumetric water content and vapor pressure deficit.
#'
#' Standardized microclimatic variables can be stored efficiently in myClim
#' data format or easily exported to standard R long or wide tables for further
#' analyses and visualization.
#'
#' **myClim object** 
#' 
#' We implemented two slightly different data formats of myClim objects calling them: 
#' Raw-format and Agg-format.
#' Raw-format is designed for data preparation. Mainly data cleaning,
#' metadata gathering, time zones handling and multiple downloads joining.
#' Outputs of functions [myClim::mc_read_files()] and [myClim::mc_read_data()] are a Raw-format.
#' Raw-format has the levels of: localities, loggers and sensors.
#' Function [myClim::mc_agg()] converts data from Raw-format to Agg-format.
#' Agg-format is designed mainly for calculations,analysis and microclimatic variables 
#' derivations on the basis of cleaned microclimatic data. 
#' Agg-format is missing the level of loggers. In Agg-format sensors are organized 
#' directly in localities, without loggers.
#' 
#' The highest hierarchical level of myClim structure is the `locality`. It has own metadata e.g.
#' coordinates and elevation. For detail description of locality metadata see [mc_LocalityMetadata].
#' On the locality in Raw-format there are `loggers`; in Agg-format the `sensors`.
#' See below. Loggers represents the files imported with myClim reading functions.
#' Both `loggesr` and `sensors` have own metadata. One logger could host more sensors e.g. 
#' Tomst TMS logger hosing TMS_T1 soil temperature, TMS_T2 surface temperature,
#' TMS_T3 air temperature , TMS_moist soil moisture.
#' For detailed description of logger and sensor metadata 
#' see [mc_LoggerMetadata], [mc_SensorMetadata], [mc_data_sensors]
#' 
#' In Raw-format Within the logger all sensors share time series. In Agg-format where level of
#' logger is missing, all sensors within the locality share time series. In Raw-format
#' the time series between the loggers or between the localities can be of different time 
#' step. E.g. on the locality there can be one logger measuring in time step 
#' 15 minutes and another one measuring once a day.  In Agg-format this is not allowed.
#' Therefore it is necessary to use [myClim::mc_agg()] to switch from Raw-format to the Agg-format.
#'
#' myClim time step is defined in seconds (`data$metadata@step`).
#' But some steps (especially irregular ones) may not be represented by seconds.
#' For example step `month` has variable number of seconds within the year. Therefore, metadata contains also text
#' representation of the step (`data$metadata@period`).
#'
#'Schema of myClim **Raw-format**
#' 
#' \preformatted{
#'             +-------------------------+
#' $metadata   | mc_MainMetadata - class |
#'             +-------------------------+
#'             | @version                |
#'             | @format_type            |
#'             +-------------------------+
#'             +-------------------------------------------------------------------------------+
#' $localities | locality[1]                                                                   |
#'             +-------------------------------------------------------------------------------+
#'             |           +-----------------------------+                                     |
#'             | $metadata | mc_LocalityMetadata - class |                                     |
#'             |           +-----------------------------+                                     |
#'             |           | @locality_id                |                                     |
#'             |           | @elevation                  |                                     |
#'             |           | @lat_wgs84                  |                                     |
#'             |           | @lon_wgs84                  |                                     |
#'             |           | @tz_offset                  |                                     |
#'             |           | @tz_type                    |                                     |
#'             |           | @join_serial                |                                     |
#'             |           | @user_data                  |                                     |
#'             |           +-----------------------------+                                     |
#'             |           +------------------------------------------------------------------+|
#'             | $loggers  | logger[1]                                                        ||
#'             |           +------------------------------------------------------------------+|
#'             |           |             +---------------------------+                        ||
#'             |           | $metadata   | mc_LoggerMetadata - class |                        ||
#'             |           |             +---------------------------+                        ||
#'             |           |             | @type                     |                        ||
#'             |           |             | @name                     |                        ||
#'             |           |             | @serial_number            |                        ||
#'             |           |             | @step                     |                        ||
#'             |           |             +---------------------------+                        ||
#'             |           |             +----------------------------+                       ||
#'             |           | $clean_info | mc_LoggerCleanInfo - class |                       ||
#'             |           |             +----------------------------+                       ||
#'             |           |             | @step                      |                       ||
#'             |           |             | @count_duplicities         |                       ||
#'             |           |             | @count_missing             |                       ||
#'             |           |             | @count_disordered          |                       ||
#'             |           |             | @rounded                   |                       ||
#'             |           |             +----------------------------+                       ||
#'             |           | $datetime   POSIXct vector                                       ||
#'             |           |             +---------------------------------------------------+||
#'             |           | $sensors    | sensor[1]                                         |||
#'             |           |             +---------------------------------------------------+||
#'             |           |             |              +---------------------------+        |||
#'             |           |             | $metadata    | mc_SensorMetadata - class |        |||
#'             |           |             |              +---------------------------+        |||
#'             |           |             |              | @sensor_id                |        |||
#'             |           |             |              | @name                     |        |||
#'             |           |             |              | @height                   |        |||
#'             |           |             |              | @calibrated               |        |||
#'             |           |             |              +---------------------------+        |||
#'             |           |             | $values      numeric/logical vector               |||
#'             |           |             |              +----------+------------+-----------+|||
#'             |           |             | $calibration | datetime | cor_factor | cor_slope ||||
#'             |           |             |              | ...      | ...        | ...       ||||
#'             |           |             |              +----------+------------+-----------+|||
#'             |           |             |              +-----+-------+-----+-------+        |||
#'             |           |             | $states      | tag | start | end | value |        |||
#'             |           |             |              | ... | ...   | ... | ...   |        |||
#'             |           |             |              +-----+-------+-----+-------+        |||
#'             |           |             +---------------------------------------------------+||
#'             |           |             +---------------------------------------------------+||
#'             |           |             | sensor[2]                                         |||
#'             |           |             +---------------------------------------------------+||
#'             |           |             ...                                                  ||
#'             |           |             +---------------------------------------------------+||
#'             |           |             | sensor[n]                                         |||
#'             |           |             +---------------------------------------------------+||
#'             |           +------------------------------------------------------------------+|
#'             |           +------------------------------------------------------------------+|
#'             |           | logger[2]                                                        ||
#'             |           +------------------------------------------------------------------+|
#'             |           ...                                                                 |
#'             |           +------------------------------------------------------------------+|
#'             |           | logger[n]                                                        ||
#'             |           +------------------------------------------------------------------+|
#'             +-------------------------------------------------------------------------------+
#'             +-------------------------------------------------------------------------------+
#'             | locality[2]                                                                   |
#'             +-------------------------------------------------------------------------------+
#'             ...
#'             +-------------------------------------------------------------------------------+
#'             | locality[n]                                                                   |
#'             +-------------------------------------------------------------------------------+}
#'
#'
#' Agg-format is the output of [myClim::mc_agg()] function.
#' Agg-format is mainly designed for calculations which are faster in Agg and slower in Raw.
#'
#'
#' Schema of myClim **Agg-format**
#' 
#' \preformatted{
#'             +----------------------------+
#' $metadata   | mc_MainMetadataAgg - class |
#'             +----------------------------+
#'             | @version                   |
#'             | @format_type               |
#'             | @step                      |
#'             | @period                    |
#'             | @intervals_start           |
#'             | @intervals_end             |
#'             +----------------------------+
#'             +-----------------------------------------------------------------+
#' $localities | locality[1]                                                     |
#'             +-----------------------------------------------------------------+
#'             |            +-----------------------------+                      |
#'             | $metadata  | mc_LocalityMetadata - class |                      |
#'             |            +-----------------------------+                      |
#'             |            | @locality_id                |                      |
#'             |            | @elevation                  |                      |
#'             |            | @lat_wgs84                  |                      |
#'             |            | @lon_wgs84                  |                      |
#'             |            | @tz_offset                  |                      |
#'             |            | @tz_type                    |                      |
#'             |            | @join_serial                |                      |
#'             |            | @user_data                  |                      |
#'             |            +-----------------------------+                      |
#'             | $datetime  POSIXct vector                                       |
#'             |            +---------------------------------------------------+|
#'             | $sensors   | sensor[1]                                         ||
#'             |            +---------------------------------------------------+|
#'             |            |              +---------------------------+        ||
#'             |            | $metadata    | mc_SensorMetadata - class |        ||
#'             |            |              +---------------------------+        ||
#'             |            |              | @sensor_id                |        ||
#'             |            |              | @name                     |        ||
#'             |            |              | @height                   |        ||
#'             |            |              | @calibrated               |        ||
#'             |            |              +---------------------------+        ||
#'             |            | $values      numeric/logical vector               ||
#'             |            |              +----------+------------+-----------+||
#'             |            | $calibration | datetime | cor_factor | cor_slope |||
#'             |            |              | ...      | ...        | ...       |||
#'             |            |              +----------+------------+-----------+||
#'             |            |              +-----+-------+-----+-------+        ||
#'             |            | $states      | tag | start | end | value |        ||
#'             |            |              | ... | ...   | ... | ...   |        ||
#'             |            |              +-----+-------+-----+-------+        ||
#'             |            +---------------------------------------------------+|
#'             |            +---------------------------------------------------+|
#'             |            | sensor[2]                                         ||
#'             |            +---------------------------------------------------+|
#'             |            ...                                                  |
#'             |            +---------------------------------------------------+|
#'             |            | sensor[n]                                         ||
#'             |            +---------------------------------------------------+|
#'             +-----------------------------------------------------------------+
#'             +-----------------------------------------------------------------+
#'             | locality[2]                                                     |
#'             +-----------------------------------------------------------------+
#'             ...
#'             +-----------------------------------------------------------------+
#'             | locality[n]                                                     |
#'             +-----------------------------------------------------------------+}
#'
#' @importFrom grDevices dev.off gray hcl.colors pdf png
#' @importFrom graphics abline axis axis.POSIXct box grconvertX grconvertY grid image layout legend lines mtext par plot
#' @importFrom methods callNextMethod is new slot<- slotNames as slot
#' @importFrom stats aggregate end quantile sd start var
#' @importFrom utils packageVersion read.table tail head
#' @keywords internal
"_PACKAGE"

# The following block is used by usethis to automatically manage
# roxygen namespace tags. Modify with care!
## usethis namespace: start
## usethis namespace: end
NULL
