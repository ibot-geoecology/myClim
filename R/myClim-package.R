if(getRversion() >= "2.15.1")  globalVariables(c(".", "mc_data_formats", "mc_data_heights", "mc_data_physical",
                                                        "mc_data_sensors", "mc_data_vwc_parameters", ".data"))

#' @description
#'
#' Package myClim was designed for handling the microclimatic data in R.
#' myClim workflow begin at the reading data primary from microclimatic dataloggers,
#' but can be also reading of meteorological station data from files or tables. 
#' Cleaning time step, time zone settings and metadata collecting is the next step of the work flow.
#' With myClim tools one can crop, join, downscale, and convert microclimatic 
#' data formats, sort them into localities, call descriptive statistics 
#' and compute microclimatic variables as e.g. Growing degree days or Vapor pressure deficit. 
#' Handy plotting functions are provided with smart defaults.   
#'
#' **myClim object** 
#' 
#' We implemented two slightly different data formats of myClim objects calling them: 
#' Raw-format and Agg-format.
#' Raw-format is designed for data preparation. Mainly data cleaning,
#' metadata gathering, time zones handling and multiple downloading missions 
#' from in time joining. Output myClim object from  
#' read functions [myClim::mc_read_files()] and [myClim::mc_read_data()] is a Raw-format.
#' Raw-format contains information about localities, loggers and sensors.
#' Function [myClim::mc_agg()] converts data from Raw-format to Agg-format.
#' Agg-format is designed mainly for calculations,analysis and microclimatic variables 
#' derivations on the basis of cleaned microclimatic data. 
#' Agg-format is missing the level of loggers. In Agg-format sensors are organized 
#' directly in localities, without loggers.
#' 
#' Microclimatic data are stored in myClim objects in regular R structure 
#' consists of classes and lists slightly resembling database scheme.  
#' The top level of the structure is the `locality`. It has own metadata e.g. 
#' altitude, latitude or user data  dedicated for any type of metadata user need. 
#' For detail description of locality metadata see [mc_LocalityMetadata]. 
#' On the locality in Raw-format there are `loggers` or in Agg-format directly `sensors`.
#' See below. Loggers represents the files imported with myClim reading functions.
#' Both `loggesr` and `sensors` have own metadata. One logger could host more sensors e.g. 
#' TOMST TMS logger hosing TMS_T1 soil temperature, TMS_T2 surface temperature, 
#' TMS_T3 air temperature , TMS_TMSmoisture soil moisture; or one logger could host 
#' single sensor.
#' For detailed description of logger and sensor metadata 
#' see [mc_LoggerMetadata], [mc_SensorMetadata], [mc_data_sensors]
#' 
#' 
#' 
#' **Raw-format**
#' 
#' Is the output of reading functions [myClim::mc_read_files()] and [myClim::mc_read_data()].
#' Raw-format is mainly designed for preparation. Especially for the situation when
#' user downloads the logger during more field visits in time. E. g. We have the logger 
#' on the locality measuring 10 years. We download data twice a year, thus we have 20 files 
#' from identical locality, but at the end we wan to work with single correctly joined 
#' time-series of the 10 years records. For this type of time joining 
#' it is useful to have a level "logger" in myClim object which allows to keep 
#' separate logger  downloads and prepare them for joining. 
#' 
#' Within the logger all sensors share time series. In Agg-format where level of
#' logger is missing, all sensors within the locality share time series. In Raw-format
#' the time series between the loggers or between the localities can be of different time 
#' step. E.g. on the locality there can be one logger measuring in time step 
#' 15 minutes and another one measuring once a day.  In Agg-format this is not allowed.
#' Therefore it is necessary to use [myClim::mc_agg()] to switch from Raw-format to the Agg-format.
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
#'             |           | @altitude                   |                                     |
#'             |           | @lat_wgs84                  |                                     |
#'             |           | @lon_wgs84                  |                                     |
#'             |           | @tz_offset                  |                                     |
#'             |           | @tz_type                    |                                     |
#'             |           | @user_data                  |                                     |
#'             |           +-----------------------------+                                     |
#'             |           +------------------------------------------------------------------+|
#'             | $loggers  | logger[1]                                                        ||
#'             |           +------------------------------------------------------------------+|
#'             |           |             +---------------------------+                        ||
#'             |           | $metadata   | mc_LoggerMetadata - class |                        ||
#'             |           |             +---------------------------+                        ||
#'             |           |             | @type                     |                        ||
#'             |           |             | @serial_number            |                        ||
#'             |           |             +---------------------------+                        ||
#'             |           |             +----------------------------+                       ||
#'             |           | $clean_info | mc_LoggerCleanInfo - class |                       ||
#'             |           |             +----------------------------+                       ||
#'             |           |             | @step                      |                       ||
#'             |           |             | @count_duplicits           |                       ||
#'             |           |             | @count_missed              |                       ||
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
#' **Agg-format**
#'
#' Is the output of [myClim::mc_agg()] function.  
#' Agg-format is mainly designed for calculations with clean, nicely
#' organized microclimatic time-series with unified time step, supplemented with necessary metadata. 
#' User can calculate various microclimatic variables on the locality using existing sensors 
#' e. g. calculate volumetric water content form TMS raw moisture records see [myClim::mc_calc_vwc]; 
#' calculate growing degree days or freezing degree days see [myClim::mc_calc_gdd], [myClim::mc_calc_fdd]; 
#' or estimate snow presence from near ground temperature [myClim::mc_calc_snow]. 
#' 
#' In Agg-format the level of logger is missing. This is in contrast with Raw-format.
#' In Agg-format sensors are organized directly in the localities.
#' Within one locality in Agg-format all sensors share time series (start, end, step).
#' All localities shares the same time step, but can have different start, end.
#' This is  practical for calculations, and it is achieved using [myClim::mc_agg()] function. 
#' E.g. we have 10 localities, each of them hosts several sensors. 
#' `locality` and `sensor` has own metadata see [mc_SensorMetadata], 
#' [mc_data_sensors], [mc_LocalityMetadata]. 
#' 
#' myClim time step is defined in seconds (`data$metadata@step`).
#' But some steps (especially irregular ones) may not be represented by seconds. 
#' For example step `month` has variable number of seconds within the year. Therefore, metadata contains also text
#' representation of the step (`data$metadata@period`).
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
#'             |            | @altitude                   |                      |
#'             |            | @lat_wgs84                  |                      |
#'             |            | @lon_wgs84                  |                      |
#'             |            | @tz_offset                  |                      |
#'             |            | @tz_type                    |                      |
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
#' @importFrom graphics abline axis axis.POSIXct box grconvertX grconvertY grid image layout legend lines mtext par
#' @importFrom methods callNextMethod is new slot<- slotNames
#' @importFrom stats aggregate end quantile sd start var
#' @importFrom utils installed.packages packageVersion read.table tail
#' @keywords internal
"_PACKAGE"

# The following block is used by usethis to automatically manage
# roxygen namespace tags. Modify with care!
## usethis namespace: start
## usethis namespace: end
NULL
