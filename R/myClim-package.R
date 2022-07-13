#' @description
#'
#' Package myClim was designed for handling the microclimatic data in R.
#' myClim workflow begin at the reading data primary from microclimatic dataloggers,
#' but can be also reading of meteorological station data from files. 
#' Cleaning time step, time zone settings and metadata collecting is the next step of the work flow.
#' With myClim tools one can crop, join, downscale, and convert microclimatic 
#' data formats, sort them into localities, request descriptive characteristics 
#' and compute microclimatic variables. Handy plotting functions are 
#' provided with smart defaults.   
#'
#' **myClim object** 
#' 
#' We implemented two slightly different data formats of myClim objects calling them: 
#' Prep-format and Calc-format. 
#' Prep-format is designed for data preparation. Mainly data cleaning, 
#' metadata gathering, time zones handling and multiple downloading missions 
#' from single localities joining. Output myClim object from  
#' read functions [myClim::mc_read_files()] and [myClim::mc_read_data()] is a Prep-format.
#' Prep-format contains information about localities, loggers and sensors. 
#' Function [myClim::mc_agg()] converts data from Prep-format to Calc-format. 
#' Calc-format is designed mainly for calculations,analysis and virtual 
#' loggers derivations on the basis of cleaned, final microclimatic data. 
#' Calc-formt has not the level of loggers and their metadata. 
#' In Calc-format sensors are organized directly in localities, without loggers. 
#' 
#' Microclimatic data are stored in myClim objects in regular R structure 
#' consists of classes and lists slightly resembling database scheme.  
#' The top level of the structure is the `locality`. It has on metadata e.g. 
#' altitude, latitude or user data  dedicated for any type of metadata user need. 
#' For detail desription of locality metadata see [mc_LocalityMetadata]. 
#' On the locality in Prep-format there are `loggers` or in Calc-format directly `sensors`. 
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
#' **Prep-format**
#' 
#' Is the output of reading functions [myClim::mc_read_files()] and [myClim::mc_read_data()].
#' Prep-format is mainly designed for preparation. Especially for the situation when 
#' user downloads the logger during more field visits in time. E. g. We have the logger 
#' on the locality measuring 10 years. We download data twice a year and at the end we  
#' wan to work with single correctly joined time series of the 10 years records. 
#' For this type of time joining it is useful to have a level "logger" in myClim 
#' object which allows to keep separate logger  downloads and prepare them for joining. 
#' 
#' Within the logger all sensors share time series. In Calc-format where level of 
#' logger is missing, all sensors within the locality sahre time series. In Prep-format 
#' the time series between the logger or between the localities can be of different time 
#' step. E.g. on the locality there can be one logger measuring in time step 
#' 15 minutes and another one measuring once a day.  In Calc-format this is not allowed. 
#' Therefore it is necessary to use [myClim::mc_agg()] to switch to the Calc-format. 
#' Later there will be join function implemented also switching from Prep-format to Calc-format.
#' 
#'Schema of myClim **Prep-format** 
#' 
#' \preformatted{
#' +-------------------------------------------------------------------------------+
#' | locality[1]                                                                   |
#' +-------------------------------------------------------------------------------+
#' |           +-----------------------------+                                     |
#' | $metadata | mc_LocalityMetadata - class |                                     |
#' |           +-----------------------------+                                     |
#' |           | @locality_id                |                                     |
#' |           | @altitude                   |                                     |
#' |           | @lat_wgs84                  |                                     |
#' |           | @lon_wgs84                  |                                     |
#' |           | @tz_offset                  |                                     |
#' |           | @tz_type                    |                                     |
#' |           | @user_data                  |                                     |
#' |           +-----------------------------+                                     |
#' |           +------------------------------------------------------------------+|
#' | $loggers  | logger[1]                                                        ||
#' |           +------------------------------------------------------------------+|
#' |           |             +---------------------------+                        ||
#' |           | $metadata   | mc_LoggerMetadata - class |                        ||
#' |           |             +---------------------------+                        ||
#' |           |             | @type                     |                        ||
#' |           |             | @serial_number            |                        ||
#' |           |             +---------------------------+                        ||
#' |           |             +----------------------------+                       ||
#' |           | $clean_info | mc_LoggerCleanInfo - class |                       ||
#' |           |             +----------------------------+                       ||
#' |           |             | @step                      |                       ||
#' |           |             | @count_duplicits           |                       ||
#' |           |             | @count_missed              |                       ||
#' |           |             | @count_disordered          |                       ||
#' |           |             | @rounded                   |                       ||
#' |           |             +----------------------------+                       ||
#' |           | $datetime   POSIXct vector                                       ||
#' |           |             +---------------------------------------------------+||
#' |           | $sensors    | sensor[1]                                         |||
#' |           |             +---------------------------------------------------+||
#' |           |             |              +---------------------------+        |||
#' |           |             | $metadata    | mc_SensorMetadata - class |        |||
#' |           |             |              +---------------------------+        |||
#' |           |             |              | @sensor_id                |        |||
#' |           |             |              | @name                     |        |||
#' |           |             |              | @height                   |        |||
#' |           |             |              | @calibrated               |        |||
#' |           |             |              +---------------------------+        |||
#' |           |             |              +----------+------------+-----------+|||
#' |           |             | $calibration | datetime | cor_factor | cor_slope ||||
#' |           |             |              | ...      | ...        | ...       ||||
#' |           |             |              +----------+------------+-----------+|||
#' |           |             | $values      numeric/logical vector               |||
#' |           |             |              +-----+-------+-----+-------+        |||
#' |           |             | $states      | tag | start | end | value |        |||
#' |           |             |              | ... | ...   | ... | ...   |        |||
#' |           |             |              +-----+-------+-----+-------+        |||
#' |           |             +---------------------------------------------------+||
#' |           |             +---------------------------------------------------+||
#' |           |             | sensor[2]                                         |||
#' |           |             +---------------------------------------------------+||
#' |           |             ...                                                  ||
#' |           |             +---------------------------------------------------+||
#' |           |             | sensor[n]                                         |||
#' |           |             +---------------------------------------------------+||
#' |           +------------------------------------------------------------------+|
#' |           +------------------------------------------------------------------+|
#' |           | logger[2]                                                        ||
#' |           +------------------------------------------------------------------+|
#' |           ...                                                                 |
#' |           +------------------------------------------------------------------+|
#' |           | logger[n]                                                        ||
#' |           +------------------------------------------------------------------+|
#' +-------------------------------------------------------------------------------+
#' +-------------------------------------------------------------------------------+
#' | locality[2]                                                                   |
#' +-------------------------------------------------------------------------------+
#' ...
#' +-------------------------------------------------------------------------------+
#' | locality[n]                                                                   |
#' +-------------------------------------------------------------------------------+}
#'
#' **Calc-format**
#'
#' Is the output of [myClim::mc_agg()] function.  
#' Calc-format is mainly designed for calculations with clean, nicely 
#' organized microclimatic data with unified time step, supplemented with necessary metadata. 
#' User can calculate various "virtual sensors" on the locality using existing ones 
#' e. g. calculate volumetric water content form TMS raw moisture records see [myClim::mc_calc_vwc]; 
#' calculate growing degree days or freezing degree days see [myClim::mc_calc_gdd], [myClim::mc_calc_fdd]; 
#' or estimate snow presence from near ground temperature [myClim::mc_calc_snow]. 
#' 
#' In Calc-format the level of logger is missing. This is in contrast with Prep-format. 
#' In Calc-format sensors are organized directly in the localities.
#' Within the locality of Calc-format all sensors share time series (start, end, step) 
#' all localities shares the same time step, can have different start, end.
#' This is  practical for calculations, and it is achieved using [myClim::mc_agg()] function. 
#' E.g. we have 10 localities, each of them hosts several sensors. 
#' The same as in Prep-format `locality` and `sensor` has own metadata 
#' see [mc_SensorMetadata], [mc_data_sensors], [mc_LocalityMetadata]. 
#' 
#' In contrast with Prep-format the Calc-formated myClim objects have additional 
#' metadata on the level of  whole object [mc_MainMetadata-class] holding information 
#' on time step which has whole object. Step is stored in minutes (`data$metadata@step`).
#' But some steps can not be represented by minutes. For example step `month` has variable 
#' number of minutes. Therefore, metadata contains also text 
#' representation of the step (`data$metadata@step_text`). Common time step 
#' of myClim Calc-format objects is practical when merging multiple object. See [myClim::mc_prep_merge]
#' 
#' 
#' Schema of myClim **Calc-format** 
#' 
#' \preformatted{
#'             +-------------------------+
#' $metadata   | mc_MainMetadata - class |
#'             +-------------------------+
#'             | @step                   |
#'             | @step_text              |
#'             +-------------------------+
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
#'             |            |              +----------+------------+-----------+||
#'             |            | $calibration | datetime | cor_factor | cor_slope |||
#'             |            |              | ...      | ...        | ...       |||
#'             |            |              +----------+------------+-----------+||
#'             |            | $values      numeric/logical vector               ||
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
#' @keywords internal
"_PACKAGE"

# The following block is used by usethis to automatically manage
# roxygen namespace tags. Modify with care!
## usethis namespace: start
## usethis namespace: end
NULL
