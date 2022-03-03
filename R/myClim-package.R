#' @description
#'
#' Package myClim has two native data formats of myClim objects. Names of formats are Prep-format and Calc-format. 
#' Prep-format is designed for data preparation. Mainly data cleaning, metadata gathering, time zones handling 
#' and multiple downloading missions from single localities joining. Loaded data from source files by 
#' read functions [myClim::mc_read_files()] and [myClim::mc_read_data()] are in Prep-format.
#' Prep-format contains information about localities, loggers and sensors. 
#' Function [myClim::mc_agg()] converts data from Prep-format to Calc-format. Calc-format is designed mainly for calculations, 
#' analysis and virtual loggers derivations on the basis of cleaned, final microclimatic data. Calc-formt does not 
#' have the level of loggers and their metadata. In Calc-format sensors are organized directly in localities. 
#' 
#' **Prep-format**
#' 
#' Is the output of reading functions [myClim::mc_read_files()] and [myClim::mc_read_data()].
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
#' |           |             |              +-----+-------+-----+                |||
#' |           |             | $states      | tag | start | end |                |||
#' |           |             |              | ... | ...   | ... |                |||
#' |           |             |              +-----+-------+-----+                |||
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
#' All localities have same time step. Step is stored in metadata of Calc-format in minutes (`data$metadata@step`).
#' Some steps can not be represented by minutes. For example step `month` has variable number of minutes.
#' Therefore, metadata contains also text representation of the step (`data$metadata@step_text`).
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
#'             |            |              +-----+-------+-----+                ||
#'             |            | $states      | tag | start | end |                ||
#'             |            |              | ... | ...   | ... |                ||
#'             |            |              +-----+-------+-----+                ||
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
