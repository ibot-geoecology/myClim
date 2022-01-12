# constants ================================================================================

#' @export
mc_const_TZ_UTC <- "UTC"
#' @export
mc_const_TZ_SOLAR <- "solar"
#' @export
mc_const_TZ_USER_DEFINED <- "user defined"

.model_const_COUNT_TEST_VALUES <- 100

# classes ================================================================================

#' Class for sensor definition
#' @slot sensor_id of sensor (TMS_T1, TMS_T2, TMS_T3, TMS_moisture, ...)
#' @slot logger name of logger (TMS, ...)
#' @slot physical measurement (T, TMS_moisture, ...)
#' @slot default_height default height of sensor in m
#' @slot min_value minimal value
#' @slot max_value maximal value
#' @export mc_Sensor
#' @exportClass mc_Sensor
mc_Sensor <- setClass("mc_Sensor",
         representation (
           sensor_id = "character",
           logger = "character",
           physical = "character",
           default_height = "numeric",
           min_value = "numeric",
           max_value = "numeric",
           plot_color = "character",
           plot_line_width = "numeric"
         ),
         prototype (
            plot_color = "",
            plot_line_width = 1
         ))

#' Class for physical
#' @slot name of physical
#' @slot description
#' @slot units measurument (°C, \%, m3/m3, raw, mm, ...)
#' @export mc_Physical
#' @exportClass mc_Physical
mc_Physical <- setClass("mc_Physical",
         representation(
           name = "character",
           description = "character",
           units = "character"
         ))

#' Class for main metadata in data format for calculation
#' @slot step of data
#' @export mc_MainMetadata
#' @exportClass mc_MainMetadata
mc_MainMetadata <- setClass("mc_MainMetadata",
                            representation(
                                    step = "numeric"
                                ),
                                prototype (
                                ))

#' Class for locality metadata
#' @slot id name of loacality
#' @slot altitude of loacality
#' @slot lat_wgs84 latitude of locality in WGS-84
#' @slot lon_wgs84 longitude of locality in WGS-84
#' @slot tz_offset offset from UTC in minutes
#' @slot tz_type type of time zone
#' @slot user_data list for user data
#' @export mc_LocalityMetadata
#' @exportClass mc_LocalityMetadata
mc_LocalityMetadata <- setClass("mc_LocalityMetadata",
         representation(
            locality_id = "character",
            altitude = "numeric",
            lat_wgs84 = "numeric",
            lon_wgs84 = "numeric",
            tz_offset = "numeric",
            tz_type = "character",
            user_data = "list"
         ),
         prototype (
            altitude = NA_real_,
            lat_wgs84 = NA_real_,
            lon_wgs84 = NA_real_,
            tz_offset = NA_integer_,
            tz_type = mc_const_TZ_UTC,
            user_data = list()
         ))

#' Class for logger metadata
#' @slot type of logger (TMS, ThermoDatalogger)
#' @slot serial_number
#' @export mc_LoggerMetadata
#' @exportClass mc_LoggerMetadata
mc_LoggerMetadata <- setClass("mc_LoggerMetadata",
                              representation(
                                type = "character",
                                serial_number = "character"),
)

#' Class for logger clean info
#' @slot step of series in minutes
#' @slot count_duplicits
#' @slot count_missed
#' @slot count_disordered
#' @export mc_LoggerCleanInfo
#' @exportClass mc_LoggerCleanInfo
mc_LoggerCleanInfo <- setClass("mc_LoggerCleanInfo",
                              representation(
                                  step = "numeric",
                                  count_duplicits = "numeric",
                                  count_missed = "numeric",
                                  count_disordered = "numeric"),
                              prototype(
                                  step = NA_integer_,
                                  count_duplicits = NA_integer_,
                                  count_missed = NA_integer_,
                                  count_disordered = NA_integer_),
)

#' Class for sensor metadata
#' @export mc_SensorMetadata
#' @exportClass mc_SensorMetadata
mc_SensorMetadata <- setClass("mc_SensorMetadata",
         representation(
           sensor_id = "character",
           name = "character",
           height = "numeric",
           calibrated = "logical"
         ),
         prototype (
             height = NA_integer_,
             calibrated = FALSE
         ))

#' Class for state of sensor
#' @slot tag
#' @slot start
#' @slot end
#' @slot tag
#' @export mc_SensorState
#' @exportClass mc_SensorState
mc_SensorState <- setClass("mc_SensorState",
         representation(
           tag = "character",
           start = "POSIXct",
           end = "POSIXct"
         ))

#' Class for source file data format
#' @slot has_header columns separator
#' @slot separator columns separator
#' @slot date_column index of date column
#' @slot date_format format of date
#' @slot na_strings strings for NA values
#' @slot columns list with names and indexes of value columns
#' @slot filename_serial_number_pattern character pattern for detecting serial_number from filename
#' @slot data_row_pattern character pattern for detecting right file format
#' @export mc_DataFormat
#' @exportClass mc_DataFormat
mc_DataFormat <- setClass("mc_DataFormat",
         representation(
           has_header = "logical",
           separator = "character",
           date_column = "numeric",
           date_format = "character",
           na_strings = "character",
           columns = "list",
           filename_serial_number_pattern = "character",
           data_row_pattern = "character",
           logger_type = "character"
         ),
         prototype(
           has_header = TRUE,
           separator = ";",
           date_column = NA_integer_,
           date_format = NA_character_,
           na_strings = NA_character_,
           columns = list(),
           filename_serial_number_pattern = NA_character_,
           logger_type = NA_character_
         ))

#' Class for source file data format for TOMST logger
#' @export mc_TOMSTDataFormat
#' @exportClass mc_TOMSTDataFormat
mc_TOMSTDataFormat <- setClass("mc_TOMSTDataFormat", contains = "mc_DataFormat")

#' Class for source file data format for joined TOMST logger
#' @export mc_TOMSTJoinDataFormat
#' @exportClass mc_TOMSTJoinDataFormat
mc_TOMSTJoinDataFormat <- setClass("mc_TOMSTJoinDataFormat", contains = "mc_DataFormat")

# generics ================================================================================

setGeneric(
  ".model_load_data_format_params_from_data",
  function(object, data){
    standardGeneric(".model_load_data_format_params_from_data")
  }
)

setGeneric(
  ".model_get_serial_number_from_filename",
  function(object, filename){
    standardGeneric(".model_get_serial_number_from_filename")
  }
)

setGeneric(
  ".model_is_file_in_right_format",
  function(object, filename){
    standardGeneric(".model_is_file_in_right_format")
  }
)

# methods ================================================================================

setMethod(
    ".model_load_data_format_params_from_data",
    "mc_DataFormat",
    function(object, data) {
        object
    }
)

setMethod(
    ".model_load_data_format_params_from_data",
    "mc_TOMSTDataFormat",
    function(object, data) {
        object@date_format <- .get_tomst_datetime_format(data, object@date_column)
        .change_tomst_columns_and_logger_type(object, data)
    }
)

.get_tomst_datetime_format <- function(data, date_column){
    if(stringr::str_detect(data[1, date_column], "\\d{4}\\.\\d{1,2}\\.\\d{1,2} \\d{2}:\\d{2}"))
    {
        return("%Y.%m.%d %H:%M")
    }
    if(stringr::str_detect(data[1, date_column], "\\d{1,2}\\.\\d{1,2}\\.\\d{4} \\d{2}:\\d{2}"))
    {
        return("%d.%m.%Y %H:%M")
    }
    return(NA_character_)
}

.change_tomst_columns_and_logger_type <- function(object, data){
    tm_columns = list(TM_T = 4)
    tms_columns = list(TMS_T1 = 4, TMS_T2 = 5, TMS_T3 = 6, TMS_TMSmoisture = 7)
    data <- head(data, .model_const_COUNT_TEST_VALUES)
    if(all(is.na(data[[tms_columns$TMS_T2]]))) {
        object@columns <- tm_columns
        object@logger_type <- "ThermoDatalogger"
    }
    else {
        object@columns <- tms_columns
        object@logger_type <- "TMS"
    }
    object
}

setMethod(
    ".model_load_data_format_params_from_data",
    "mc_TOMSTJoinDataFormat",
    function(object, data) {
        .change_tomst_join_columns_and_logger_type(object, data)
    }
)

.change_tomst_join_columns_and_logger_type <- function(object, data){
    tmj_columns = list(TM_T = 5)
    tmsj_columns = list(TMS_T1 = 5, TMS_T2 = 6, TMS_T3 = 7, TMS_TMSmoisture = 8, TMS_moisture = 9)
    data <- head(data, .model_const_COUNT_TEST_VALUES)
    if((all(is.na(data[[tmsj_columns$TMS_T2]])) && all(is.na(data[[tmsj_columns$TMS_T3]]))) ||
       (all(data[[tmsj_columns$TMS_T1]] == data[[tmsj_columns$TMS_T2]]) &&
           all(data[[tmsj_columns$TMS_T1]] == data[[tmsj_columns$TMS_T3]]))) {
        object@columns <- tmj_columns
        object@logger_type <- "ThermoDatalogger"
        return(object)
    }
    object@logger_type <- "TMS"
    moisture = data[[tmsj_columns$TMS_moisture]]
    if(all(moisture == 0)) {
        object@columns <- within(tmsj_columns, rm(TMS_moisture))
        return(object)
    }
    object@columns <- tmsj_columns
    object
}

setMethod(
    ".model_get_serial_number_from_filename",
    signature("mc_DataFormat"),
    function(object, filename) {
        if(is.null(object@filename_serial_number_pattern)) {
          stop(stringr::str_glue("It is not possible identify serial_number from file {filename}."))
        }
        stringr::str_match(basename(filename), object@filename_serial_number_pattern)[1, 2]
    }
)

setMethod(
    ".model_is_file_in_right_format",
    signature("mc_DataFormat"),
    function(object, filename) {
        con = file(filename, "r")
        skip <- object@has_header
        while (TRUE) {
            line = readLines(con, n = 1)
            if ( length(line) == 0 ) {
                close(con)
                return(FALSE)
            }
            if(skip) {
              skip <- FALSE
              next
            }
            close(con)
            return(stringr::str_detect(line, object@data_row_pattern))
        }
    }
)
