# constants ================================================================================

#' @export
mc_const_NONE_LOCALITY_ID <- "None"

# classes ================================================================================

#' Class for sensor definition
#' @slot name name of sensor (T1, T2, T3, TDT, RH, ...)
#' @slot logger name of logger (TMS, TMS-T1, TMS Dendro, iButton Hygrochron, iButton Thermochron, HOBO RH, HOBO T, ...)
#' @slot physical measurement (T, RH, VWC, ...)
#' @slot units measurument (°C, \%, m3/m3, raw, mm, ...)
#' @slot default_height default height of sensor in cm
#' @slot min_value minimal value
#' @slot max_value maximal value
#' @export mc_Sensor
#' @exportClass mc_Sensor
mc_Sensor <- setClass("mc_Sensor",
         slots = c(
           name = "character",
           logger = "character",
           physical = "character",
           units = "character",
           default_height = "numeric",
           min_value = "numeric",
           max_value = "numeric"
         ))

#' Class for locality metadata
#' @slot id name of loacality
#' @slot altitude of loacality
#' @slot lat_wgs84 latitude of locality in WGS-84
#' @slot lon_wgs84 longitude of locality in WGS-84
#' @slot user_data list for user data
#' @export mc_LocalityMetadata
#' @exportClass mc_LocalityMetadata
mc_LocalityMetadata <- setClass("mc_LocalityMetadata",
         representation(
            id = "character",
            altitude = "numeric",
            lat_wgs84 = "numeric",
            lon_wgs84 = "numeric",
            user_data = "list"
         ),
         prototype (
            altitude = NA_real_,
            lat_wgs84 = NA_real_,
            lon_wgs84 = NA_real_,
            user_data = list()
         ))

#' Class for logger metadata
#' @slot type of logger
#' @slot serial_number
#' @export mc_LoggerMetadata
#' @exportClass mc_LoggerMetadata
mc_LoggerMetadata <- setClass("mc_LoggerMetadata",
         representation(
           type = "character",
           serial_number = "character"
         ))

#' Class for sensor metadata
#' @export mc_SensorMetadata
#' @exportClass mc_SensorMetadata
mc_SensorMetadata <- setClass("mc_SensorMetadata",
         representation(
           sensor = "character",
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
           data_row_pattern = "character"
         ),
         prototype(
           has_header = TRUE,
           separator = ";",
           date_column = NA_integer_,
           date_format = NA_character_,
           na_strings = NA_character_,
           columns = list(),
           filename_serial_number_pattern = NA_character_
         ))

#' Class for source file data format for TMS logger
#' @export mc_TMSDataFormat
#' @exportClass mc_TMSDataFormat
mc_TMSDataFormat <- setClass("mc_TMSDataFormat", contains = "mc_DataFormat")

# generics ================================================================================

setGeneric(
  ".model.load_data_format_params_from_data",
  function(object, data){
    standardGeneric(".model.load_data_format_params_from_data")
  }
)

setGeneric(
  ".model.get_serial_number_from_filename",
  function(object, filename){
    standardGeneric(".model.get_serial_number_from_filename")
  }
)

setGeneric(
  ".model.is_file_in_right_format",
  function(object, filename){
    standardGeneric(".model.is_file_in_right_format")
  }
)

# methods ================================================================================

setMethod(
    ".model.load_data_format_params_from_data",
    "mc_DataFormat",
    function(object, data) {
        object
    }
)

setMethod(
    ".model.load_data_format_params_from_data",
    "mc_TMSDataFormat",
    function(object, data) {
        object@date_format <- .get_tms_datetime_format(data, object@date_column)
        object@columns <- .get_tms_columns(data)
        object
    }
)

.get_tms_datetime_format <- function(data, date_column){
    if(stringr::str_detect(data[1, date_column], "\\d{4}\\.\\d{2}\\.\\d{2} \\d{2}:\\d{2}"))
    {
        return("%Y.%m.%d %H:%M")
    }
    if(stringr::str_detect(data[1, date_column], "\\d{2}\\.\\d{2}\\.\\d{4} \\d{2}:\\d{2}"))
    {
        return("%d.%m.%Y %H:%M")
    }
    return(NA_character_)
}

.get_tms_columns <- function(data, T2_column){
    tms1_columns = list(T1 = 4)
    tms3_columns = list(T1 = 4, T2 = 5, T3 = 6, moisture = 7)
    if(data[1, tms3_columns$T2] == -200) {
        return(tms1_columns)
    }
    else {
        return(tms3_columns)
    }
    return(list())
}

setMethod(
    ".model.get_serial_number_from_filename",
    signature("mc_DataFormat"),
    function(object, filename) {
        if(is.null(object@filename_serial_number_pattern)) {
          stop(sprintf("It is not possible identify serial_number from file %s.", filename));
        }
        stringr::str_match(filename, object@filename_serial_number_pattern)[1, 2]
    }
)

setMethod(
    ".model.is_file_in_right_format",
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
