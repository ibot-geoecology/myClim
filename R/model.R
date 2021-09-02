# constants ================================================================================

#' @export
model.NONE_LOCALITY_ID <- "None"

# classes ================================================================================

#' Class for sensor definition
#' @slot name name of sensor (T1, T2, T3, TDT, RH, ...)
#' @slot logger name of logger (TMS, TMS-T1, TMS Dendro, iButton Hygrochron, iButton Thermochron, HOBO RH, HOBO T, ...)
#' @slot physical measurement (T, RH, VWC, ...)
#' @slot units measurument (°C, \%, m3/m3, raw, mm, ...)
#' @slot default_height default height of sensor in cm
#' @slot min_value minimal value
#' @slot max_value maximal value
#' @export model.Sensor
#' @exportClass model.Sensor
model.Sensor <- setClass("model.Sensor",
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
#' @export model.LocalityMetadata
#' @exportClass model.LocalityMetadata
model.LocalityMetadata <- setClass("model.LocalityMetadata",
         representation(
            id = "character",
            altitude = "numeric",
            lat_wgs84 = "numeric",
            lon_wgs84 = "numeric"
         ),
         prototype (
            altitude = NA_real_,
            lat_wgs84 = NA_real_,
            lon_wgs84 = NA_real_
         ))

#' Class for logger metadata
#' @export model.LoggerMetadata
#' @exportClass model.LoggerMetadata
model.LoggerMetadata <- setClass("model.LoggerMetadata",
         representation(
           type = "character",
           serial_number = "character"
         ))

#' Class for sensor data
#' @export model.SensorData
#' @exportClass model.SensorData
model.SensorData <- setClass("model.SensorData",
         representation(
           sensor = "character",
           height = "numeric",
           calibrated = "logical",
           values = "numeric"
         ),
         prototype (
             height = NA_integer_,
             calibrated = FALSE
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
#' @export model.DataFormat
#' @exportClass model.DataFormat
model.DataFormat <- setClass("model.DataFormat",
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
#' @export model.TMSDataFormat
#' @exportClass model.TMSDataFormat
model.TMSDataFormat <- setClass("model.TMSDataFormat", contains = "model.DataFormat")

# generics ================================================================================

#' @export
setGeneric(
  "model.load_info_from_data",
  function(object, data){
    standardGeneric("model.load_info_from_data")
  }
)

#' @export
setGeneric(
  "model.get_serial_number_from_filename",
  function(object, filename){
    standardGeneric("model.get_serial_number_from_filename")
  }
)

#' @export
setGeneric(
  "model.is_file_in_right_format",
  function(object, filename){
    standardGeneric("model.is_file_in_right_format")
  }
)

# methods ================================================================================

#' @export
setMethod(
    "model.load_info_from_data",
    "model.DataFormat",
    function(object, data) {
        object
    }
)

#' @export
setMethod(
    "model.load_info_from_data",
    "model.TMSDataFormat",
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

#' @export
setMethod(
    "model.get_serial_number_from_filename",
    signature("model.DataFormat"),
    function(object, filename) {
        if(is.null(object@filename_serial_number_pattern)) {
          stop(sprintf("It is not possible identify serial_number from file %s.", filename));
        }
        stringr::str_match(filename, object@filename_serial_number_pattern)[1, 2]
    }
)

#' @export
setMethod(
    "model.is_file_in_right_format",
    signature("model.DataFormat"),
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
