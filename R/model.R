
# classes ================================================================================

#' Class for sensor definition
#' @slot name name of sensor (T1, T2, T3, TDT, RH, ...)
#' @slot logger name of logger (TMS3, TMS4 long, TMS-T1, TMS Dendro, iButton Hygrochron, iButton Thermochron, HOBO RH, HOBO T, ...)
#' @slot physical measurement (T, RH, VWC, ...)
#' @slot units measurument (°C, %, m3/m3, raw, mm, ...)
#' @slot default_height default height of sensor in cm
#' @slot min_value minimal value
#' @slot max_value maximal value
#' @export
setClass("model.Sensor",
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
#' @export
setClass("model.LocalityMetadata",
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
#' @export
setClass("model.LoggerMetadata",
         representation(
           type = "character",
           serial_number = "character"
         ))

#' Class for sensor data
#' @export
setClass("model.SensorData",
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
#' @export
setClass("model.DataFormat",
         representation(
           has_header = "logical",
           separator = "character",
           date_column = "numeric",
           date_format = "character",
           na_strings = "character",
           columns = "list",
           filename_serial_number_pattern = "character"
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

#' Class for source file data format for TMS3 logger
#' @export
setClass("model.TMSDataFormat", contains = "model.DataFormat")

# generics ================================================================================

setGeneric(
  "model.load_info_from_data",
  function(object, data){
    standardGeneric("model.load_info_from_data")
  }
)

setGeneric(
  "model.get_serial_number_from_filename",
  function(object, filename){
    standardGeneric("model.get_serial_number_from_filename")
  }
)

# methods ================================================================================

setMethod(
    "model.load_info_from_data",
    signature("model.DataFormat"),
    function(object, data) {
        object
    }
)

setMethod(
    "model.load_info_from_data",
    signature("model.TMSDataFormat"),
    function(object, data) {
        object <- .change_tms_datetime_format(object, data)
        object <- .change_tms_columns(object, data)
        object
    }
)

.change_tms_datetime_format <- function(object, data){
    if(grepl("\\d{4}\\.\\d{2}\\.\\d{2} \\d{2}:\\d{2}", data[1, object@date_column], perl = TRUE))
    {
        object@date_format <- "%Y.%m.%d %H:%M"
    }
    else if(grepl("\\d{2}\\.\\d{2}\\.\\d{4} \\d{2}:\\d{2}", data[1, object@date_column], perl = TRUE))
    {
        object@date_format <- "%d.%m.%Y %H:%M"
    }
    object
}

.change_tms_columns <- function(object, data){
    tms1_columns = list(T1 = 4)
    tms3_columns = list(T1 = 4, T2 = 5, T3 = 6, moisture = 7)
    if(data[1, tms3_columns$T2] == -200)
    {
        object@columns <- tms1_columns
    }
    else {
        object@columns <- tms3_columns
    }
    object
}

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
