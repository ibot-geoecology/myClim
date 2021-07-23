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

#' Class for logger data
#' @export
setClass("model.LoggerMetadata",
         representation(
           serial_number = "character"
         ))

#' Class for sensor data
#' @export
setClass("model.SensorData",
         representation(
           sensor = "character",
           height = "numeric",
           calibrated = "logical",
           data = "data.frame"
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
           columns = "list"
         ),
         prototype(
           has_header = TRUE,
           separator = ";",
           date_column = NA_integer_,
           date_format = NA_character_,
           na_strings = NA_character_
         ))

#' Class for source file data format for TMS3 logger
#' @export
setClass("model.TMS3DataFormat", contains = "model.DataFormat")

setGeneric(
  "model.load_info_from_data",
  function(object, data){
    standardGeneric("model.load_info_from_data")
  }
)

setMethod(
    "model.load_info_from_data",
    signature("model.DataFormat"),
    function(object, data) {
        object
    }
)

setMethod(
    "model.load_info_from_data",
    signature("model.TMS3DataFormat"),
    function(object, data) {
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
)
