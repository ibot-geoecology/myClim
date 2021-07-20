#' Class for sensor definition
#' @slot name of sensor (T1, T2, T3, TDT, RH, ...)
#' @slot logger name of logger (TMS3, TMS4 long, TMS-T1, TMS Dendro, iButton Hygrochron, iButton Thermochron, HOBO RH, HOBO T, ...)
#' @slot physical measurement (T, RH, VWC, ...)
#' @slot units measurument (°C, %, m3/m3, raw, mm, ...)
#' @slot defaultHeight default height of sensor in cm
#' @slot minValue minimal value
#' @slot maxValue maximal value
#' @export
setClass("Sensor",
         slots = c(
           name = "character",
           logger = "character",
           physical = "character",
           units = "character",
           defaultHeight = "numeric",
           minValue = "numeric",
           maxValue = "numeric"
         ))

#' Class for source file data format
#' @slot has_header columns separator
#' @slot separator columns separator
#' @slot date_column index of date column
#' @slot date_format format of date
#' @export
setClass("DataFormat",
         slots = c(
           header = "logical",
           separator = "character",
           date_column = "numeric",
           date_format = "character"
         ))
