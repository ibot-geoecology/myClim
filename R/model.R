#' Class for sensor definition
#' @slot name of sensor (T1, T2, T3, TDT, RH, ...)
#' @slot logger name of logger (TMS3, TMS4 long, TMS-T1, TMS Dendro, iButton Hygrochron, iButton Thermochron, HOBO RH, HOBO T, ...)
#' @slot physical measurement (T, RH, VWC, ...)
#' @slot units measurument (°C, %, m3/m3, raw, mm, ...)
#' @slot defaultHeight default height of sensor in cm
#' @slot minValue minimal value
#' @slot maxValue maximal value

setClass("Sensor",
         slots = c(
           name = "character",
           logger = "character",
           phycsical = "character",
           units = "character",
           defaultHeight = "numeric",
           minValue = "numeric",
           maxValue = "numeric"
         ))
