#' Filter data from myClim object
#' @description 
#' This function filter data by localities and sensors.
#' 
#' @details 
#' In default settings it returns the object containing input sensors / localities.
#' When you provide vector of localities e.g. `localities=c("A6W79", "A2E32")` 
#' selected localities are filtered with all sensors on those localities.
#' The same as When you provide vector of sensors `sensors=c("TMS_T1", "TMS_T2")`, 
#' selected sensors are filtered through all localities.
#' When you combine localities and sensors, then filtering return 
#' selected sensors on selected localities. 
#' 
#' When `reverse = TRUE` and using only localities parameter then 
#' the listed localities are removed. Similarly `reverse = TRUE` with proving only 
#' sensors parameter, then the listed sensors are removed from all localities. When 
#' using `reverse = TRUE` and combining sensors and localities parameters then
#' selected sensors are removed from selected localities.   
#' 
#' 
#' @template param_myClim_object
#' @param localities locality_ids for filtering data; if NULL then do nothing
#' @param sensors sensor_ids for filtering data; if NULL then do nothing see `names(mc_data_sensors)`
#' @param reverse if TRUE then input localities and/or sensors are excluded (default FALSE)
#' @param stop_if_empty if TRUE then error for empty output (default TRUE)
#' @return filtered myClim object
#' @export
#' @examples 
#' ## keep only "A6W79", "A2E32" localities with all their sensors
#' mc_filter(mc_data_example_raw, localities=c("A6W79", "A2E32"))
#' 
#' ## remove "A6W79", "A2E32" localities and keep all others
#' mc_filter(mc_data_example_raw, localities=c("A6W79", "A2E32"), reverse=T)
#' 
#' ## keep only "TMS_T1", and "TMS_T2" sensors on all localities
#' mc_filter(mc_data_example_raw, sensors=c("TMS_T1", "TMS_T2"))
#' 
#' ## remove "TMS_T1", and "TMS_T2" sensors from all localities
#' mc_filter(mc_data_example_raw, sensors=c("TMS_T1", "TMS_T2"),reverse=T)
#' 
#' ## keep only "TMS_T1", and "TMS_T2" sensors on "A6W79", "A2E32" localities
#' mc_filter(mc_data_example_raw, localities=c("A6W79", "A2E32"), sensors=c("TMS_T1", "TMS_T2"))
#' 
#' ## remove "TMS_T1", and "TMS_T2" sensors from "A6W79", "A2E32" localities and keep all other sensors and localities
#' mc_filter(mc_data_example_raw, localities=c("A6W79", "A2E32"), sensors=c("TMS_T1", "TMS_T2"),reverse=T)
mc_filter <- function(data, localities=NULL, sensors=NULL, reverse=FALSE, stop_if_empty=TRUE) {
    is_agg_format <- .common_is_agg_format(data)
    if(!is.null(localities)) {
        filter_function <- if(reverse) purrr::discard else purrr::keep
        localities <- filter_function(data$localities, function(.x) .x$metadata@locality_id %in% localities)
        data$localities <- localities
    }
    if(!is.null(sensors)) {
        if(is_agg_format) {
            data <- .filter_agg_sensors(data, sensors, reverse)
        } else {
            data <- .filter_raw_sensors(data, sensors, reverse)
        }
    }
    if(stop_if_empty && length(data$localities) == 0) {
        stop("All data are removed by filter.")
    }
    data
}

.filter_raw_sensors <- function(data, sensors, reverse) {
    logger_function <- function (logger) {
        filter_function <- if(reverse) purrr::discard else purrr::keep
        logger$sensors <- filter_function(logger$sensors, function(.x) .x$metadata@name %in% sensors)
        logger
    }

    locality_function <- function (locality) {
        locality$loggers <- purrr::map(locality$loggers, logger_function)
        locality$loggers <- purrr::keep(locality$loggers, function(.x) length(.x$sensors) > 0)
        locality
    }
    data$localities <- purrr::map(data$localities, locality_function)
    data$localities <- purrr::keep(data$localities, function(.x) length(.x$loggers) > 0)
    return(data)
}

.filter_agg_sensors <- function(data, sensors, reverse) {
    locality_function <- function (locality) {
        filter_function <- if(reverse) purrr::discard else purrr::keep
        locality$sensors <- filter_function(locality$sensors, function(.x) .x$metadata@name %in% sensors)
        locality
    }
    data$localities <- purrr::map(data$localities, locality_function)
    data$localities <- purrr::keep(data$localities, function(.x) length(.x$sensors) > 0)
    data
}
