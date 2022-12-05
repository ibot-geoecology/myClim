#' Filter data from myClim object
#' @description 
#' This function filter data by localities and sensors.
#' 
#' @details 
#' In default settings it returns the object containing input sensors / localities.
#' It is possible to filter selected sensors from all localities, selected 
#' localities with all sensors or combine both. 
#' 
#' When `reverse = TRUE` then 
#' resulting myClim object contains all localities and sensors as input except
#' those you listed in parameters `localities, sensors`
#' 
#' @template param_myClim_object
#' @param localities locality_ids for filtering data; if NULL then do nothing
#' @param sensors sensor_ids for filtering data; if NULL then do nothing see `names(mc_data_sensors)`
#' @param reverse if TRUE then input localities and/or sensors are excluded (default FALSE)
#' @param stop_if_empty if TRUE then error for empty output (default TRUE)
#' @return filtered myClim object
#' @export
#' @examples 
#' mc_filter(mc_data_example_raw, localities=c("A6W79", "A2E32"), sensors=c("TMS_T1", "TMS_T2"))
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
