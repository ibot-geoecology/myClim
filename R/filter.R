#' Filter data
#'
#' This function filter data by localities and sensors
#'
#' @param data myClim object in Prep-format or Calc-formt see [myClim-package]
#' @param localities locality_ids for filtering data; if NULL then do nothing
#' @param sensors sensor_ids for filtering data; if NULL then do nothing
#' @param reverse - if TRUE then filtered discard else keeped (default FALSE)
#' @param stop_if_empty - if TRUE then error for empty output (default TRUE)
#' @return filtered data in same format as input
#' @export
#' @examples
#' \donotrun{example_tomst_data1 <- mc_filter(example_tomst_data1, localities=c("A6W79", "A2E32"), sensors=c("TMS_T1", "TMS_T2"))}
mc_filter <- function(data, localities=NULL, sensors=NULL, reverse=FALSE, stop_if_empty=TRUE) {
    is_calc_format <- myClim:::.common_is_calc_format(data)
    if(!is.null(localities)) {
        filter_function <- if(reverse) purrr::discard else purrr::keep
        localities <- filter_function(myClim:::.common_get_localities(data), function(.x) .x$metadata@locality_id %in% localities)
        if(is_calc_format) {
            data$localities <- localities
        } else {
            data <- localities
        }
    }
    if(!is.null(sensors)) {
        if(is_calc_format) {
            data <- .filter_calc_sensors(data, sensors, reverse)
        } else {
            data <- .filter_prep_sensors(data, sensors, reverse)
        }
    }
    if(stop_if_empty && length(myClim:::.common_get_localities(data)) == 0) {
        stop("All data are removed by filter.")
    }
    data
}

.filter_prep_sensors <- function(data, sensors, reverse) {
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
    data <- purrr::map(data, locality_function)
    purrr::keep(data, function(.x) length(.x$loggers) > 0)
}

.filter_calc_sensors <- function(data, sensors, reverse) {
    locality_function <- function (locality) {
        filter_function <- if(reverse) purrr::discard else purrr::keep
        locality$sensors <- filter_function(locality$sensors, function(.x) .x$metadata@name %in% sensors)
        locality
    }
    data$localities <- purrr::map(data$localities, locality_function)
    data$localities <- purrr::keep(data$localities, function(.x) length(.x$sensors) > 0)
    data
}
