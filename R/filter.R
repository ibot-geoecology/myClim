#' Filter data
#'
#' This function filter data by localities and sensors
#'
#' @param data in standard format
#' @param localities locality_ids for filtering data; if empty then all
#' @param sensors sensor_ids for filtering data; if empty then all
#' @return filtered data in standard format
#' @export
#' @examples
#' example_tomst_data1 <- mc_filter(example_tomst_data1, localities=c("A6W79", "A2E32"), sensors=c("TMS_T1", "TMS_T2"))
mc_filter <- function(data, localities=NULL, sensors=NULL) {
    if(!is.null(localities)) {
        data <- purrr::keep(data, function(.x) .x$metadata@locality_id %in% localities)
    }
    if(!is.null(sensors)) {
        data <- .filter_sensors(data, sensors)
    }
    data
}

.filter_sensors <- function(data, sensors) {
    logger_function <- function (logger) {
        logger$sensors <- purrr::keep(logger$sensors, function(.x) .x$metadata@sensor_id %in% sensors)
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
