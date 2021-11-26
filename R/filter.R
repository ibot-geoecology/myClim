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
mc_filter <- function(data, localities=c(), sensors=c()) {
    if(length(localities) == 0) {
        localities <- names(data)
    }
    localities_filter <- list2env(sapply(localities, function(x) NULL))
    sensors_filter <- list2env(sapply(sensors, function(x) NULL))
    result <- Filter(function(x)rlang::env_has(localities_filter, x$metadata@locality_id), data)
    for(locality_name in names(result)) {
        result[[locality_name]]$loggers <- .filter_get_filtered_loggers(result[[locality_name]]$loggers, sensors_filter)
    }
    Filter(function(x) length(x$loggers) > 0, result)
}

.filter_get_filtered_loggers <- function(loggers, sensors_filter) {
    result <- lapply(loggers, function(x) {
        x$sensors <- .filter_get_filtered_sensors(x$sensors, sensors_filter)
        x})
    Filter(function(x) length(x$sensors) > 0, result)
}

.filter_get_filtered_sensors <- function(sensors, sensors_filter) {
    if(length(sensors_filter) == 0) {
        return(sensors)
    }
    Filter(function(x) rlang::env_has(sensors_filter, x$metadata@sensor_id), sensors)
}

