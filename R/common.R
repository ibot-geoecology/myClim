.common_convert_factors_in_dataframe <- function(dataframe) {
    factor_columns <- sapply(dataframe, is.factor)
    if(!any(factor_columns)) {
        return(dataframe)
    }
    dataframe[factor_columns] <- lapply(dataframe[factor_columns], as.character)
    return(dataframe)
}

.common_get_filtered_data <- function(data, localities=c(), sensors=c()) {
    if(length(localities) == 0) {
        localities <- names(data)
    }
    localities_filter <- list2env(sapply(localities, function(x) NULL))
    sensors_filter <- list2env(sapply(sensors, function(x) NULL))
    result <- Filter(function(x)rlang::env_has(localities_filter, x$metadata@id), data)
    for(locality_name in names(result)) {
        result[[locality_name]]$loggers <- .common_get_filtered_loggers(result[[locality_name]]$loggers, sensors_filter)
    }
    Filter(function(x) length(x$loggers) > 0, result)
}

.common_get_filtered_loggers <- function(loggers, sensors_filter) {
    result <- lapply(loggers, function(x) {
        x$sensors <- .common_get_filtered_sensors(x$sensors, sensors_filter)
        x})
    Filter(function(x) length(x$sensors) > 0, result)
}

.common_get_filtered_sensors <- function(sensors, sensors_filter) {
    if(length(sensors_filter) == 0) {
        return(sensors)
    }
    Filter(function(x) rlang::env_has(sensors_filter, x$metadata@sensor), sensors)
}

.common_get_sensor_info <- function(sensor_metadata) {
    microclim::mc_data_sensors[[sensor_metadata@sensor]]
}

.common_as_utc_posixct <- function(datetime) {
    as.POSIXct(datetime, origin="1970-01-01", tz="UTC")
}

.common_get_loggers <- function(data) {
    unname(do.call(c, lapply(data, function(x) x$loggers)))
}

.common_logger_values_as_tibble <- function(logger) {
    data <- c(list(datetime=logger$datetime), purrr::map(logger$sensors, ~ .x$values))
    tibble::as_tibble(data)
}
