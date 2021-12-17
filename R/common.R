.common_convert_factors_in_dataframe <- function(dataframe) {
    factor_columns <- sapply(dataframe, is.factor)
    if(!any(factor_columns)) {
        return(dataframe)
    }
    dataframe[factor_columns] <- lapply(dataframe[factor_columns], as.character)
    return(dataframe)
}

.common_get_sensor_info <- function(sensor_metadata) {
    microclim::mc_data_sensors[[sensor_metadata@sensor_id]]
}

.common_as_utc_posixct <- function(datetime) {
    as.POSIXct(datetime, origin="1970-01-01", tz="UTC")
}

.common_get_loggers <- function(data) {
    unname(do.call(c, lapply(data, function(x) x$loggers)))
}

.common_logger_values_as_tibble <- function(logger) {
    data <- c(list(datetime=logger$datetime), purrr::map(logger$sensors, function(.x) .x$values))
    tibble::as_tibble(data)
}

.common_is_calc_format <- function(data) {
    length(data) > 0 && "sensors" %in% names(data[[1]])
}

.common_stop_if_not_calc_format <- function(data) {
    if(!.common_is_calc_format(data)) {
        stop("Format of data isn't right for calculation.")
    }
}

.common_is_prep_format <- function(data) {
    length(data) > 0 && "loggers" %in% names(data[[1]])
}

.common_stop_if_not_prep_format <- function(data) {
    if(!.common_is_prep_format(data)) {
        stop("Format of data isn't right for preparing.")
    }
}

.common_get_sensor <- function(sensor_name, sensor_id=NA_character_, values=NULL){
    metadata <- mc_SensorMetadata(sensor_id = sensor_id,
                                  name = sensor_name)
    item <- list(metadata = metadata,
                 values = values,
                 states = list())
    item
}
