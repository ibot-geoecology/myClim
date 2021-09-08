#' Sensor values by localities
#'
#' This function create data.frame with values of sensor
#'
#' @param data all data in standard format
#' @param localities names of localities; if empty then all
#' @param sensors names of sensors; if empty then all
#' @return data in standard format
#' @export
#' @examples
#' example_tms_t1_table <- microclim::mc_reshape_wideformat(example_tms_data, c("LOC_1", "LOC_2"), c("T1", "T2"))
mc_reshape_wideformat <- function(data, localities=c(), sensors=c()) {
    if(length(localities) == 0) {
        localities <- names(data)
    }
    loggers <- .reshape_get_loggers_with_sensors_from_localities(data, localities, sensors)
    result <- data.frame(datetime=.reshape_get_datetimes_of_loggers(loggers))
    for(locality in localities) {
        filtered_loggers <- .reshape_filtered_loggers_from_locality(data, locality, sensors)
        for(logger in filtered_loggers) {
            result <- .reshape_add_wideformat_logger_columns(result, locality, logger, sensors)
        }
    }
    result
}

.reshape_filtered_loggers_from_locality <- function(data, locality, sensors)
{
    Filter(function(x) length(sensors) == 0 || length(intersect(sensors, names(x$sensors))) > 0, data[[locality]]$loggers)
}

.reshape_get_loggers_with_sensors_from_localities <- function(data, localities, sensors) {
    result <- c()
    for(locality in localities) {
        filtered_loggers <- .reshape_filtered_loggers_from_locality(data, locality, sensors)
        result <- c(result, filtered_loggers)
    }
    result
}

.reshape_get_datetimes_of_loggers <- function(loggers){
    if(length(loggers) == 0) {
        return(c())
    }
    result <- list()
    current_indexes <- rep(1, length(loggers))
    current_values_function <- function(x) {
        if(current_indexes[[x]] == 0) {
            return(0)
        }
        return(loggers[[x]]$datetime[[current_indexes[[x]]]])}
    current_values <- sapply(1:length(current_indexes), current_values_function)
    repeat {
        value <- min(Filter(function(x){x > 0}, current_values))
        result[[length(result) + 1]] <- value
        for(logger_index in 1:length(current_indexes)) {
            if(current_values[[logger_index]] == value) {
                if(current_indexes[[logger_index]] == length(loggers[[logger_index]]$datetime)) {
                    current_indexes[[logger_index]] <- 0
                }
                else {
                    current_indexes[[logger_index]] <- current_indexes[[logger_index]] + 1
                }
            }
        }
        if(all(current_indexes == 0)) {
            break
        }
        current_values <- sapply(1:length(current_indexes), current_values_function)
    }
    as.POSIXct(unlist(result), origin="1970-01-01", tz="UTC")
}

.reshape_add_wideformat_logger_columns <- function(df, locality, logger, sensors) {
    if(length(sensors) == 0) {
        sensors <- names(logger$sensors)
    }
    for(sensor in sensors) {
        if(!(sensor %in% names(logger$sensors))){
            continue
        }
        column_name <- paste(locality, logger$metadata@serial_number, sensor, sep="-")
        df[[column_name]] <- .resahpe_get_sensor_values_from_localities_series(df, logger, sensor)
    }
    df
}

.resahpe_get_sensor_values_from_localities_series <- function(df, logger, sensor){
    if(length(df$datetime) == 0)
    {
        return(c())
    }
    current_logger_index <- 1
    values <- numeric(length(df$datetime))
    for(i in 1:length(df$datetime)) {
        if(current_logger_index > length(logger$datetime) || logger$datetime[[current_logger_index]] > df$datetime[[i]])
        {
            values[[i]] <- NA_real_
        }
        else
        {
            values[[i]] <- logger$sensors[[sensor]]$values[[current_logger_index]]
            current_logger_index <- current_logger_index + 1
        }
    }
    values
}
