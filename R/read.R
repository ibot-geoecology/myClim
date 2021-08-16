#' Sensor values by localities
#'
#' This function create data.frame with values of sensor
#'
#' @param data all data in standard format
#' @param sensor name of sensor
#' @param localities names of localities
#' @return data in standard format
#' @export
read.get_sensor_values_from_localities <- function(data, sensor, localities) {
    loggers <- .get_loggers_with_sensor_from_localities(data, sensor, localities)
    result <- data.frame(datetime=.get_sensor_values_from_localities_datetime(loggers))
    for(locality in localities) {
        filtered_loggers <- Filter(function(x) sensor %in% names(x$sensors_data), data[[locality]]$loggers)
        for(logger in filtered_loggers) {
            column_name <- paste(locality, logger$metadata@serial_number, sep="-")
            result[[column_name]] <- .get_sensor_values_from_localities_series(result, logger, sensor)
        }
    }
    result
}

.get_loggers_with_sensor_from_localities <- function(data, sensor, localities) {
    result <- c()
    for(locality in localities) {
        filtered_loggers <- Filter(function(x) sensor %in% names(x$sensors_data), data[[locality]]$loggers)
        result <- c(result, filtered_loggers)
    }
    result
}

.get_sensor_values_from_localities_datetime <- function(loggers){
    if(length(loggers) == 0) {
        return(c())
    }
    result <- list()
    current_indexes <- rep(1, length(loggers))
    current_values_function <- function(x) loggers[[x]]$datetime[[current_indexes[[x]]]]
    current_values <- sapply(1:length(current_indexes), current_values_function)
    repeat {
        value <- min(current_values)
        result[[length(result) + 1]] <- value
        new_index_function <- function(logger_index) {
            if(current_values[[logger_index]] != value || current_indexes[[logger_index]] == length(loggers[[logger_index]]$datetime)) {
                return(current_indexes[[logger_index]])
            }
            return(current_indexes[[logger_index]] + 1)
        }
        new_indexes <- sapply(1:length(current_indexes), new_index_function)
        if(all(new_indexes == current_indexes)) {
            break
        }
        current_indexes <- new_indexes
        current_values <- sapply(1:length(current_indexes), current_values_function)
    }
    as.POSIXct(unlist(result), origin="1970-01-01")
}

.get_sensor_values_from_localities_series <- function(df, logger, sensor){
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
            values[[i]] <- logger$sensors_data[[sensor]]@values[[current_logger_index]]
            current_logger_index <- current_logger_index + 1
        }
    }
    values
}
