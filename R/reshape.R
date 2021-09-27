#' Wideformat of sensor values
#'
#' This function create data.frame with values of sensor in wide format.
#'
#' @param data all data in standard format
#' @param localities names of localities; if empty then all
#' @param sensors names of sensors; if empty then all
#' @return data in standard format
#' @export
#' @examples
#' example_tms_wideformat <- mc_reshape_wideformat(example_tms_data1, c("LOC_1", "LOC_2"), c("T1", "T2"))
mc_reshape_wideformat <- function(data, localities=c(), sensors=c()) {
    data <- microclim:::.common_get_filtered_data(data, localities, sensors)
    loggers <- unname(do.call(c, lapply(data, function(x) x$loggers)))
    result <- data.frame(datetime=.reshape_get_datetimes_of_loggers(loggers))
    for(locality in data) {
        for(logger in locality$loggers) {
            result <- .reshape_add_wideformat_logger_columns(result, locality$metadata@id, logger)
        }
    }
    result
}

.reshape_get_datetimes_of_loggers <- function(loggers){
    if(length(loggers) == 0) {
        return(c())
    }
    result <- Reduce(union, sapply(loggers, function(x) x$datetime))
    result <- sort(result)
    microclim:::.common_as_utc_posixct(result)
}

.reshape_add_wideformat_logger_columns <- function(df, locality, logger) {
    logger_df <- data.frame(datetime=logger$datetime)
    for(sensor in logger$sensors) {
        column_name <- paste(locality, logger$metadata@serial_number, sensor$metadata@sensor, sep="-")
        logger_df[column_name] <- sensor$values
    }
    merge(df, logger_df, by="datetime", all=TRUE)
}

#' Wideformat of sensor values by interval
#'
#' This function create data.frame with values of sensor in wide format.
#' Mean is computed from values in datetime interval.
#'
#' @param data all data in standard format
#' @param localities names of localities; if empty then all
#' @param sensors names of sensors; if empty then all
#' @param interval_length in minutes (default 15)
#' @return data in standard format
#' @export
#' @examples
#' example_tms_wideformat_interval <- microclim::mc_reshape_wideformat_interval(example_tms_data, c("LOC_1", "LOC_2"), c("T1", "T2"), 10)
mc_reshape_wideformat_interval <- function(data, localities=c(), sensors=c(), interval_length=15) {
    table <- mc_reshape_wideformat(data, localities, sensors)
    range <- .reshape_get_datetimes_range(table$datetime, interval_length)
    start_datetimes <- .reshape_get_start_datetime_intervals_from_range(range, interval_length)
    bins <- .reshape_get_bins_from_start_datetimes(table$datetime, start_datetimes)
    result <- .reshape_get_new_interval_data_frame(start_datetimes)
    if(ncol(table) == 1) {
        return(result)
    }
    for(i in 2:ncol(table)) {
        result[names(table)[[i]]] <- tapply(table[[i]], bins, .reshape_interval_mean)
    }
    result
}

.reshape_wideformat_interval_logger <- function(logger, interval_length=15) {
    range <- .reshape_get_datetimes_range(logger$datetime, interval_length)
    start_datetimes <- .reshape_get_start_datetime_intervals_from_range(range, interval_length)
    bins <- .reshape_get_bins_from_start_datetimes(logger$datetime, start_datetimes)
    result <- .reshape_get_new_interval_data_frame(start_datetimes)
    for(sensor in logger$sensors) {
        result[sensor$metadata@sensor] <- tapply(sensor$values, bins, .reshape_interval_mean)
    }
    result
}

.reshape_get_datetimes_range <- function(datetimes, interval_length)
{
    interval_length_seconds <- interval_length * 60
    min_datetime <- as.numeric(min(datetimes))
    min_range <- min_datetime %/% interval_length_seconds * interval_length_seconds
    max_datetime <- as.numeric(max(datetimes))
    max_range <- ceiling(max_datetime / interval_length_seconds) * interval_length_seconds
    if(max_range == max_datetime) {
        max_range <- max_range + interval_length_seconds
    }
    c(min_range, max_range)
}

.reshape_get_start_datetime_intervals_from_range <- function(range, interval_length) {
    seq(range[1], range[2], by=interval_length*60)
}

.reshape_get_bins_from_start_datetimes <- function(datetime_values, start_datetimes) {
    cut(as.numeric(datetime_values), start_datetimes, right = FALSE)
}

.reshape_get_new_interval_data_frame <- function(start_datetimes) {
    data.frame(datetime <- microclim:::.common_as_utc_posixct(start_datetimes[-length(start_datetimes)]))
}

.reshape_interval_mean <- function(x) {
    mean_result <- mean(x, na.rm = TRUE)
    if (is.nan(mean_result)) NA else mean_result
}

#' Longformat of sensor values
#'
#' This function create data.frame with values of sensor
#'
#' @param data all data in standard format
#' @param localities names of localities; if empty then all
#' @param sensors names of sensors; if empty then all
#' @return data in standard format
#' @export
#' @examples
#' example_tms_t1_table <- microclim::mc_reshape_longformat(example_tms_data, c("LOC_1", "LOC_2"), c("T1", "T2"))
mc_reshape_longformat <- function(data, localities=c(), sensors=c()) {
    data <- microclim:::.common_get_filtered_data(data, localities, sensors)
    rows_count <- .reshape_number_of_sensor_values(data)
    result_env <- new.env()
    result_env$localities <- character()
    result_env$serial_numbers <- character()
    result_env$sensors <- character()
    result_env$datetimes <- numeric()
    result_env$values <- numeric()
    for(locality in data) {
        for(logger in locality$loggers) {
            .reshape_add_logger_rows_to_longformat_table(result_env, logger)
            count_items <- length(result_env$values) - length(result_env$serial_numbers)
            result_env$serial_numbers <- c(result_env$serial_numbers, rep(logger$metadata@serial_number, count_items))
        }
        count_items <- length(result_env$values) - length(result_env$localities)
        result_env$localities <- c(result_env$localities, rep(locality$metadata@id, count_items))
    }
    data.frame(location=result_env$localities,
               serial_number=result_env$serial_numbers,
               sensor=result_env$sensors,
               datetime=microclim:::.common_as_utc_posixct(result_env$datetimes),
               value=result_env$values)
}

.reshape_number_of_sensor_values <- function(data){
    sensor_values_count <- function(sensor) length(sensor$values)
    logger_row_count <- function(logger) sum(sapply(logger$sensors, sensor_values_count))
    location_row_count <- function(location) sum(sapply(location$loggers, logger_row_count))
    sum(sapply(data, location_row_count))
}

.reshape_add_logger_rows_to_longformat_table <- function(result_env, logger){
    for(sensor in logger$sensors) {
        if(length(sensor$values) == 0){
            continue
        }
        result_env$values <- c(result_env$values, sensor$values)
        result_env$datetimes <- c(result_env$datetimes, logger$datetime)
        count_items <- length(result_env$values) - length(result_env$sensors)
        result_env$sensors <- c(result_env$sensors, rep(sensor$metadata@sensor, count_items))
    }
}

