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
#' example_tms_wideformat <- mc_reshape_wide(example_tomst_data1, c("A6W79", "A2E32"), c("TMS_T1", "TMS_T2"))
mc_reshape_wide <- function(data, localities=c(), sensors=c()) {
    data <- mc_filter(data, localities, sensors)
    loggers <- unname(do.call(c, lapply(data, function(x) x$loggers)))
    result <- data.frame(datetime=.reshape_get_datetimes_of_loggers(loggers))
    for(locality in data) {
        for(logger in locality$loggers) {
            result <- .reshape_add_wideformat_logger_columns(result, locality$metadata@locality_id, logger)
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
        column_name <- .reshape_get_sesnor_fullname(locality, logger$metadata@serial_number, sensor$metadata@sensor_id)
        logger_df[column_name] <- sensor$values
    }
    merge(df, logger_df, by="datetime", all=TRUE)
}

.reshape_get_sesnor_fullname <- function(locality_id, logger_serial_number, sensor){
    paste(locality_id, logger_serial_number, sensor, sep="-")
}

#' Longformat of sensor values
#'
#' This function create data.frame with values of sensor
#'
#' @param data all data in standard format
#' @param localities names of localities; if empty then all
#' @param sensors names of sensors; if empty then all
#' @return data.frame with columns location, serial_number, sensor, datetime, value
#' @export
#' @examples
#' example_tms_t1_table <- microclim::mc_reshape_long(example_tomst_data, c("A6W79", "A2E32"), c("TMS_T1", "TMS_T2"))
mc_reshape_long <- function(data, localities=c(), sensors=c()) {
    data <- mc_filter(data, localities, sensors)
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
        result_env$localities <- c(result_env$localities, rep(locality$metadata@locality_id, count_items))
    }
    data.frame(location=result_env$localities,
               serial_number=result_env$serial_numbers,
               sensor=result_env$sensors,
               datetime=microclim:::.common_as_utc_posixct(result_env$datetimes),
               value=result_env$values)
}

.reshape_add_logger_rows_to_longformat_table <- function(result_env, logger){
    sensors <- purrr::keep(logger$sensors, function(x) length(x$values) > 0)
    sensor_function <- function(sensor) {
        result_env$values <- c(result_env$values, sensor$values)
        result_env$datetimes <- c(result_env$datetimes, logger$datetime)
        count_items <- length(result_env$values) - length(result_env$sensors)
        result_env$sensors <- c(result_env$sensors, rep(sensor$metadata@sensor_id, count_items))
    }
    purrr::walk(sensors, sensor_function)
}

