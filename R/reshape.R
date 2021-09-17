#' Wideformat of sensor values
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
               datetime=as.POSIXct(result_env$datetimes, origin="1970-01-01", tz="UTC"),
               value=result_env$values)
}

.reshape_get_datetimes_of_loggers <- function(loggers){
    if(length(loggers) == 0) {
        return(c())
    }
    result <- Reduce(union, sapply(loggers, function(x) x$datetime))
    result <- sort(result)
    as.POSIXct(result, origin="1970-01-01", tz="UTC")
}

.reshape_add_wideformat_logger_columns <- function(df, locality, logger) {
    logger_df <- data.frame(datetime=logger$datetime)
    for(sensor in logger$sensors) {
        column_name <- paste(locality, logger$metadata@serial_number, sensor$metadata@sensor, sep="-")
        logger_df[column_name] <- sensor$values
    }
    merge(df, logger_df, by="datetime", all=TRUE)
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
