#' Snow detection
#'
#' Function detect snow based on detrended time series
#'
#' @param data all data in standard format
#' @param sensor name of temperature sensor
#' @param localities names of localities; if empty then all
#' @param dr delta range
#' @param tmax maximal temperature
#' @return data.frame with datetime column and logical columns named by serial_number of loggers
#' @export
#' @examples
#' snow <- mc_eco_snow(example_tomst_data1, "TMS_T3")
mc_eco_snow <- function(data, sensor, localities=c(), dr=2, tmax=0.5) {
    data <- microclim:::.common_get_filtered_data(data, localities, sensor)
    loggers <- unname(do.call(c, lapply(data, function(x) x$loggers)))
    snow_tables <- purrr::map(loggers, function(x) .get_eco_snow_from_logger(x, dr, tmax))
    result <- purrr::reduce(snow_tables, function(x, y) dplyr::full_join(x, y, by="datetime"))
    as.data.frame(result)
}

.get_eco_snow_from_logger <- function(logger, dr, tmax) {
    microclim:::.clean_warn_if_datetime_step_unprocessed(logger)
    result = tibble::tibble(datetime=logger$datetime)
    if(length(logger$sensors) == 0){
        result[[logger$metadata@serial_number]] <- NA
        return(result)
    }
    day_max_temp <- runner::runner(logger$sensors[[1]]$values, k=3600*24, idx=logger$datetime, f=function(x) if(length(x) == 0) NA else max(x), na_pad=TRUE)
    day_range_temp <- runner::runner(logger$sensors[[1]]$values, k=3600*24, idx=logger$datetime, f=function(x) if(length(x) == 0) NA else max(x) - min(x), na_pad=TRUE)
    result[[logger$metadata@serial_number]] <- (day_range_temp < dr) & (day_max_temp < tmax)
    return(result)
}

#' Snow detection summary
#'
#' Function return summary info about snow detection
#'
#' @param data all data in standard format
#' @param sensor name of temperature sensor
#' @param localities names of localities; if empty then all
#' @param dr delta range
#' @param tmax maximal temperature
#' @param period count days for continuous cover of snow (default 3)
#' @return data.frame with columns serial_number, snow_days, first_day, last_day, first_day_period, last_day_period
#' @export
#' @examples
#' snow_agg <- mc_eco_snow_agg(example_tomst_data1, "TMS_T3")
mc_eco_snow_agg <- function(data, sensor, localities=c(), dr=2, tmax=0.5, period = 3) {
    data <- microclim:::.common_get_filtered_data(data, localities, sensor)
    loggers_with_offset <- .eco_get_loggers_with_offset(data)
    result_env <- new.env()
    result_env$serial_number <- character()
    result_env$snow_days <- numeric()
    result_env$first_day <- as.Date(x = integer(0), origin = "1970-01-01")
    result_env$last_day <- as.Date(x = integer(0), origin = "1970-01-01")
    result_env$first_day_period <- as.Date(x = integer(0), origin = "1970-01-01")
    result_env$last_day_period <- as.Date(x = integer(0), origin = "1970-01-01")
    purrr::walk(loggers_with_offset, function (x) {
        snow_table <- .get_eco_snow_from_logger(x$logger, dr, tmax)
        .eco_compute_snow_agg_from_table(snow_table, x$tz_offset, period, result_env)
    })
    data.frame(serial_number=result_env$serial_number,
               snow_days=result_env$snow_days,
               first_day=result_env$first_day,
               last_day=result_env$last_day,
               first_day_period=result_env$first_day_period,
               last_day_period=result_env$last_day_period)
}

.eco_get_loggers_with_offset <- function(data) {
    locality_function <- function(locality) {
        microclim:::.clean_warn_if_unset_tz_offset(locality)
        purrr::map(locality$loggers, function(logger) list(logger=logger, tz_offset=locality$metadata@tz_offset))
    }
    purrr::flatten(purrr::map(data, locality_function))
}

.eco_compute_snow_agg_from_table <- function(snow_table, tz_offset, period, environment) {
    environment$serial_number <- c(environment$serial_number, colnames(snow_table)[[2]])
    snow_table <- snow_table[!is.na(snow_table[[2]]), ]
    snow_table$datetime <- .eco_get_datetimes_with_offset(snow_table$datetime, tz_offset)

    if(nrow(snow_table) == 0) {
        environment$snow_days <- c(environment$snow_days, 0)
        environment$first_day <- c(environment$first_day, NA)
        environment$last_day <- c(environment$last_day, NA)
        environment$first_day_period <- c(environment$first_day_period, NA)
        environment$last_day_period <- c(environment$last_day_period, NA)
        return()
    }
    snow_days_table <- aggregate(snow_table[[2]], by=list(day=cut(snow_table$datetime, breaks = "days")), FUN=max)
    snow_days_table$day <- as.Date(snow_days_table$day)
    snow_days <- sum(snow_days_table$x)
    environment$snow_days <- c(environment$snow_days, snow_days)
    if(snow_days == 0) {
        environment$first_day <- c(environment$first_day, NA)
        environment$last_day <- c(environment$last_day, NA)
    }
    else {
        environment$first_day <- c(environment$first_day, as.Date(snow_days_table$day[min(which(snow_days_table$x == 1))]))
        environment$last_day <- c(environment$last_day, as.Date(snow_days_table$day[max(which(snow_days_table$x == 1))]))
    }
    snow_by_period <- runner::runner(snow_days_table$x, k=period, idx=as.Date(snow_days_table$day), f=function(x) if(length(x) == 0) NA else min(x), na_pad=TRUE)
    snow_by_period_index <- which(snow_by_period == 1)
    if(length(snow_by_period_index) == 0) {
        environment$first_day_period <- c(environment$first_day_period, NA)
        environment$last_day_period <- c(environment$last_day_period, NA)
    }
    else {
        environment$first_day_period <- c(environment$first_day_period, snow_days_table$day[min(snow_by_period_index) - period + 1])
        environment$last_day_period <- c(environment$last_day_period, snow_days_table$day[max(snow_by_period_index)])
    }
}

.eco_get_datetimes_with_offset <- function(datetimes, tz_offset) {
    if(is.na(tz_offset)) {
        return(datetimes)
    }
    datetimes + (tz_offset * 60)
}

#' Agregate data by function
#'
#' Function return aggregated data by function
#'
#' @param data all data in standard format
#' @param fun aggregation function
#' @param braks cut function parameter
#' @param localities locality_ids for filtering data; if empty then all
#' @param snesors sensor_ids for filtering data; if empty then all
#' @return aggregated data in standard formta
#' @export
#' @examples
#' example_cleaned_tomst_data <- mc_eco_agg(example_cleaned_tomst_data, quantile, "hour", probs = 0.5, na.rm=TRUE)
mc_eco_agg <- function(data, fun, breaks, localities=c(), sensors=c(), ...) {
    data <- microclim:::.common_get_filtered_data(data, localities, sensors)
    locality_function <- function (locality) {
        locality$loggers <- purrr::map(locality$loggers, function (logger) .eco_aggregate_logger(logger, fun, breaks, ...))
        locality
    }
    purrr::map(data, locality_function)
}

.eco_aggregate_logger <- function(logger, fun, breaks, ...)
{
    microclim:::.clean_warn_if_datetime_step_unprocessed(logger)
    by_aggregate <- list(step=cut(logger$datetime, breaks=breaks))
    logger$datetime <- aggregate(logger$datetime, by_aggregate, min)$x
    sensor_function <- function(sensor) {
        sensor$values <- aggregate(sensor$values, by_aggregate, fun, ...)$x
        sensor
    }
    logger$sensors <- purrr::map(logger$sensors, sensor_function)
    logger
}