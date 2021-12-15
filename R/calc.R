#' Snow detection
#'
#' Function detect snow based on detrended time series
#'
#' @param data all data in format for calculation
#' @param sensor name of temperature sensor
#' @param output_sensor name of new snow sensor (default "snow")
#' @param localities names for calculation; if empty then all
#' @param dr delta range
#' @param tmax maximal temperature
#' @return input data with added snow sensor
#' @export
#' @examples
#' snow <- mc_calc_snow(example_tomst_data1, "TMS_T3")
mc_calc_snow <- function(data, sensor, output_sensor="snow", localities=NULL, dr=2, tmax=0.5) {
    microclim:::.common_stop_if_not_calc_format(data)
    locality_function <- function(locality) {
        if(!(is.null(localities) || locality$metadata@locality_id %in% localities)) {
            return(locality)
        }
        .calc_add_snow_to_locality(locality, sensor, output_sensor, dr, tmax)
    }
    purrr::map(data, locality_function)
}

.calc_add_snow_to_locality <- function(locality, sensor, output_sensor, dr, tmax) {
    if(!(sensor %in% names(locality$sensors))){
        return(locality)
    }
    .calc_warn_if_overwriting(locality, output_sensor)
    day_max_temp <- runner::runner(locality$sensors[[sensor]]$values, k=3600*24, idx=locality$datetime, f=function(x) if(length(x) == 0) NA else max(x), na_pad=TRUE)
    day_range_temp <- runner::runner(locality$sensors[[sensor]]$values, k=3600*24, idx=locality$datetime, f=function(x) if(length(x) == 0) NA else max(x) - min(x), na_pad=TRUE)
    values <- (day_range_temp < dr) & (day_max_temp < tmax)
    locality$sensors[[output_sensor]] <- microclim:::.common_get_sensor(output_sensor, values=values)
    return(locality)
}

.calc_warn_if_overwriting <- function(locality, output_sensor) {
    if(output_sensor %in% names(locality$sensors)) {
        warning(stringr::str_glue("Sensor {output_sensor} exists in locality {locality$metadata@locality_id}. It will be overwritten."))
    }
}

#' Snow detection summary
#'
#' @description
#' Function return summary info about snow detection
#'
#' @details
#' If sensor isn't in locality, then NA returned.
#'
#' @param data all data in standard format
#' @param snow_sensor name of snow sensor created by function mc_calc_snow
#' @param localities names of localities; if empty then all
#' @param period count days for continuous cover of snow (default 3)
#' @param use_utc if set FALSE then datetime changed by locality tz_offset; default FALSE
#' @return data.frame with columns locality, snow_days, first_day, last_day, first_day_period, last_day_period
#' @export
#' @examples
#' snow_agg <- mc_calc_snow_agg(example_tomst_data1, "TMS_T3")
mc_calc_snow_agg <- function(data, snow_sensor, localities=NULL, period=3, use_utc=F) {
    microclim:::.common_stop_if_not_calc_format(data)
    data <- mc_filter(data, localities)
    if(!use_utc) {
        microclim:::.prep_warn_if_unset_tz_offset(data)
    }
    locality_function <- function(locality) {
        .calc_get_snow_agg_row(locality, snow_sensor, period, use_utc)
    }
    as.data.frame(purrr::map_dfr(data, locality_function))
}

.calc_get_snow_agg_row <- function(locality, snow_sensor, period, use_utc) {
    result <- list(locality_id = locality$metadata@locality_id,
                   snow_days = NA_integer_,
                   first_day = NA,
                   last_day = NA,
                   first_day_period = NA,
                   last_day_period = NA)
    if(!(snow_sensor %in% names(locality$sensors))) {
        return(result)
    }
    snow_table <- tibble::tibble(datetime=locality$datetime, snow=locality$sensors[[snow_sensor]]$values)
    snow_table <- dplyr::filter(snow_table, !is.na(snow))
    if(!use_utc) {
        snow_table$datetime <- .calc_get_datetimes_with_offset(snow_table$datetime, locality$metadata@tz_offset)
    }

    if(nrow(snow_table) == 0) {
        return(result)
    }
    snow_days_table <- aggregate(snow_table$snow, by=list(day=cut(snow_table$datetime, breaks = "days")), FUN=max)
    snow_days_table$day <- as.Date(snow_days_table$day)
    result$snow_days <- sum(snow_days_table$x)
    if(result$snow_days > 0) {
        result$first_day <- as.Date(snow_days_table$day[min(which(snow_days_table$x == 1))])
        result$last_day <- as.Date(snow_days_table$day[max(which(snow_days_table$x == 1))])
    }
    snow_by_period <- runner::runner(snow_days_table$x, k=period, idx=as.Date(snow_days_table$day), f=function(x) if(length(x) == 0) NA else min(x), na_pad=TRUE)
    snow_by_period_index <- which(snow_by_period == 1)
    if(length(snow_by_period_index) > 0) {
        result$first_day_period <- snow_days_table$day[min(snow_by_period_index) - period + 1]
        result$last_day_period <- snow_days_table$day[max(snow_by_period_index)]
    }
    result
}

.calc_get_datetimes_with_offset <- function(datetimes, tz_offset) {
    if(is.na(tz_offset) || tz_offset == 0) {
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
#' @param breaks cut function parameter
#' @param localities locality_ids for filtering data; if empty then all
#' @param sensors sensor_ids for filtering data; if empty then all
#' @param use_utc if set FALSE then datetime changed by locality tz_offset; default FALSE
#' @param ... parameters for aggregation function
#' @return aggregated data in standard format
#' @export
#' @examples
#' example_cleaned_tomst_data <- mc_calc_agg(example_cleaned_tomst_data, quantile, "hour", probs = 0.5, na.rm=TRUE)
mc_calc_agg <- function(data, fun, breaks, localities=NULL, sensors=NULL, use_utc=F, ...) {
    data <- mc_filter(data, localities, sensors)
    microclim:::.prep_warn_if_datetime_step_unprocessed(data)
    if(!use_utc) {
        microclim:::.prep_warn_if_unset_tz_offset(data)
    }
    locality_function <- function (locality) {
        logger_function <- function (logger) {
            tz_offset <- if(use_utc) 0 else locality$metadata@tz_offset
            .calc_aggregate_logger(logger, fun, breaks, tz_offset, ...)
        }
        locality$loggers <- purrr::map(locality$loggers, logger_function)
        locality
    }
    purrr::map(data, locality_function)
}

.calc_aggregate_logger <- function(logger, fun, breaks, tz_offset, ...)
{
    if(length(logger$datetime) == 0) {
        return(logger)
    }
    logger$datetime <- .calc_get_datetimes_with_offset(logger$datetime, tz_offset)
    by_aggregate <- list(step=cut(logger$datetime, breaks=breaks))
    logger$datetime <- aggregate(logger$datetime, by_aggregate, min)$x
    sensor_function <- function(sensor) {
        sensor$values <- aggregate(sensor$values, by_aggregate, fun, ...)$x
        sensor
    }
    logger$sensors <- purrr::map(logger$sensors, sensor_function)
    if(length(logger$datetime) > 1) {
        logger$clean_info@step <- diff(as.numeric(logger$datetime[1:2])) %/% 60
    } else {
        logger$clean_info@step <- NA_integer_
    }

    logger
}
