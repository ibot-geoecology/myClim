#' Snow detection
#'
#' @description
#' Function add sensor to locality with snow detection.
#'
#' @details
#' Maximal step length of data is day.
#'
#' @param data in format for calculation
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
    if(.calc_is_step_bigger_then(data, lubridate::days(1))) {
        stop(stringr::str_glue("Step {data$metadata@step_text} in data is too long. Maximal step is day."))
    }
    locality_function <- function(locality) {
        if(!(is.null(localities) || locality$metadata@locality_id %in% localities)) {
            return(locality)
        }
        .calc_add_snow_to_locality(locality, sensor, output_sensor, dr, tmax)
    }
    data$localities <- purrr::map(data$localities, locality_function)
    data
}

.calc_is_step_bigger_then <- function(data, max_period) {
    data_period <- lubridate::period(data$metadata@step_text)
    return(data_period > max_period)
}

.calc_add_snow_to_locality <- function(locality, sensor, output_sensor, dr, tmax) {
    if(!(sensor %in% names(locality$sensors))){
        return(locality)
    }
    .calc_warn_if_overwriting(locality, output_sensor)
    day_max_temp <- runner::runner(locality$sensors[[sensor]]$values, k=3600*24, idx=locality$datetime, f=function(x) if(length(x) == 0) NA else max(x), na_pad=TRUE)
    day_range_temp <- runner::runner(locality$sensors[[sensor]]$values, k=3600*24, idx=locality$datetime, f=function(x) if(length(x) == 0) NA else max(x) - min(x), na_pad=TRUE)
    values <- as.numeric((day_range_temp < dr) & (day_max_temp < tmax))
    locality$sensors[[output_sensor]] <- microclim:::.common_get_new_sensor(output_sensor, values=values)
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
#' If snow_sensor isn't in locality, then skipped.
#'
#' @param data in format for calculation
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
    data <- mc_filter(data, localities, sensors=snow_sensor, stop_if_empty=FALSE)
    if(length(data$localities) == 0) {
        stop("Sensor doesn't exist in any locality.")
    }
    if(!use_utc) {
        microclim:::.prep_warn_if_unset_tz_offset(data)
    }
    locality_function <- function(locality) {
        .calc_get_snow_agg_row(locality, snow_sensor, period, use_utc)
    }
    as.data.frame(purrr::map_dfr(data$localities, locality_function))
}

.calc_get_snow_agg_row <- function(locality, snow_sensor, period, use_utc) {
    result <- list(locality_id = locality$metadata@locality_id,
                   snow_days = NA_integer_,
                   first_day = NA,
                   last_day = NA,
                   first_day_period = NA,
                   last_day_period = NA)
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

