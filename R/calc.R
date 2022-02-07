#' Snow detection
#'
#' @description
#' Function add new virtual sensor to locality with information about snow detection.
#'
#' @details
#' Maximal step length of data is day.
#'
#' @param data in format for calculation
#' @param sensor name of temperature sensor
#' @param output_sensor name of new snow sensor (default "snow")
#' @param localities list of locality_ids for calculation; if NULL then all (default NULL)
#' @param dr delta range
#' @param tmax maximal temperature
#' @return input data with added snow sensor
#' @export
#' @examples
#' snow <- mc_calc_snow(example_tomst_data1, "TMS_T2", output_sensor="TMS_T2_snow")
mc_calc_snow <- function(data, sensor, output_sensor="snow", localities=NULL, dr=2, tmax=0.5) {
    myClim:::.common_stop_if_not_calc_format(data)
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
        warning(stringr::str_glue("Locality {locality$metadata@locality_id} doesn't contain sensor {sensor}. It is skipped."))
        return(locality)
    }
    .calc_warn_if_overwriting(locality, output_sensor)
    day_max_temp <- runner::runner(locality$sensors[[sensor]]$values, k=3600*24, idx=locality$datetime, f=function(x) if(length(x) == 0) NA else max(x), na_pad=TRUE)
    day_range_temp <- runner::runner(locality$sensors[[sensor]]$values, k=3600*24, idx=locality$datetime, f=function(x) if(length(x) == 0) NA else max(x) - min(x), na_pad=TRUE)
    values <- (day_range_temp < dr) & (day_max_temp < tmax)
    locality$sensors[[output_sensor]] <- myClim:::.common_get_new_sensor("snow", output_sensor, values=values)
    return(locality)
}

.calc_warn_if_overwriting <- function(locality, output_sensor) {
    if(output_sensor %in% names(locality$sensors)) {
        warning(stringr::str_glue("Sensor {output_sensor} exists in locality {locality$metadata@locality_id}. It will be overwritten."))
    }
}

#' Summary about snow detected with mc_calc_snow()
#'
#' @description
#' Function returns summary about snow detected with mc_calc_snow() aggregated over the whole period
#'
#' @details
#' If snow_sensor isn't in locality, then skipped.
#'
#' @param data in format for calculation
#' @param snow_sensor name of snow sensor created by function mc_calc_snow (default "snow")
#' @param localities list of locality_ids; if NULL then all (default NULL)
#' @param period count days for continuous cover of snow (default 3)
#' @param use_utc if set FALSE then datetime changed by locality tz_offset (default FALSE)
#' @return data.frame with columns locality, snow_days, first_day, last_day, first_day_period, last_day_period
#' @export
#' @examples
#' snow_agg <- mc_calc_snow_agg(example_tomst_data1, "TMS_T2_snow")
mc_calc_snow_agg <- function(data, snow_sensor="snow", localities=NULL, period=3, use_utc=F) {
    myClim:::.common_stop_if_not_calc_format(data)
    data <- mc_filter(data, localities, sensors=snow_sensor, stop_if_empty=FALSE)
    if(length(data$localities) == 0) {
        stop("Sensor doesn't exist in any locality.")
    }
    if(!use_utc) {
        myClim:::.prep_warn_if_unset_tz_offset(data)
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

#' Converting soil moisture from TDT signal to volumetric water content
#'
#' @description
#' Function converting soil moisture from TDT signal to volumetric water content.
#'
#' @details
#'
#' @param data in format for calculation
#' @param moist_sensor name of soil moisture sensor (default "TMS_TMSmoisture")
#'
#' Soil moisture sensor must be in TMSmoisture physical. Function use sensor$calibration table for calculation values
#' of new sensor and copy table to new_sensor.
#' Value sensor$metadata@calibrated is derived from calibration parameters.
#' @param temp_sensor name of soil temperature sensor (default "TMS_T1")
#'
#' Temperature sensor must be in T physical.
#' @param output_sensor name of new snow sensor (default "vwc_moisture")
#' @param soiltype value from mc_data_vwc_parameters in column soiltype (default "universal")
#'
#' Parameters a, b and c are used in calculation.
#' @param localities list of locality_ids for calculation; if NULL then all (default NULL)
#' @param t_ref (default 24)
#' @param acor_t (default 1.91132689118)
#' @param wcor_t (default 0.64108)
#' @return input data with added VWC moisture sensor
#' @export
#' @examples
#' calc_data <- mc_calc_vwc(calc_data, soiltype="sand", localities="A2E32")
mc_calc_vwc <- function(data, moist_sensor="TMS_TMSmoisture", temp_sensor="TMS_T1",
                        output_sensor="vwc_moisture",
                        soiltype="universal", localities=NULL,
                        t_ref=24, acor_t=1.91132689118, wcor_t=0.64108) {
    myClim:::.common_stop_if_not_calc_format(data)

    locality_function <- function(locality) {
        if(!(is.null(localities) || locality$metadata@locality_id %in% localities)) {
            return(locality)
        }
        .calc_add_vwc_to_locality(locality, moist_sensor, temp_sensor, output_sensor,
                                  soiltype, t_ref, acor_t, wcor_t)
    }
    data$localities <- purrr::map(data$localities, locality_function)
    data
}

.calc_add_vwc_to_locality <- function(locality, moist_sensor, temp_sensor, output_sensor,
                                      soiltype_value, t_ref, acor_t, wcor_t) {
    skip <- .calc_vwc_check_sensors_get_skip(locality, moist_sensor, temp_sensor, output_sensor)
    if(skip) {
        return(locality)
    }
    soil_row <- dplyr::filter(mc_data_vwc_parameters, soiltype == soiltype_value)
    if(nrow(soil_row) != 1) {
        stop(stringr::str_glue("Soiltype {soiltype_value} is unknown."))
    }
    values_table <- tibble::tibble(datetime = locality$datetime,
                                   raw = locality$sensors[[moist_sensor]]$values,
                                   temp = locality$sensors[[temp_sensor]]$values)
    calibration <- locality$sensors[[moist_sensor]]$calibration
    input_data <- .calc_split_data_by_calibration(values_table, calibration)
    data_function <- function(intercept, slope, data){
        is_calibrated <- !is.na(intercept) && !is.na(slope)
        .calc_get_vwc_values(raw_values = data$raw,
                             temp_values = data$temp,
                             cal_intercept = if(is_calibrated) intercept else 0,
                             cal_slope = if(is_calibrated) slope else 0,
                             a = soil_row$a, b = soil_row$b, c = soil_row$c,
                             t_ref = t_ref, acor_t = acor_t, wcor_t = wcor_t)
    }
    values <- purrr::pmap(dplyr::select(input_data, intercept, slope, data), data_function)
    is_calibrated <- nrow(calibration) > 0
    locality$sensors[[output_sensor]] <- myClim:::.common_get_new_sensor("moisture", output_sensor,
                                                                         values=purrr::flatten_dbl(values), calibrated = is_calibrated,
                                                                         calibration=locality$sensors[[moist_sensor]]$calibration)
    return(locality)
}

.calc_vwc_check_sensors_get_skip <- function(locality, moist_sensor, temp_sensor, output_sensor){
    if(!(moist_sensor %in% names(locality$sensors))){
        warning(stringr::str_glue("Locality {locality$metadata@locality_id} doesn't contain sensor {moist_sensor}. It is skipped."))
        return(TRUE)
    }
    moist_sensor_physical <- mc_data_sensors[[locality$sensors[[moist_sensor]]$metadata@sensor_id]]@physical
    if(moist_sensor_physical != "TMSmoisture"){
        stop(stringr::str_glue("Physical of {moist_sensor} isn't TMSmoisture."))
    }
    if(!(temp_sensor %in% names(locality$sensors))){
        warning(stringr::str_glue("Locality {locality$metadata@locality_id} doesn't contain sensor {temp_sensor}. It is skipped."))
        return(TRUE)
    }
    temp_sensor_physical <- mc_data_sensors[[locality$sensors[[temp_sensor]]$metadata@sensor_id]]@physical
    if(temp_sensor_physical != "T"){
        stop(stringr::str_glue("Physical of {temp_sensor} isn't T."))
    }
    .calc_warn_if_overwriting(locality, output_sensor)
    return(FALSE)
}

.calc_split_data_by_calibration <- function(values_table, calib_table) {
    if(nrow(calib_table) == 0) {
        calib_table <- tibble::tibble(datetime = dplyr::first(values_table$datetime),
                                      slope = NA_real_,
                                      intercept = NA_real_)
    } else if (dplyr::first(values_table$datetime) < dplyr::first(calib_table$datetime)) {
        calib_table <- tibble::add_row(calib_table,
                                       datetime = dplyr::first(values_table$datetime),
                                       slope = NA_real_,
                                       intercept = NA_real_,
                                       .before = 1)
    }
    calib_table[["end_datetime"]] <- c(as.numeric(calib_table$datetime), Inf)[-1]
    subset_function <- function(start, end) {
        dplyr::filter(values_table, datetime >= start & datetime < end)
    }
    calib_table$data <- purrr::map2(calib_table$datetime, calib_table$end_datetime, subset_function)
    calib_table
}

.calc_get_vwc_values <- function(raw_values, temp_values, cal_intercept, cal_slope,
                                 a, b, c, t_ref, acor_t, wcor_t) {
    vwc <- a * raw_values^2 + b * raw_values + c
    dcor_t <- wcor_t - acor_t
    tcor <- raw_values + (temp_values - t_ref) * (acor_t + dcor_t * vwc)
    vwc_cor <- a * (tcor + cal_intercept + cal_slope * vwc)^2 + b * (tcor + cal_intercept + cal_slope * vwc) + c
    pmin(pmax(vwc_cor, 0), 1)
}
