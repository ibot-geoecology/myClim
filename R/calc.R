.calc_MESSAGE_LOCALITY_NOT_CONTAINS_SENSOR <- "Locality {locality$metadata@locality_id} doesn't contains sensor {sensor}. It is skipped."
.calc_MESSAGE_STEP_LONGER_DAY <- "Step {data$metadata@step_text} in data is too long. Maximal step is day."
.calc_MESSAGE_WRONG_PHYSICAL_UNIT <- "Physical unit of {sensor_name} isn't {unit_name}."
.calc_MESSAGE_OVERWRITE_SENSOR <- "Sensor {output_sensor} exists in locality {locality$metadata@locality_id}. It will be overwritten."
.calc_MESSAGE_SENSOR_NOT_EXISTS_IN_LOCALITIES <- "Sensor doesn't exist in any locality."
.calc_MESSAGE_UNKNONW_SIOLTYPE <- "Soiltype {soiltype_value} is unknown."

#' Snow detection from temperature
#'
#' @description
#' Function accept only myClim objects in calculation format. See [myClim::mc_agg()]. Function `mc_calc_snow` creates new virtual sensor on locality within myClim data object. Function return TRUE/FALSE vector in original time step for Snow/non-snow  events.  
#'
#' @details
#' Function was designed estimate to snow presence from temperature in situation when temperature sensor is covered by snow. Snow detection algorithm combines daily range `dr`of temperature with the maximal daily temperature `tmax`. I.e in default settings TRUE (snow presence) is returned when daily temperature range is lower than 2°C and daily maximal temperature is lower than 0.5 °C.
#' 
#' TRUE/FALSE = Snow/non-snow information is returned in original time step (e.g. 15 min, 1 h...) despite function operate with daily temperature range and maximum. Because of dependency on daily temperatures, the longest time step for snow detection allowed is day. 
#'
#' @param data myClim object in calculation format. See [myClim::mc_agg()]
#' @param sensor name of temperature sensor used for snow estimation. (e.g. TMS_T2)
#' @param output_sensor name of output snow sensor (default "snow")
#' @param localities list of locality_ids where snow sill be calculated; if NULL then all (default NULL)
#' @param dr delta range (maximal daily temperature range on sensor covered by snow)
#' @param tmax maximal daily temperature on sensor covered by snow
#' @return The new myClim data object, identical as input with added snow sensor. Time step is not modified.
#' @export
#' @examples
#' snow <- mc_calc_snow(example_tomst_data1, "TMS_T2", output_sensor="TMS_T2_snow")
mc_calc_snow <- function(data, sensor, output_sensor="snow", localities=NULL, dr=2, tmax=0.5) {
    myClim:::.common_stop_if_not_calc_format(data)
    .calc_check_maximal_day_step(data)
    locality_function <- function(locality) {
        if(!(is.null(localities) || locality$metadata@locality_id %in% localities)) {
            return(locality)
        }
        .calc_add_sensor_to_locality(locality, sensor, "snow", output_sensor, "T",
                                     .calc_snow_values_function, dr=dr, tmax=tmax)
    }
    data$localities <- purrr::map(data$localities, locality_function)
    data
}

.calc_check_maximal_day_step <- function(data) {
    if(.calc_is_step_bigger_then(data, lubridate::days(1))) {
        stop(stringr::str_glue(.calc_MESSAGE_STEP_LONGER_DAY))
    }
}

.calc_is_step_bigger_then <- function(data, max_period) {
    data_period <- lubridate::period(data$metadata@step_text)
    return(data_period > max_period)
}

.calc_snow_values_function <- function(locality, sensor, dr, tmax) {
    day_max_temp <- runner::runner(locality$sensors[[sensor]]$values, k=3600*24, idx=locality$datetime, f=function(x) if(length(x) == 0) NA else max(x), na_pad=TRUE)
    day_range_temp <- runner::runner(locality$sensors[[sensor]]$values, k=3600*24, idx=locality$datetime, f=function(x) if(length(x) == 0) NA else max(x) - min(x), na_pad=TRUE)
    (day_range_temp < dr) & (day_max_temp < tmax)
}

.calc_add_sensor_to_locality <- function(locality, sensor, output_sensor_id, output_sensor_name, sensor_physical, values_function, ...) {
    if(!.calc_check_sensor_in_locality(locality, sensor)){
        return(locality)
    }
    if(!myClim:::.model_is_physical_T(locality$sensors[[sensor]]$metadata)){
        .calc_wrong_physical_error_function(sensor, sensor_physical)
    }
    .calc_warn_if_overwriting(locality, output_sensor_name)
    values <- values_function(locality, sensor, ...)
    locality$sensors[[output_sensor_name]] <- myClim:::.common_get_new_sensor(output_sensor_id, output_sensor_name, values=values)
    return(locality)
}

.calc_check_sensor_in_locality <- function(locality, sensor) {
    result <- sensor %in% names(locality$sensors)
    if(!result){
        warning(stringr::str_glue(.calc_MESSAGE_LOCALITY_NOT_CONTAINS_SENSOR))
    }
    result
}

.calc_wrong_physical_error_function <- function(sensor_name, unit_name) {
    stop(stringr::str_glue(.calc_MESSAGE_WRONG_PHYSICAL_UNIT))
}

.calc_warn_if_overwriting <- function(locality, output_sensor) {
    if(output_sensor %in% names(locality$sensors)) {
        warning(stringr::str_glue(.calc_MESSAGE_OVERWRITE_SENSOR))
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
        stop(.calc_MESSAGE_SENSOR_NOT_EXISTS_IN_LOCALITIES)
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
#' @param ref_t (default 24)
#' @param acor_t (default 1.91132689118083)
#' @param wcor_t (default 0.64108)
#' @return input data with added VWC moisture sensor
#' @export
#' @examples
#' calc_data <- mc_calc_vwc(calc_data, soiltype="sand", localities="A2E32")
mc_calc_vwc <- function(data, moist_sensor="TMS_TMSmoisture", temp_sensor="TMS_T1",
                        output_sensor="vwc_moisture",
                        soiltype="universal", localities=NULL,
                        ref_t=myClim:::.calib_MOIST_REF_T,
                        acor_t=myClim:::.calib_MOIST_ACOR_T,
                        wcor_t=myClim:::.calib_MOIST_WCOR_T) {
    myClim:::.common_stop_if_not_calc_format(data)

    locality_function <- function(locality) {
        if(!(is.null(localities) || locality$metadata@locality_id %in% localities)) {
            return(locality)
        }
        .calc_add_vwc_to_locality(locality, moist_sensor, temp_sensor, output_sensor,
                                  soiltype, ref_t, acor_t, wcor_t)
    }
    data$localities <- purrr::map(data$localities, locality_function)
    data
}

.calc_add_vwc_to_locality <- function(locality, moist_sensor, temp_sensor, output_sensor,
                                      soiltype_value, ref_t, acor_t, wcor_t) {
    skip <- .calc_vwc_check_sensors_get_skip(locality, moist_sensor, temp_sensor, output_sensor)
    if(skip) {
        return(locality)
    }
    soil_row <- dplyr::filter(mc_data_vwc_parameters, soiltype == soiltype_value)
    if(nrow(soil_row) != 1) {
        stop(stringr::str_glue(.calc_MESSAGE_UNKNONW_SIOLTYPE))
    }
    values_table <- tibble::tibble(datetime = locality$datetime,
                                   raw = locality$sensors[[moist_sensor]]$values,
                                   temp = locality$sensors[[temp_sensor]]$values)
    calibration <- locality$sensors[[moist_sensor]]$calibration
    input_data <- myClim:::.prep_split_data_by_calibration(values_table, calibration)
    data_function <- function(intercept, slope, data){
        is_calibrated <- !is.na(intercept) && !is.na(slope)
        .calc_get_vwc_values(raw_values = data$raw,
                             temp_values = data$temp,
                             cal_intercept = if(is_calibrated) intercept else 0,
                             cal_slope = if(is_calibrated) slope else 0,
                             a = soil_row$a, b = soil_row$b, c = soil_row$c,
                             ref_t = ref_t, acor_t = acor_t, wcor_t = wcor_t)
    }
    values <- purrr::pmap(dplyr::select(input_data, intercept, slope, data), data_function)
    is_calibrated <- nrow(calibration) > 0
    locality$sensors[[output_sensor]] <- myClim:::.common_get_new_sensor("moisture", output_sensor,
                                                                         values=purrr::flatten_dbl(values), calibrated = is_calibrated,
                                                                         calibration=locality$sensors[[moist_sensor]]$calibration)
    return(locality)
}

.calc_vwc_check_sensors_get_skip <- function(locality, moist_sensor, temp_sensor, output_sensor){
    if(!.calc_check_sensor_in_locality(locality, moist_sensor)){
        return(TRUE)
    }
    if(!myClim:::.model_is_physical_TMSmoisture(locality$sensors[[moist_sensor]]$metadata)){
        .calc_wrong_physical_error_function(moist_sensor, "TMSmoisture")
    }
    if(!.calc_check_sensor_in_locality(locality, temp_sensor)){
        return(TRUE)
    }
    if(!myClim:::.model_is_physical_T(locality$sensors[[temp_sensor]]$metadata)){
        .calc_wrong_physical_error_function(temp_sensor, "T")
    }
    .calc_warn_if_overwriting(locality, output_sensor)
    return(FALSE)
}

.calc_get_vwc_values <- function(raw_values, temp_values, cal_intercept, cal_slope,
                                 a, b, c, ref_t, acor_t, wcor_t) {
    vwc <- a * raw_values^2 + b * raw_values + c
    dcor_t <- wcor_t - acor_t
    tcor <- raw_values + (temp_values - ref_t) * (acor_t + dcor_t * vwc)
    vwc_cor <- a * (tcor + cal_intercept + cal_slope * vwc)^2 + b * (tcor + cal_intercept + cal_slope * vwc) + c
    pmin(pmax(vwc_cor, 0), 1)
}

#' Growing Degree Days
#'
#' @description
#' Function add new virtual sensor with values of GDD Growing Degree Days.
#'
#' @details
#' Maximal step length of data is day.
#'
#' @param data in format for calculation
#' @param sensor name of temperature sensor
#' @param output_prefix name prefix of new GDD sensor (default "GDD")
#'
#' name of output sensor consists of output_prefix and value t_base
#' @param t_base threshold temperaturefor calculation GDD (default 5)
#' @param localities list of locality_ids for calculation; if NULL then all (default NULL)
#' @return input data with added GDD sensor
#' @export
#' @examples
mc_calc_gdd <- function(data, sensor, output_prefix="GDD", t_base=5, localities=NULL) {
    .calc_xdd(data, sensor, output_prefix, t_base, localities, .calc_gdd_values_function)
}

.calc_xdd <- function(data, sensor, output_prefix, t_base, localities, values_function) {
    myClim:::.common_stop_if_not_calc_format(data)
    .calc_check_maximal_day_step(data)

    output_sensor <- stringr::str_glue("{output_prefix}{t_base}")
    step_part_day <- data$metadata@step / (24 * 60)

    locality_function <- function(locality) {
        if(!(is.null(localities) || locality$metadata@locality_id %in% localities)) {
            return(locality)
        }
        .calc_add_sensor_to_locality(locality, sensor, "GDD", output_sensor, "T",
                                     values_function, t_base=t_base, step_part_day=step_part_day)
    }
    data$localities <- purrr::map(data$localities, locality_function)
    data
}

.calc_gdd_values_function <- function(locality, sensor, t_base, step_part_day) {
    pmax(locality$sensors[[sensor]]$values - t_base, 0) * step_part_day
}

#' Freezing Degree Days
#'
#' @description
#' Function add new virtual sensor with values of FDD Freezing Degree Days.
#'
#' @details
#' Maximal step length of data is day.
#'
#' @param data in format for calculation
#' @param sensor name of temperature sensor
#' @param output_prefix name prefix of new FDD sensor (default "FDD")
#'
#' name of output sensor consists of output_prefix and value t_base
#' @param t_base threshold temperaturefor calculation FDD (default 0)
#' @param localities list of locality_ids for calculation; if NULL then all (default NULL)
#' @return input data with added FDD sensor
#' @export
#' @examples
mc_calc_fdd <- function(data, sensor, output_prefix="FDD", t_base=0, localities=NULL) {
    .calc_xdd(data, sensor, output_prefix, t_base, localities, .calc_fdd_values_function)
}

.calc_fdd_values_function <- function(locality, sensor, t_base, step_part_day) {
    pmax(t_base - locality$sensors[[sensor]]$values, 0) * step_part_day
}

