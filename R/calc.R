.calc_const_MESSAGE_LOCALITY_NOT_CONTAINS_SENSOR <- "Locality {item$metadata@locality_id} doesn't contains sensor {sensor}. It is skipped."
.calc_const_MESSAGE_LOGGER_NOT_CONTAINS_SENSOR <- "Loger {item$metadata@serial_number} doesn't contains sensor {sensor}. It is skipped."
.calc_const_MESSAGE_STEP_LONGER_DAY <- "Step {data$metadata@step_text} in data is too long. Maximal step is day."
.calc_const_MESSAGE_LOGGER_STEP_LONGER_DAY <- "Step in logger {logger$metadata@serial_number} is too long. Maximal step is day. It is skipped."
.calc_const_MESSAGE_WRONG_PHYSICAL_UNIT <- "Physical unit of {sensor_name} isn't {unit_name}."
.calc_const_MESSAGE_OVERWRITE_SENSOR <- "Sensor {output_sensor} exists in locality {locality$metadata@locality_id}. It will be overwritten."
.calc_const_MESSAGE_SENSOR_NOT_EXISTS_IN_LOCALITIES <- "Sensor doesn't exist in any locality."
.calc_const_MESSAGE_UNKNONW_SIOLTYPE <- "Soiltype {soiltype_value} is unknown."
.calc_const_MESSAGE_VPD_AGGREGATED <- "You are attempting to calculate VPD from aggregated temperature / RH values. Due to non-linear relationship between VPD and T/RH, this may produce biased VPD estimates."

#' Snow detection from temperature
#'
#' @description
#' Function detects snow cover from temperature time-series.
#'
#' @details
#' Function detects snow cover from temperature time-series. Temperature sensor is considered as covered by snow
#' when the maximal temperature in the preceding or subsequent time-window (specified by 'days' param)
#' does not exceed specific 'tmax' threshold value (default 1.25°C) and the temperature range remain below specified
#' 'range' threshold (default 1°C). This function rely on insulating effect of a thick layer of snow,
#' significantly reducing diurnal temperature variation and restricting the maximal temperature near the ground
#' close to freezing point. Temperature sensor near the ground ('TMS_T2') is default choice for snow-cover detection.
#' Snow detection with default values accurately detects snow of depth > 15cm (unpublished data).
#' For detection of thin snow, range parameter should be set to 3-4 °C.
#' The function returns vector of snow cover with same time-step as input data. To get number of days with snow cover
#' and more info, apply [mc_calc_snow_agg].
#'
#' @param data myClim object in cleaned Prep-format or Calc-formt see [myClim::mc_agg()] and [myClim-package]
#' @param sensor name of temperature sensor used for snow estimation. (e.g. TMS_T2)
#' @param output_sensor name of output snow sensor (default "snow")
#' @param localities list of locality_ids where snow sill be calculated; if NULL then all (default NULL)
#' @param range maximal temperature range threshold for snow-covered sensor (default 1°C)
#' @param tmax maximal temperature threshold for snow-covered sensor  (default 1.25°C)
#' @param days number of days to be used for moving-window for snow detection algorithm (default 3 days)
#' @return myClim object with added virtual sensor 'snow' (logical) indicating snow presence.
#' @export
#' @examples
#' data <- mc_calc_snow(mc_data_example_calc, "TMS_T2", output_sensor="TMS_T2_snow", localities = c("A2E32", "A6W79"))
mc_calc_snow <- function(data, sensor, output_sensor="snow", localities=NULL, range=1, tmax=1.25, days=3) {
    is_calc <- myClim:::.common_is_calc_format(data)
    if(is_calc) {
        .calc_check_maximal_day_step(data)
    } else {
        myClim:::.prep_check_datetime_step_unprocessed(data, stop)
    }

    call_snow <- function(item) {
        .calc_add_sensor_to_item(item, sensor, myClim:::.model_const_SENSOR_snow_bool, output_sensor,
                                 myClim:::.model_const_PHYSICAL_T_C,
                                 .calc_snow_values_function, range=range, tmax=tmax, days=days)
    }

    logger_function <- function(logger) {
        if(.calc_check_maximal_day_step_in_logger_get_skip(logger))
        {
            return(logger)
        }
        call_snow(logger)
    }

    locality_function <- function(locality) {
        if(!(is.null(localities) || locality$metadata@locality_id %in% localities)) {
            return(locality)
        }
        if(is_calc) {
            return(call_snow(locality))
        }

        locality$loggers <- purrr::map(locality$loggers, logger_function)
        return(locality)
    }
    localities <- purrr::map(myClim:::.common_get_localities(data), locality_function)
    .common_set_localities(data, localities)
}

.calc_check_maximal_day_step <- function(data) {
    if(.calc_is_step_bigger_then(data, lubridate::days(1))) {
        stop(stringr::str_glue(.calc_const_MESSAGE_STEP_LONGER_DAY))
    }
}

.calc_check_maximal_day_step_in_logger_get_skip <- function(logger) {
    logger_period <- lubridate::minutes(logger$clean_info@step)
    if(logger_period > lubridate::days(1)) {
        warning(stringr::str_glue(.calc_const_MESSAGE_LOGGER_STEP_LONGER_DAY))
        return(TRUE)
    }
    return(FALSE)
}

.calc_is_step_bigger_then <- function(data, max_period) {
    data_period <- lubridate::period(data$metadata@step_text)
    return(data_period > max_period)
}

.calc_snow_values_function <- function(locality, sensor_name, range, tmax, days) {
    per <- 3600*24*days
    day_max_temp_prev <- runner::runner(locality$sensors[[sensor_name]]$values, k=per, idx=locality$datetime, f=function(x) if(length(x) == 0) NA else max(x), na_pad=TRUE)
    day_range_temp_prev <- runner::runner(locality$sensors[[sensor_name]]$values, k=per, idx=locality$datetime, f=function(x) if(length(x) == 0) NA else max(x) - min(x), na_pad=TRUE)
    snow_prev <- (day_range_temp_prev < range) & (day_max_temp_prev < tmax)
    snow <- runner::runner(snow_prev, k=per, lag = -per+1, idx=locality$datetime, f=function(x) if(length(x[is.finite(x)]) == 0) NA else any(x,na.rm = T), na_pad=F)

    return(snow)
}

.calc_add_sensor_to_item <- function(item, sensor_name, output_sensor_id, output_sensor_name, sensor_physical=NULL, values_function, ...) {
    if(!.calc_check_sensor_in_item(item, sensor_name)){
        return(item)
    }
    if(!is.null(sensor_physical) && !myClim:::.model_is_physical(item$sensors[[sensor_name]]$metadata, sensor_physical)){
        .calc_wrong_physical_error_function(sensor_name, sensor_physical)
    }
    .calc_warn_if_overwriting(item, output_sensor_name)
    height <- item$sensors[[sensor_name]]$metadata@height
    values <- values_function(item, sensor_name, ...)
    item$sensors[[output_sensor_name]] <- myClim:::.common_get_new_sensor(output_sensor_id, output_sensor_name, values=values, height=height)
    return(item)
}

.calc_check_sensor_in_item <- function(item, sensor) {
    result <- sensor %in% names(item$sensors)
    if(!result){
        if(is(item$metadata, "mc_LocalityMetadata")){
            warning(stringr::str_glue(.calc_const_MESSAGE_LOCALITY_NOT_CONTAINS_SENSOR))
        } else {
            warning(stringr::str_glue(.calc_const_MESSAGE_LOGGER_NOT_CONTAINS_SENSOR))
        }
    }
    result
}

.calc_wrong_physical_error_function <- function(sensor_name, unit_name) {
    stop(stringr::str_glue(.calc_const_MESSAGE_WRONG_PHYSICAL_UNIT))
}

.calc_warn_if_overwriting <- function(item, output_sensor) {
    if(output_sensor %in% names(item$sensors)) {
        warning(stringr::str_glue(.calc_const_MESSAGE_OVERWRITE_SENSOR))
    }
}

#' Summary of TRUE/FALSE snow sensor
#'
#' @description
#' This function works with the virtual snow sensor of TRUE/FALSE 
#' which is the output of [myClim::mc_calc_snow()]. So, before calling 
#' `mc_calc_snow_agg` you need to calculate or import TRUE/FALSE snow detection sensor.  
#' `mc_calc_snow_agg`returns the summary table of snow sensor 
#' (e.g number of days with snow cover, the longest continual snow period...). 
#' The snow summary is returned for whole date range provided. 
#'
#' @details
#' Primary designed for virtual snow sensor created by [myClim::mc_calc_snow()], 
#' but accepts any sensor with TRUE/FLAST snow event detection. If `snow_sensor` 
#' on the locality missing, then locality is skipped.
#'
#' @param data myClim object in cleaned Prep-format or Calc-formt (see [myClim::mc_agg()] and [myClim-package]) with TRUE/FALSE snow sensor see [myClim::mc_calc_snow()]
#' @param snow_sensor name of snow sensor containing TRUE/FALS snow detection, suitable for virtual sensors created by function `mc_calc_snow`; (default "snow")
#' @param localities optional subset of localities where to run the function (list of locality_ids); if NULL then return all localities (default NULL)
#' @param period number of days defining the continuous snow cover period of interest (default 3 days)
#' @param use_utc if set FALSE then time is corrected based on offset provided in locality metadata `tz_offset`, see e.g. [myClim::mc_prep_solar_tz()], [myClim::mc_prep_meta()]; (default FALSE)
#' @return 
#' Returns data.frame with columns:
#' * locality - locality name
#' * snow_days - number of days with snow cover
#' * first_day - first day with snow
#' * last_day - last day with snow 
#' * first_day_period - first day of period with continual snow cover based on `period` parameter
#' * last_day_period - last day of period with continual snow cover based on `period` parameter
#' 
#' @export
#' @examples
#' data <- mc_calc_snow(mc_data_example_calc, "TMS_T2", output_sensor="TMS_T2_snow", localities = c("A2E32", "A6W79"))
#' mc_calc_snow_agg(data, "TMS_T2_snow")
mc_calc_snow_agg <- function(data, snow_sensor="snow", localities=NULL, period=3, use_utc=F) {
    data <- mc_filter(data, localities, sensors=snow_sensor, stop_if_empty=FALSE)
    is_calc <- myClim:::.common_is_calc_format(data)
    if((is_calc && length(data$localities) == 0) || (!is_calc && length(data) == 0)) {
        stop(.calc_const_MESSAGE_SENSOR_NOT_EXISTS_IN_LOCALITIES)
    }
    if(!use_utc) {
        myClim:::.prep_warn_if_unset_tz_offset(data)
    }
    locality_function <- function(locality) {
        if(is_calc) {
            return(.calc_get_snow_agg_row(locality, locality$metadata@locality_id, locality$metadata@tz_offset, snow_sensor, period, use_utc))
        }

        logger_function <- function(logger) {
            .calc_get_snow_agg_row(logger, locality$metadata@locality_id, locality$metadata@tz_offset, snow_sensor, period, use_utc)
        }
        purrr::map_dfr(locality$loggers, logger_function)
    }
    as.data.frame(purrr::map_dfr(myClim:::.common_get_localities(data), locality_function))
}

.calc_get_snow_agg_row <- function(item, locality_id, tz_offset, snow_sensor, period, use_utc) {
    result <- list(locality_id = locality_id,
                   snow_days = NA_integer_,
                   first_day = NA,
                   last_day = NA,
                   first_day_period = NA,
                   last_day_period = NA)
    snow_table <- tibble::tibble(datetime=item$datetime, snow=item$sensors[[snow_sensor]]$values)
    snow_table <- dplyr::filter(snow_table, !is.na(snow))
    if(!use_utc) {
        snow_table$datetime <- .calc_get_datetimes_with_offset(snow_table$datetime, tz_offset)
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

#' Converting soil moisture from raw TMS units to volumetric water content
#'
#' @description
#' This function creates new virtual sensor on locality within myClim data object.
#' Function converts the soil moisture from raw TMS units (scaled TDT signal) to volumetric water content. 
#'
#' @details
#'This function is suitable for TMS loggers measuring soil moisture in raw TDT units (TMS units).
#' Raw TDT units represents inverted and scaled (1-4095) number of high frequency-shaped electromagnetic
#' pulses (ca 2.5 GHz) sent through a ca 30 cm long circuit within a 640-microsecond time window.
#' For more details see (Wild et al. 2019). Pulses counted is directly related to the soil moisture content,
#' with higher soil moisture. Therefore, based on experimental calibration curves, it is possible to directly
#' convert to standardized volumetric water content in cubic meters. For more details see (Kopecky et al. 2021).
#'
#' The function uses experimentally derived calibration curves under reference temperatures for several soil types.
#' For the volumetric water content conversion the reference temperature is corrected with the actual soil temperature
#' using TMS_T1 soil temperature sensor records. As the calibration curves were derived for several soil types,
#' in case user know specific soil type, where the logger was measuring, then it is possible to chose the closest
#' existing calibration curve for specific soil type instead of default "universal".
#' Available soil types are: sand, loamy sand A, loamy sand B, sandy loam A, sandy loam B, loam, silt loam, peat, water,
#' universal, sand TMS1, loamy sand TMS1, silt loam TMS1. For more details see (Wild et al. 2019).
#' For full table of function parameters see [mc_data_vwc_parameters]
#' 
#' The function by default replace the moisture records while soil is frozen with NA, because the soil
#' moisture sensor was not designed to measure in frozen soils and the returned records are thus not comparable
#' with values from non frozen soil.  
#'
#' @param data myClim object in cleaned Prep-format or Calc-formt see [myClim::mc_agg()] and [myClim-package]
#' @param moist_sensor name of soil moisture sensor to be converted from raw to volumetric (default "TMS_TMSmoisture") see `names(mc_data_sensors)`
#'
#' Soil moisture sensor must be in TMSmoisture physical.
#' @param temp_sensor name of soil temperature sensor (default "TMS_T1") see `names(mc_data_sensors)`
#'
#' Temperature sensor must be in T_C physical.
#' @param output_sensor name of new snow sensor (default "VWC_moisture")
#' @param soiltype value from mc_data_vwc_parameters in column soiltype (default "universal")
#'
#' Parameters a, b and c are used in calculation.
#' @param localities list of locality_ids for calculation; if NULL then all (default NULL)
#' @param ref_t (default 24)
#' @param acor_t (default 1.91132689118083) correction temperature while sensor on the air see [myClim::mc_calib_moisture()]
#' @param wcor_t (default 0.64108) correction temperature while sensor in the water [myClim::mc_calib_moisture()]
#' @param frozen2NA if TRUE then those moisture records are set to NA when soil temperature is below 0 (default TRUE)
#' @return myClim object same as input but with added VWC moisture sensor
#' @export
#' @seealso [mc_data_vwc_parameters]
#' @references
#' Wild, J., Kopecky, M., Macek, M., Sanda, M., Jankovec, J., Haase, T., 2019. Climate at ecologically relevant scales:
#' A new temperature and soil moisture logger for long-term microclimate measurement. Agric. For. Meteorol. 268, 40-47.
#' https://doi.org/10.1016/j.agrformet.2018.12.018
#' 
#' Kopecky, M., Macek, M., Wild, J., 2021. Topographic Wetness Index calculation guidelines based on measured soil
#' moisture and plant species composition. Sci. Total Environ. 757, 143785. https://doi.org/10.1016/j.scitotenv.2020.143785
#' 
#' @examples
#' calc_data <- mc_calc_vwc(mc_data_example_calc, soiltype="sand", localities="A2E32")
mc_calc_vwc <- function(data, moist_sensor=myClim:::.model_const_SENSOR_TMS_TMSmoisture,
                        temp_sensor=myClim:::.model_const_SENSOR_TMS_T1,
                        output_sensor="VWC_moisture",
                        soiltype="universal", localities=NULL,
                        ref_t=myClim:::.calib_MOIST_REF_T,
                        acor_t=myClim:::.calib_MOIST_ACOR_T,
                        wcor_t=myClim:::.calib_MOIST_WCOR_T,
                        frozen2NA=TRUE) {
    is_calc <- myClim:::.common_is_calc_format(data)
    if(!is_calc) {
        myClim:::.prep_check_datetime_step_unprocessed(data, stop)
    }
    call_vwc <- function(item) {
        .calc_add_vwc_to_item(item, moist_sensor, temp_sensor, output_sensor,
                              soiltype, ref_t, acor_t, wcor_t, frozen2NA)
    }
    locality_function <- function(locality) {
        if(!(is.null(localities) || locality$metadata@locality_id %in% localities)) {
            return(locality)
        }
        if(is_calc) {
            return(call_vwc(locality))
        }

        locality$loggers <- purrr::map(locality$loggers, ~ call_vwc(.x))
        return(locality)
    }
    out_localities <- purrr::map(myClim:::.common_get_localities(data), locality_function)
    myClim:::.common_set_localities(data, out_localities)
}

.calc_add_vwc_to_item <- function(item, moist_sensor, temp_sensor, output_sensor,
                                  soiltype_value, ref_t, acor_t, wcor_t, frozen2NA) {
    skip <- .calc_vwc_check_sensors_get_skip(item, moist_sensor, temp_sensor, output_sensor)
    if(skip) {
        return(item)
    }
    soil_row <- dplyr::filter(mc_data_vwc_parameters, soiltype == soiltype_value)
    if(nrow(soil_row) != 1) {
        stop(stringr::str_glue(.calc_const_MESSAGE_UNKNONW_SIOLTYPE))
    }
    values_table <- tibble::tibble(datetime = item$datetime,
                                   raw = item$sensors[[moist_sensor]]$values,
                                   temp = item$sensors[[temp_sensor]]$values)
    calibration <- item$sensors[[moist_sensor]]$calibration
    input_data <- myClim:::.prep_split_data_by_calibration(values_table, calibration)
    data_function <- function(cor_factor, cor_slope, data){
        is_calibrated <- !is.na(cor_factor) && !is.na(cor_slope)
        .calc_get_vwc_values(raw_values = data$raw,
                             temp_values = data$temp,
                             cal_cor_factor = if(is_calibrated) cor_factor else 0,
                             cal_cor_slope = if(is_calibrated) cor_slope else 0,
                             a = soil_row$a, b = soil_row$b, c = soil_row$c,
                             ref_t = ref_t, acor_t = acor_t, wcor_t = wcor_t,
                             frozen2NA = frozen2NA)
    }
    values <- purrr::pmap(dplyr::select(input_data, cor_factor, cor_slope, data), data_function)
    is_calibrated <- nrow(calibration) > 0
    height <- item$sensors[[moist_sensor]]$metadata@height
    item$sensors[[output_sensor]] <- myClim:::.common_get_new_sensor(myClim:::.model_const_SENSOR_moisture, output_sensor,
                                                                     values=purrr::flatten_dbl(values), height=height,
                                                                     calibrated = is_calibrated,
                                                                     calibration=item$sensors[[moist_sensor]]$calibration)
    return(item)
}

.calc_vwc_check_sensors_get_skip <- function(item, moist_sensor, temp_sensor, output_sensor){
    if(!.calc_check_sensor_in_item(item, moist_sensor)){
        return(TRUE)
    }
    if(!myClim:::.model_is_physical_TMSmoisture(item$sensors[[moist_sensor]]$metadata)){
        .calc_wrong_physical_error_function(moist_sensor, myClim:::.model_const_PHYSICAL_TMSmoisture)
    }
    if(!.calc_check_sensor_in_item(item, temp_sensor)){
        return(TRUE)
    }
    if(!myClim:::.model_is_physical_T_C(item$sensors[[temp_sensor]]$metadata)){
        .calc_wrong_physical_error_function(temp_sensor, myClim:::.model_const_PHYSICAL_T_C)
    }
    .calc_warn_if_overwriting(item, output_sensor)
    return(FALSE)
}

.calc_get_vwc_values <- function(raw_values, temp_values, cal_cor_factor, cal_cor_slope,
                                 a, b, c, ref_t, acor_t, wcor_t, frozen2NA) {
    vwc <- a * raw_values^2 + b * raw_values + c
    dcor_t <- wcor_t - acor_t
    tcor <- raw_values + (temp_values - ref_t) * (acor_t + dcor_t * vwc)
    vwc_cor <- a * (tcor + cal_cor_factor + cal_cor_slope * vwc)^2 + b * (tcor + cal_cor_factor + cal_cor_slope * vwc) + c
    result <- pmin(pmax(vwc_cor, 0), 1)
    if(frozen2NA) {
        result[temp_values < 0] <- NA_real_
    }
    result
}

#' Growing Degree Days
#'
#' @description
#' This function creates new virtual sensor on locality within myClim data object. New virtual sensor 
#' hosts values of GDD (Growing Degree Days) in Celsius degrees in original time step. see details 
#'
#' @details
#' Maximal allowed step length for GDD calculation is day and shorter. Function creates new virtual sensor with
#' the same time step as input data. I. e. when the time step is shorter than a day (whch is not very intuitive for growing degree **days**) 
#' than growing degree day is divided into smaller time step but still summing the day. 
#' For shorter intervals (steps) than the day, the GDD value is the contribution
#' of the interval to the growing degree day.
#' Be careful while aggregating growing degree days to longer periods
#' see [myClim::mc_agg()] only meaningful aggregation function is `sum`, but myClim let you apply anything.
#'
#' @param data myClim object in cleaned Prep-format or Calc-formt see [myClim::mc_agg()] and [myClim-package]
#' @param sensor name of temperature sensor used fot GDD calculation e.g. TMS_T3 see `names(mc_data_sensors)`
#' @param output_prefix name prefix of new GDD sensor (default "GDD")
#'
#' name of output sensor consists of output_prefix and value t_base e.g. GDD5
#' @param t_base base temperature for calculation of GDD (default 5)
#' @param localities list of locality_ids for calculation; if NULL then all (default NULL)
#' @return The same myClim object as input but with added GDD sensor
#' @export
#' @examples
#' gdd_data <- mc_calc_gdd(mc_data_example_calc, "TMS_T3", localities = c("A2E32", "A6W79"))
#' gdd_agg <- mc_agg(gdd_data, list(TMS_T3=c("min", "max"), GDD5="sum"), period="day")
mc_calc_gdd <- function(data, sensor, output_prefix="GDD", t_base=5, localities=NULL) {
    .calc_xdd(data, sensor, myClim:::.model_const_SENSOR_GDD, output_prefix, t_base, localities, .calc_gdd_values_function)
}

.calc_xdd <- function(data, sensor, output_sensor_id, output_prefix, t_base, localities, values_function) {
    is_calc <- myClim:::.common_is_calc_format(data)
    if(is_calc) {
        .calc_check_maximal_day_step(data)
        data_step_part_day <- data$metadata@step / (24 * 60)
    } else {
        myClim:::.prep_check_datetime_step_unprocessed(data, stop)
    }

    output_sensor <- .calc_get_name_with_base(output_prefix, t_base)

    call_add_sensor <- function(item, step_part_day) {
        .calc_add_sensor_to_item(item, sensor, output_sensor_id, output_sensor,
                                 myClim:::.model_const_PHYSICAL_T_C,
                                 values_function, t_base=t_base, step_part_day=step_part_day)
    }

    logger_function <- function(logger) {
        if(.calc_check_maximal_day_step_in_logger_get_skip(logger))
        {
            return(logger)
        }
        step_part_day <- logger$clean_info@step / (24 * 60)
        call_add_sensor(logger, step_part_day)
    }

    locality_function <- function(locality) {
        if(!(is.null(localities) || locality$metadata@locality_id %in% localities)) {
            return(locality)
        }
        if(is_calc) {
            return(call_add_sensor(locality, data_step_part_day))
        }
        locality$loggers <- purrr::map(locality$loggers, logger_function)
        return(locality)
    }

    out_localities <- purrr::map(myClim:::.common_get_localities(data), locality_function)
    myClim:::.common_set_localities(data, out_localities)
}

.calc_get_name_with_base <- function(prefix, base) {
    sign_text <- if(base < 0) "minus" else ""
    stringr::str_glue("{prefix}{sign_text}{abs(base)}")
}

.calc_gdd_values_function <- function(locality, sensor_name, t_base, step_part_day) {
    pmax(locality$sensors[[sensor_name]]$values - t_base, 0) * step_part_day
}

#' Freezing Degree Days
#'
#' @description
#' Function add new virtual sensor with values of FDD Freezing Degree Days.
#'
#' @details
#' Maximal allowed step length for FDD calculation is day and shorter. Function creates new virtual sensor with
#' the same time step as input data. I. e. when the time step is shorter than a day than freezing degree day
#' is divided into smaller time step but still summing the day. For shorter intervals than the day the FDD value
#' is the contribution of the interval to the freezing degree day.
#' Be careful while aggregating freezing degree days to longer periods see [myClim::mc_agg()] only meaningful
#' aggregation function is `sum`, but user is allowed to apply anything.
#'
#' @param data myClim object in cleaned Prep-format or Calc-formt see [myClim::mc_agg()] and [myClim-package]
#' @param sensor name of temperature sensor used fot FDD calculation e.g. TMS_T3 see `names(mc_data_sensors)`
#' @param output_prefix name prefix of new FDD sensor (default "FDD")
#'
#' name of output sensor consists of output_prefix and value t_base
#' @param t_base threshold temperaturefor calculation FDD (default 0)
#' @param localities list of locality_ids for calculation; if NULL then all (default NULL)
#' @return The same myClim object as input but with added GDD sensor
#' @export
#' @examples
#' fdd_data <- mc_calc_fdd(mc_data_example_calc, "TMS_T3", localities = c("A2E32", "A6W79"))
#' fdd_agg <- mc_agg(fdd_data, list(TMS_T3=c("min", "max"), FDD5="sum"), period="day")
mc_calc_fdd <- function(data, sensor, output_prefix="FDD", t_base=0, localities=NULL) {
    .calc_xdd(data, sensor, myClim:::.model_const_SENSOR_FDD, output_prefix, t_base, localities, .calc_fdd_values_function)
}

.calc_fdd_values_function <- function(locality, sensor_name, t_base, step_part_day) {
    pmax(t_base - locality$sensors[[sensor_name]]$values, 0) * step_part_day
}

#' Cumulative sum
#'
#' @description
#' Function calculate cumulative sum for the values on the sensor and add output as new sensors.
#' Names of new sensors are original sensor name + `outpus_suffix`.
#'
#' @details
#' If value type of sensor is logical, then output type is integer.
#'
#' @param data myClim object in cleaned Prep-format or Calc-formt see [myClim::mc_agg()] and [myClim-package]
#' @param sensors names of sensors where to calculate cumulative sum
#' @param output_suffix name suffix for new names (default "_cumsum") e.g. TMS_T3_cumsum
#' @param localities list of locality_ids for calculation; if NULL then all (default NULL)
#' @return The same myClim object as input but with added cumsum sensors.
#' @export
#' @examples
#' cumsum_data <- mc_calc_cumsum(mc_data_example_calc, c("TMS_T1", "TMS_T2"))
mc_calc_cumsum <- function(data, sensors, output_suffix="_cumsum", localities=NULL) {
    is_calc <- myClim:::.common_is_calc_format(data)
    if(!is_calc) {
        myClim:::.prep_check_datetime_step_unprocessed(data, stop)
    }

    values_function <- function(locality, sensor_name) {
        cumsum(locality$sensors[[sensor_name]]$values)
    }

    sensor_function <- function(item, sensor_name) {
        if(!.calc_check_sensor_in_item(item, sensor_name)){
            return(item)
        }
        origin_sensor <- item$sensors[[sensor_name]]
        output_sensor_id <- origin_sensor$metadata@sensor_id
        output_sensor_name <- stringr::str_glue("{origin_sensor$metadata@name}{output_suffix}")
        item <- .calc_add_sensor_to_item(item, sensor_name, output_sensor_id, output_sensor_name,
                                         values_function = values_function)
        if(is.logical(origin_sensor$values) && !is.logical(item$sensors[[output_sensor_name]]$values)) {
            item$sensors[[output_sensor_name]]$metadata@sensor_id <- myClim:::.model_const_SENSOR_integer
        }
        item
    }

    all_sensors_function <- function(item) {
        for (sensor_name in sensors) {
            item <- sensor_function(item, sensor_name)
        }
        item
    }

    locality_function <- function(locality) {
        if(!(is.null(localities) || locality$metadata@locality_id %in% localities)) {
            return(locality)
        }

        if(is_calc) {
            return(all_sensors_function(locality))
        }

        locality$loggers <- purrr::map(locality$loggers, all_sensors_function)
        return(locality)
    }

    out_localities <- purrr::map(myClim:::.common_get_localities(data), locality_function)
    myClim:::.common_set_localities(data, out_localities)
}

#' Converting raw values of TOMST dendrometer to micrometers
#'
#' @description
#' This function convert change in stem size fram raw TOMST units to micrometers.
#'
#' @param data myClim object in cleaned Prep-format or Calc-formt see [myClim::mc_agg()] and [myClim-package]
#' @param dendro_sensor name of change in stem size sensor to be converted from raw to micrometers (default "DEND_TOMSTdendro") see `names(mc_data_sensors)`
#' @param output_sensor name of new change in stem size sensor (default "dendro_l_um")
#' @param localities list of locality_ids for calculation; if NULL then all (default NULL)
#' @return myClim object same as input but with added dendro_l_um sensor
#' @export
#' @examples
#' calc_data <- mc_calc_tomst_dendro(mc_data_example_calc, localities="A1E05")
mc_calc_tomst_dendro <- function(data, dendro_sensor=myClim:::.model_const_SENSOR_DEND_TOMSTdendro,
                        output_sensor=myClim:::.model_const_SENSOR_dendro_l_um,
                        localities=NULL) {
    is_calc <- myClim:::.common_is_calc_format(data)
    if(!is_calc) {
        myClim:::.prep_check_datetime_step_unprocessed(data, stop)
    }

    call_dendro_function <- function(item) {
        .calc_add_sensor_to_item(item, dendro_sensor, myClim:::.model_const_SENSOR_dendro_l_um, output_sensor,
                                 myClim:::.model_const_PHYSICAL_TOMSTdendro,
                                 .calc_get_dendro_l_um)
    }

    locality_function <- function(locality) {
        if(!(is.null(localities) || locality$metadata@locality_id %in% localities)) {
            return(locality)
        }
        if(is_calc) {
            return(call_dendro_function(locality))
        }
        locality$loggers <- purrr::map(locality$loggers, call_dendro_function)
        return(locality)
    }
    out_localities <- purrr::map(myClim:::.common_get_localities(data), locality_function)
    myClim:::.common_set_localities(data, out_localities)
}

.calc_get_dendro_l_um <- function(item, sensor_name) {
    min_raw_value <- mc_data_sensors[[myClim:::.model_const_SENSOR_DEND_TOMSTdendro]]@min_value
    max_raw_value <- mc_data_sensors[[myClim:::.model_const_SENSOR_DEND_TOMSTdendro]]@max_value
    um_range <- myClim:::.model_const_TOMST_DENDROMETER_UM_RANGE
    (item$sensors[[sensor_name]]$values - min_raw_value) * (um_range / (max_raw_value - min_raw_value))
}

#' Calculate vapor pressure deficit (in kPa)
#'
#' @description
#' Function calculate vapor pressure deficit (in kPa) from temperature and relative humidity readings using equation
#' from the CR-5 Users Manual 2009–12 from Buck Research modified from Buck (1981) and adapted by Jones, 2013 (eq. 5.15)
#' Elevation to pressure conversion function uses eq. 3.7 from Campbell G.S. & Norman J.M. (1998).
#'
#' @param data myClim object in cleaned Prep-format or Calc-formt see [myClim::mc_agg()] and [myClim-package]
#' @param temp_sensor name of temperature sensor
#'
#' Temperature sensor must be in T_C physical.
#' @param rh_sensor name relative humidity sensor
#'
#' Temperature sensor must be in RH_perc physical.
#' @param output_sensor name of new snow sensor (default "VPD")
#' @param altitude value in meters (default 0)
#' @param metadata_altitude if TRUE then altitude from metadata of locality is used (default TRUE)
#' @param localities list of locality_ids for calculation; if NULL then all (default NULL)
#' @return myClim object same as input but with added VPD sensor
#' @export
#' @references
#' Jones H.G. (2014) Plants and Microclimate, Third Edit. Cambridge University Press, Cambridge Buck A.L. (1981)
#' New equations for computing vapor pressure and enhancment factor. Journal of Applied Meteorology 20: 1527–1532.
#' Campbell G.S. & Norman J.M. (1998). An Introduction to Environmental Biophysics, Springer New York, New York, NY
#'
#' @examples
#' calc_data <- mc_calc_vpd(mc_data_example_calc, "HOBO_T_C", "HOBO_RH", localities="A2E32")
mc_calc_vpd <- function(data, temp_sensor, rh_sensor,
                        output_sensor="VPD", altitude=0,
                        metadata_altitude=TRUE, localities=NULL) {
    is_calc <- myClim:::.common_is_calc_format(data)
    if(!is_calc) {
        myClim:::.prep_check_datetime_step_unprocessed(data, stop)
    }

    if(is_calc && lubridate::period(data$metadata@step_text) >= lubridate::days(1)) {
        warning(.calc_const_MESSAGE_VPD_AGGREGATED)
    }

    call_vpd_function <- function(item, altitude) {
        .calc_add_vpd_to_item(item, temp_sensor, rh_sensor, output_sensor, altitude)
    }

    locality_function <- function(locality) {
        if(!(is.null(localities) || locality$metadata@locality_id %in% localities)) {
            return(locality)
        }
        local_altitude <- altitude
        if(metadata_altitude && !is.na(locality$metadata@altitude)) {
            local_altitude <- locality$metadata@altitude
        }
        if(is_calc) {
            return(call_vpd_function(locality, local_altitude))
        }
        locality$loggers <- purrr::map2(locality$loggers, local_altitude, call_vpd_function)
        return(locality)
    }
    out_localities <- purrr::map(myClim:::.common_get_localities(data), locality_function)
    myClim:::.common_set_localities(data, out_localities)
}

.calc_add_vpd_to_item <- function(item, temp_sensor, rh_sensor, output_sensor, altitude) {
    skip <- .calc_vpd_check_sensors_get_skip(item, temp_sensor, rh_sensor, output_sensor)
    if(skip) {
        return(item)
    }

    T <- item$sensors[[temp_sensor]]$values
    RH <- item$sensors[[rh_sensor]]$values
    a <- 0.61121
    b <- 18.678 - (T / 234.5)
    c <- 257.14
    P <- 101300 * exp(- altitude / 8200)
    f <- 1.00072 + (10e-7 * P * (0.032 + 5.9 * 10e-6 * T^2)) #enhancement factor
    values <- f * a * exp(b * T / (c + T)) * (1 - RH / 100)

    height <- item$sensors[[rh_sensor]]$metadata@height
    item$sensors[[output_sensor]] <- myClim:::.common_get_new_sensor(myClim:::.model_const_SENSOR_VPD, output_sensor,
                                                                     height=height, values=values)
    return(item)
}

.calc_vpd_check_sensors_get_skip <- function(item, temp_sensor, rh_sensor, output_sensor){
    if(!.calc_check_sensor_in_item(item, temp_sensor)){
        return(TRUE)
    }
    if(!myClim:::.model_is_physical_T_C(item$sensors[[temp_sensor]]$metadata)){
        .calc_wrong_physical_error_function(temp_sensor, myClim:::.model_const_PHYSICAL_T_C)
    }
    if(!.calc_check_sensor_in_item(item, rh_sensor)){
        return(TRUE)
    }
    if(!myClim:::.model_is_physical(item$sensors[[rh_sensor]]$metadata, myClim:::.model_const_PHYSICAL_RH_perc)){
        .calc_wrong_physical_error_function(temp_sensor, myClim:::.model_const_PHYSICAL_RH_perc)
    }
    .calc_warn_if_overwriting(item, output_sensor)
    return(FALSE)
}
