.calc_MESSAGE_LOCALITY_NOT_CONTAINS_SENSOR <- "Locality {locality$metadata@locality_id} doesn't contains sensor {sensor}. It is skipped."
.calc_MESSAGE_STEP_LONGER_DAY <- "Step {data$metadata@step_text} in data is too long. Maximal step is day."
.calc_MESSAGE_WRONG_PHYSICAL_UNIT <- "Physical unit of {sensor_name} isn't {unit_name}."
.calc_MESSAGE_OVERWRITE_SENSOR <- "Sensor {output_sensor} exists in locality {locality$metadata@locality_id}. It will be overwritten."
.calc_MESSAGE_SENSOR_NOT_EXISTS_IN_LOCALITIES <- "Sensor doesn't exist in any locality."
.calc_MESSAGE_UNKNONW_SIOLTYPE <- "Soiltype {soiltype_value} is unknown."

#' Snow detection from temperature
#'
#' @description
#' This function creates new virtual sensor on locality within myClim data object. Function return TRUE/FALSE vector in original time step for Snow/non-snow  events.  
#'
#' @details
#' Function was designed to estimate snow presence from temperature in situation when temperature sensor is covered by snow. Snow detection algorithm combines daily range `dr`of temperature with the maximal daily temperature `tmax`. I.e in default settings TRUE (snow presence) is returned when daily temperature range is lower than 2°C and daily maximal temperature is lower than 0.5 °C.
#' 
#' TRUE/FALSE = Snow/non-snow information is returned in original time step (e.g. 15 min, 1 h...) despite function operate with daily temperature range and maximum. Because of dependency on daily temperatures, the longest time step for snow detection allowed is day. 
#'
#' @param data myClim object in Calc-format. See [myClim::mc_agg()] and [myClim-package]
#' @param sensor name of temperature sensor used for snow estimation. (e.g. TMS_T2)
#' @param output_sensor name of output snow sensor (default "snow")
#' @param localities list of locality_ids where snow sill be calculated; if NULL then all (default NULL)
#' @param range maximal temperature range threshold for snow-covered sensor
#' @param tmax maximal temperature threshold for snow-covered sensor
#' @param days number of days to be used for moving-window for snow detection algorithm
#' @return The new myClim data object, identical as input but with added snow sensor. Time step is not modified.
#' @export
#' @examples
#' snow <- mc_calc_snow(example_tomst_data1, "TMS_T2", output_sensor="TMS_T2_snow")
mc_calc_snow <- function(data, sensor, output_sensor="snow", localities=NULL, range=2, tmax=0.5,days = 1) {
    myClim:::.common_stop_if_not_calc_format(data)
    .calc_check_maximal_day_step(data)
    locality_function <- function(locality) {
        if(!(is.null(localities) || locality$metadata@locality_id %in% localities)) {
            return(locality)
        }
        .calc_add_sensor_to_locality(locality, sensor, myClim:::.model_const_SENSOR_snow_bool, output_sensor,
                                     myClim:::.model_const_PHYSICAL_T_C,
                                     .calc_snow_values_function, range=range, tmax=tmax,days=days)
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

.calc_snow_values_function <- function(locality, sensor_name, range, tmax, days) {
    per <- 3600*24*days
    day_max_temp_prev <- runner::runner(locality$sensors[[sensor_name]]$values, k=per, idx=locality$datetime, f=function(x) if(length(x) == 0) NA else max(x), na_pad=TRUE)
    day_range_temp_prev <- runner::runner(locality$sensors[[sensor_name]]$values, k=per, idx=locality$datetime, f=function(x) if(length(x) == 0) NA else max(x) - min(x), na_pad=TRUE)

    day_max_temp_next <- runner::runner(locality$sensors[[sensor_name]]$values, k=per, lag = -per+1, idx=locality$datetime, f=function(x) if(length(x) == 0) NA else max(x) - min(x), na_pad=TRUE)
,lag = -, idx=locality$datetime, f=function(x) if(length(x) == 0) NA else max(x), na_pad=TRUE)
    day_range_temp_next <- runner::runner(locality$sensors[[sensor_name]]$values, k=per,lag = -per+1, idx=locality$datetime, f=function(x) if(length(x) == 0) NA else max(x) - min(x), na_pad=TRUE)

    (day_range_temp_prev < range) & (day_max_temp_prev < tmax) | (day_range_temp_next < range) & (day_max_temp_next < tmax)
}

.calc_add_sensor_to_locality <- function(locality, sensor_name, output_sensor_id, output_sensor_name, sensor_physical=NULL, values_function, ...) {
    if(!.calc_check_sensor_in_locality(locality, sensor_name)){
        return(locality)
    }
    if(!is.null(sensor_physical) && !myClim:::.model_is_physical(locality$sensors[[sensor_name]]$metadata, sensor_physical)){
        .calc_wrong_physical_error_function(sensor_name, sensor_physical)
    }
    .calc_warn_if_overwriting(locality, output_sensor_name)
    values <- values_function(locality, sensor_name, ...)
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
#' @param data myClim object in calculation format (see [myClim::mc_agg()]) with TRUE/FALSE snow sensor see [myClim::mc_calc_snow()]
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
#' @param data myClim object in Calc-format see [myClim::mc_agg()] and [myClim-package]
#' @param moist_sensor name of soil moisture sensor to be converted from raw to volumetric (default "TMS_TMSmoisture") see `names(mc_data_sensors)`
#' @param temp_sensor name of soil temperature sensor (default "TMS_T1") see `names(mc_data_sensors)`
#'
#' Temperature sensor must be in T physical.
#' @param output_sensor name of new snow sensor (default "vwc_moisture")
#' @param soiltype value from mc_data_vwc_parameters in column soiltype (default "universal")
#'
#' Parameters a, b and c are used in calculation.
#' @param localities list of locality_ids for calculation; if NULL then all (default NULL)
#' @param ref_t (default 24)
#' @param acor_t (default 1.91132689118083) correction temperature while sensor on the air see [myClim::mc_calib_moisture()]
#' @param wcor_t (default 0.64108) correction temperature while sensor in the water [myClim::mc_calib_moisture()]
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
    data_function <- function(cor_factor, cor_slope, data){
        is_calibrated <- !is.na(cor_factor) && !is.na(cor_slope)
        .calc_get_vwc_values(raw_values = data$raw,
                             temp_values = data$temp,
                             cal_cor_factor = if(is_calibrated) cor_factor else 0,
                             cal_cor_slope = if(is_calibrated) cor_slope else 0,
                             a = soil_row$a, b = soil_row$b, c = soil_row$c,
                             ref_t = ref_t, acor_t = acor_t, wcor_t = wcor_t)
    }
    values <- purrr::pmap(dplyr::select(input_data, cor_factor, cor_slope, data), data_function)
    is_calibrated <- nrow(calibration) > 0
    locality$sensors[[output_sensor]] <- myClim:::.common_get_new_sensor(myClim:::.model_const_SENSOR_moisture, output_sensor,
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
    if(!myClim:::.model_is_physical_T_C(locality$sensors[[temp_sensor]]$metadata)){
        .calc_wrong_physical_error_function(temp_sensor, "T")
    }
    .calc_warn_if_overwriting(locality, output_sensor)
    return(FALSE)
}

.calc_get_vwc_values <- function(raw_values, temp_values, cal_cor_factor, cal_cor_slope,
                                 a, b, c, ref_t, acor_t, wcor_t) {
    vwc <- a * raw_values^2 + b * raw_values + c
    dcor_t <- wcor_t - acor_t
    tcor <- raw_values + (temp_values - ref_t) * (acor_t + dcor_t * vwc)
    vwc_cor <- a * (tcor + cal_cor_factor + cal_cor_slope * vwc)^2 + b * (tcor + cal_cor_factor + cal_cor_slope * vwc) + c
    pmin(pmax(vwc_cor, 0), 1)
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
#' @param data myClim object in Calc-format see [myClim::mc_agg()] and [myClim-package]
#' @param sensor name of temperature sensor used fot GDD calculation e.g. TMS_T3 see `names(mc_data_sensors)`
#' @param output_prefix name prefix of new GDD sensor (default "GDD")
#'
#' name of output sensor consists of output_prefix and value t_base e.g. GDD_5
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
    myClim:::.common_stop_if_not_calc_format(data)
    .calc_check_maximal_day_step(data)

    output_sensor <- stringr::str_glue("{output_prefix}{t_base}")
    step_part_day <- data$metadata@step / (24 * 60)

    locality_function <- function(locality) {
        if(!(is.null(localities) || locality$metadata@locality_id %in% localities)) {
            return(locality)
        }
        .calc_add_sensor_to_locality(locality, sensor, output_sensor_id, output_sensor,
                                     myClim:::.model_const_PHYSICAL_T_C,
                                     values_function, t_base=t_base, step_part_day=step_part_day)
    }
    data$localities <- purrr::map(data$localities, locality_function)
    data
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
#' Maximal allowed step length for FDD calculation is day and shorter. Function creates new virtual sensor with the same time step as input data. I. e. when the time step is shorter than a day than freezing degree day is divided into smaller time step but still summing the day. For shorter intervals than the day the FDD value is the contribution of the interval to the freezing degree day.
#' Be careful while aggregating freezing degree days to longer periods see [myClim::mc_agg()] only meaningful aggregation function is `sum`, but user is allowed to apply anything.
#'
#' @param data myClim object in Calc-format see [myClim::mc_agg()] and [myClim-package]
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
#' @param data myClim object in Calc-format see [myClim::mc_agg()] and [myClim-package]
#' @param sensors names of sensors where to calculate cumulative sum
#' @param output_suffix name suffix for new names (default "_cumsum") e.g. TMS_T3_cumsum
#' @param localities list of locality_ids for calculation; if NULL then all (default NULL)
#' @return The same myClim object as input but with added cumsum sensors.
#' @export
#' @examples
#' cumsum_data <- mc_calc_cumsum(mc_data_example_calc, c("TMS_T1", "TMS_T2"))
mc_calc_cumsum <- function(data, sensors, output_suffix="_cumsum", localities=NULL) {
    myClim:::.common_stop_if_not_calc_format(data)

    values_function <- function(locality, sensor_name) {
        cumsum(locality$sensors[[sensor_name]]$values)
    }

    sensor_function <- function(locality, sensor_name) {
        if(!.calc_check_sensor_in_locality(locality, sensor_name)){
            return(locality)
        }
        origin_sensor <- locality$sensors[[sensor_name]]
        output_sensor_id <- origin_sensor$metadata@sensor_id
        output_sensor_name <- stringr::str_glue("{origin_sensor$metadata@name}{output_suffix}")
        locality <- .calc_add_sensor_to_locality(locality, sensor_name, output_sensor_id, output_sensor_name,
                                                 values_function = values_function)
        if(is.logical(origin_sensor$values) && !is.logical(locality$sensors[[output_sensor_name]]$values)) {
            locality$sensors[[output_sensor_name]]$metadata@sensor_id <- myClim:::.model_const_SENSOR_integer
        }
        locality
    }

    locality_function <- function(locality) {
        if(!(is.null(localities) || locality$metadata@locality_id %in% localities)) {
            return(locality)
        }

        for (sensor_name in sensors) {
            locality <- sensor_function(locality, sensor_name)
        }

        locality
    }

    data$localities <- purrr::map(data$localities, locality_function)
    data
}

#' Converting raw values of TOMST dendrometer to micrometers
#'
#' @description
#' This function convert differences of trunk radius fram raw TOMST units to micrometers.
#'
#' @param data myClim object in Calc-format see [myClim::mc_agg()] and [myClim-package]
#' @param dendro_sensor name of radius difference sensor to be converted from raw to micrometers (default "DEND_TOMST_r_delta") see `names(mc_data_sensors)`
#' @param output_sensor name of new snow sensor (default "r_delta")
#' @param localities list of locality_ids for calculation; if NULL then all (default NULL)
#' @return myClim object same as input but with added r_delta sensor
#' @export
#' @examples
#' calc_data <- mc_calc_tomst_dendro(mc_data_example_calc, localities="A1E05")
mc_calc_tomst_dendro <- function(data, dendro_sensor=myClim:::.model_const_SENSOR_DEND_TOMST_r_delta,
                        output_sensor=myClim:::.model_const_SENSOR_r_delta,
                        localities=NULL) {
    myClim:::.common_stop_if_not_calc_format(data)

    locality_function <- function(locality) {
        if(!(is.null(localities) || locality$metadata@locality_id %in% localities)) {
            return(locality)
        }
        .calc_add_r_delta_to_locality(locality, dendro_sensor, output_sensor)
    }
    data$localities <- purrr::map(data$localities, locality_function)
    data
}

.calc_add_r_delta_to_locality <- function(locality, dendro_sensor, output_sensor) {
    if(!.calc_check_sensor_in_locality(locality, dendro_sensor)){
        return(locality)
    }
    min_raw_value <- mc_data_sensors[[myClim:::.model_const_SENSOR_DEND_TOMST_r_delta]]@min_value
    max_raw_value <- mc_data_sensors[[myClim:::.model_const_SENSOR_DEND_TOMST_r_delta]]@max_value
    um_range <- myClim:::.model_const_TOMST_DENDROMETER_UM_RANGE
    values <- (locality$sensors[[dendro_sensor]]$values - min_raw_value) * (um_range / (max_raw_value - min_raw_value))
    locality$sensors[[output_sensor]] <- myClim:::.common_get_new_sensor(myClim:::.model_const_SENSOR_r_delta, output_sensor,
                                                                         values)
    return(locality)
}
