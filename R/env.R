.env_const_MIN_PERCENTILE <- 5
.env_const_MAX_PERCENTILE <- 95
.env_const_GDD <- "GDD"
.env_const_FDD <- "FDD"
.env_const_FROSTDAYS <- "frostdays"
.env_const_SD <- "sd"
.env_const_T_PREFIX <- "T."
.env_const_VWC_PREFIX <- "VWC."
.env_const_VPD_PREFIX <- "VPD."

.env_const_MESSAGE_ANY_VWC <- "There arn't any moisture sensors in volumetric water content units."
.env_const_MESSAGE_ANY_VPD <- "There arn't any VPD sensors."

#' Standardised myClim temperature variables
#'
#' @description
#' The wrapper function returning 7 standardised ecologically relevant myClim variables
#' derived from temperature.   
#'
#' @details
#' This function was designed for time-series of step shorter than one 
#' day and will not work with coarser data. It automatically use all 
#' available sensors in myClim object and returns all possible variables based
#' on sensor type and measurement height/depth. In contrast with other myClim functions
#' returning myClim objects this wrapper function return long table.
#' The mc_env_temp function first aggregates time-series to daily time-step 
#' and then aggregates to the final time-step set in `period` parameter. 
#' Because freezing and growing degree days are always aggregated with sum function,
#' these two variables are not first aggregated to the daily time-steps. 
#' Variables are named based on sensor name, height, and function e.g., 
#' (T.air_15_cm.max95p, T.air_15_cm.drange)
#' 
#' Standardised myClim temperature variables:
#'   - min5p: Minimum temperature = 5th percentile of daily minimum temperatures
#'   - mean: Mean temperature = mean of daily mean temperatures
#'   - max95p: Maximum temperature = 95th percentile of daily maximum temperatures
#'   - drange: Temperature range = mean of daily temperature range (i.e., difference of daily minima and maxima)
#'   - GDD{5}: Growing degree days = sum of growing degree days above defined base temperature (default 5°C) `gdd_t_base`
#'   - FDD{0}: Freezing degree days = sum of freezing degree days below defined base temperature (default 0°C) `fdd_t_base`
#'   - frostdays: Frost days = number of days with frost (daily minimum < 0°C) `fdd_t_base`
#'   
#' @template param_myClim_object_cleaned
#' @template params_env_agg
#' @param gdd_t_base base temperature for Growing Degree Days [myClim::mc_calc_gdd()] (default 5)
#' @param fdd_t_base base temperature for Freezing Degree Days [myClim::mc_calc_fdd()] (default 0)
#' @return table in long format with standardised myClim variables
#' @export
#' @examples
#' data <- mc_prep_crop(mc_data_example_clean, lubridate::ymd_h("2020-11-01 00"),
#'                      lubridate::ymd_h("2021-02-01 00"), end_included = FALSE)
#' mc_env_temp(data, "month")
mc_env_temp <- function(data, period, use_utc=TRUE, custom_start=NULL, custom_end=NULL, min_coverage=1, gdd_t_base=5, fdd_t_base=0) {
    is_agg <- .common_is_agg_format(data)
    if(!is_agg) {
        data <- mc_agg(data)
    }
    prepared <- .env_temp_calc_fdd_gdd_and_rename(data, gdd_t_base, fdd_t_base)
    table <- .env_temp_get_table(prepared$temp_sensors, gdd_t_base, fdd_t_base)
    day_data <- .env_get_day_agg(prepared$data, table, use_utc)
    custom_functions <- list(frostdays=function(values){sum(values < fdd_t_base)})
    result_data <- .env_get_result_agg(day_data, table, period, TRUE, custom_start, custom_end, min_coverage, custom_functions)
    mc_reshape_long(result_data)
}

.env_temp_calc_fdd_gdd_and_rename <- function(data, gdd_t_base, fdd_t_base) {
    temp_sensors <- .env_get_sensors_by_physical_or_id(data, physical=.model_const_PHYSICAL_T_C)
    result <- list(temp_sensors=character(), other_sensors=character(), data=data)
    for(temp_sensor in names(temp_sensors$localities)) {
        localities <- temp_sensors$localities[[temp_sensor]]
        result$data <- mc_calc_gdd(result$data, temp_sensor, output_prefix=.env_const_GDD, t_base=gdd_t_base, localities=localities)
        result$data <- mc_calc_fdd(result$data, temp_sensor, output_prefix=.env_const_FDD, t_base=fdd_t_base, localities=localities)
        height_name <- .agg_get_height_name(temp_sensor, temp_sensors$heights[[temp_sensor]])
        new_name <- paste0(.env_const_T_PREFIX, height_name)
        result$temp_sensors <- append(result$temp_sensors, new_name)
        rename_rules <- list()
        rename_rules[[temp_sensor]] <- new_name
        gdd_name <- .calc_get_name_with_base(.env_const_GDD, gdd_t_base)
        new_gdd_name <- .env_get_calc_sensor_name(gdd_name, new_name)
        rename_rules[[gdd_name]] <- new_gdd_name
        fdd_name <- .calc_get_name_with_base(.env_const_FDD, fdd_t_base)
        new_fdd_name <- .env_get_calc_sensor_name(fdd_name, new_name)
        rename_rules[[fdd_name]] <- new_fdd_name
        result$data <- mc_prep_meta_sensor(result$data, rename_rules, param_name="name")
    }
    return(result)
}

.env_get_sensors_by_physical_or_id <- function(data, physical=NULL, sensor_id=NULL) {
    result <- new.env()
    result$localities <- new.env()
    result$heights <- new.env()
    sensor_function <- function(locality_id, sensor) {
        if(!is.null(physical) && !.model_is_physical(sensor$metadata, physical)) {
            return()
        }
        if(!is.null(sensor_id) && sensor$metadata@sensor_id != sensor_id) {
            return()
        }
        if(!exists(sensor$metadata@name, envir = result$localities)) {
            result$localities[[sensor$metadata@name]] <- character()
        }
        result$localities[[sensor$metadata@name]] <- append(result$localities[[sensor$metadata@name]], locality_id)
        if(is.na(sensor$metadata@height)) {
            result$heights[[sensor$metadata@name]] <- NA_character_
        } else if(!exists(sensor$metadata@name, envir = result$heights)) {
            result$heights[[sensor$metadata@name]] <- sensor$metadata@height
        } else if(is.na(result$heights[[sensor$metadata@name]])) {
            return()
        } else if (result$heights[[sensor$metadata@name]] != sensor$metadata@height) {
            result$heights[[sensor$metadata@name]] <- NA_character_
        }
    }
    locality_function <- function(locality) {
        purrr::walk2(locality$metadata@locality_id, locality$sensors, sensor_function)
    }
    purrr::walk(data$localities, locality_function)
    return(result)
}

.env_get_calc_sensor_name <- function(function_name, height_name) {
    stringr::str_glue("{function_name}.{height_name}")
}

.env_temp_get_table <- function(temp_sensors, gdd_t_base, fdd_t_base) {
    sensor_function <- function(sensor) {
        count_rows <- 7
        gdd_with_base <- .calc_get_name_with_base(.env_const_GDD, gdd_t_base)
        fdd_with_base <- .calc_get_name_with_base(.env_const_FDD, fdd_t_base)
        gdd_name <- .env_get_calc_sensor_name(gdd_with_base, sensor)
        fdd_name <- .env_get_calc_sensor_name(fdd_with_base, sensor)
        sensor_base <- c(rep(sensor, count_rows - 2), gdd_name, fdd_name)
        day_fun <- c(
            .agg_const_FUNCTION_MIN,
            .agg_const_FUNCTION_MIN,
            .agg_const_FUNCTION_MEAN,
            .agg_const_FUNCTION_MAX,
            .agg_const_FUNCTION_RANGE,
            .agg_const_FUNCTION_SUM,
            .agg_const_FUNCTION_SUM)
        sensor_day <- purrr::map2_chr(sensor_base, day_fun, ~ .agg_get_aggregated_sensor_name(.x, .y))
        period_fun <- c(
            .agg_const_FUNCTION_PERCENTILE,
            .env_const_FROSTDAYS,
            .agg_const_FUNCTION_MEAN,
            .agg_const_FUNCTION_PERCENTILE,
            .agg_const_FUNCTION_MEAN,
            .agg_const_FUNCTION_SUM,
            .agg_const_FUNCTION_SUM)
        output_sensor_function <- function(sensor_day, fun) {
            if(fun == .agg_const_FUNCTION_PERCENTILE) {
                if(stringr::str_ends(sensor_day, .agg_const_FUNCTION_MIN)) {
                    fun <- .agg_get_percentile_function_name(.env_const_MIN_PERCENTILE)
                } else {
                    fun <- .agg_get_percentile_function_name(.env_const_MAX_PERCENTILE)
                }
            }
            .agg_get_aggregated_sensor_name(sensor_day, fun)
        }
        sensor_period <- purrr::map2_chr(sensor_day, period_fun, output_sensor_function)
        result_texts <- c("min5p", .env_const_FROSTDAYS, "mean", "max95p", "drange", gdd_with_base, fdd_with_base)
        height <- substring(sensor, nchar(.env_const_T_PREFIX) + 1)
        result_names <- stringr::str_glue("{.env_const_T_PREFIX}{height}.{result_texts}")
        list(sensor_base=sensor_base,
             day_fun=day_fun,
             sensor_prep=sensor_day,
             period_fun=period_fun,
             sensor_period=sensor_period,
             result_name=result_names)
    }

    purrr::map_dfr(temp_sensors, sensor_function)
}

.env_get_day_agg <- function(data, table, use_utc) {
    agg_table <- dplyr::distinct(dplyr::select(table, "sensor_base", "day_fun"))
    agg_table <- dplyr::group_by(agg_table, .data$sensor_base)
    group_function <- function(fun_table, group) {
        fun_table$day_fun
    }
    fun <- dplyr::group_map(agg_table, group_function)
    names(fun) <- dplyr::group_keys(agg_table)$sensor_base
    mc_agg(data, fun, "day", use_utc=use_utc)
}

.env_get_result_agg <- function(day_data, table, period, use_utc, custom_start, custom_end, min_coverage, custom_functions=NULL,
                                percentiles=c(.env_const_MIN_PERCENTILE, .env_const_MAX_PERCENTILE)) {
    agg_table <- dplyr::select(table, "sensor_prep", "period_fun")
    agg_table <- dplyr::group_by(agg_table, .data$sensor_prep)

    group_function <- function(fun_table, group) {
        fun_table$period_fun
    }
    fun <- dplyr::group_map(agg_table, group_function)
    names(fun) <- dplyr::group_keys(agg_table)$sensor_prep
    fun <- purrr::map(fun, ~ unique(.x))
    result_data <- mc_agg(day_data, fun, period, custom_start=custom_start, custom_end=custom_end,
                          percentiles=percentiles, custom_functions=custom_functions, min_coverage=min_coverage,
                          use_utc=use_utc)
    rename_rules <- as.list(table$result_name)
    names(rename_rules) <- table$sensor_period
    result_data <- mc_prep_meta_sensor(result_data, rename_rules, param_name="name")
    mc_filter(result_data, sensors=table$result_name)
}

#' Standardised myClim soil moisture variables
#'
#' @description
#' The wrapper function returning 4 standardised ecologically relevant myClim variables
#' derived from soil moisture. The mc_env_moist function needs time-series of 
#' volumetric water content (VWC) measurements as input. Therefore, non-VWC soil
#' moisture measurements must be first converted to VWC. 
#' For Tomst loggers see [myClim::mc_calc_vwc()]
#'   
#'
#' @details
#' This function was designed for time-series of step shorter than one 
#' day and will not work with coarser data. In contrast with other myClim functions
#' returning myClim objects this wrapper function return long table.
#' Variables are named based on sensor name, height, and function e.g., 
#' (VWC.soil_0_15_cm.5p, VWC.soil_0_15_cm.mean)
#' 
#' Standardised myClim soil moisture variables:
#'   - VWC.5p: Minimum soil moisture = 5th percentile of VWC values
#'   - VWC.mean: Mean soil moisture = mean of VWC values
#'   - VWC.95p: Maximum soil moisture = 95th percentile of VWC values
#'   - VWC.sd: Standard deviation of VWC measurements
#'
#' @template param_myClim_object_cleaned
#' @template params_env_agg
#' @return table in long format with standardised myClim variables
#' @export
#' @examples
#' data <- mc_prep_crop(mc_data_example_agg, lubridate::ymd_h("2020-11-01 00"),
#'                      lubridate::ymd_h("2021-02-01 00"), end_included = FALSE)
#' data <- mc_calc_vwc(data, localities=c("A2E32", "A6W79"))
#' mc_env_moist(data, "month")
mc_env_moist <- function(data, period, use_utc=TRUE, custom_start=NULL, custom_end=NULL, min_coverage=1) {
    is_agg <- .common_is_agg_format(data)
    if(!is_agg) {
        data <- mc_agg(data)
    }
    prepared <- .env_moist_rename_sensors(data)
    table <- .env_moist_get_table(prepared$moist_sensors)
    if(nrow(table) == 0) {
        stop(.env_const_MESSAGE_ANY_VWC)
    }
    result_data <- .env_get_result_agg(prepared$data, table, period, use_utc, custom_start, custom_end, min_coverage, list(sd=sd))
    mc_reshape_long(result_data)
}

.env_moist_rename_sensors <- function(data) {
    moist_sensors <- .env_get_sensors_by_physical_or_id(data, physical=.model_const_PHYSICAL_moisture)
    result <- list(moist_sensors=character(), data=data)
    for(moist_sensor in names(moist_sensors$localities)) {
        height_name <- .agg_get_height_name(moist_sensor, moist_sensors$heights[[moist_sensor]])
        new_name <- paste0(.env_const_VWC_PREFIX, height_name)
        result$moist_sensors <- append(result$moist_sensors, new_name)
        rename_rules <- list()
        rename_rules[[moist_sensor]] <- new_name
        result$data <- mc_prep_meta_sensor(result$data, rename_rules, param_name="name")
    }
    return(result)
}

.env_moist_get_table <- function(moist_sensors) {
    sensor_function <- function(sensor) {
        count_rows <- 4
        sensor_base <- rep(sensor, count_rows)
        period_fun <- c(
            .agg_const_FUNCTION_PERCENTILE,
            .agg_const_FUNCTION_MEAN,
            .agg_const_FUNCTION_PERCENTILE,
            .env_const_SD)
        output_sensor_function <- function(sensor, fun, percentile) {
            if(fun == .agg_const_FUNCTION_PERCENTILE) {
                fun <- .agg_get_percentile_function_name(percentile)
            }
            .agg_get_aggregated_sensor_name(sensor, fun)
        }
        sensor_period <- purrr::pmap_chr(list(sensor=sensor_base,
                                              fun=period_fun,
                                              percentile=c(.env_const_MIN_PERCENTILE, NA, .env_const_MAX_PERCENTILE, NA)),
                                         output_sensor_function)
        result_texts <- c("5p", "mean", "95p", .env_const_SD)
        height <- substring(sensor, nchar(.env_const_VWC_PREFIX) + 1)
        result_names <- stringr::str_glue("{.env_const_VWC_PREFIX}{height}.{result_texts}")
        list(sensor_prep=sensor_base,
             period_fun=period_fun,
             sensor_period=sensor_period,
             result_name=result_names)
    }

    purrr::map_dfr(moist_sensors, sensor_function)
}

#' Standardised myClim vapor pressure deficit variables
#'
#' @description
#' The wrapper function returning 2 standardised ecologically relevant myClim variables
#' derived from vapor pressure deficit. The mc_env_vpd function needs time-series of 
#' vapor pressure deficit measurements as input. Therefore, VPD must be first calculated
#' from temperature and air humidity sensors see [myClim::mc_calc_vpd()]
#'   
#'
#' @details
#' This function was designed for time-series of step shorter than one 
#' day and will not work with coarser data. The mc_env_vpd function 
#' first aggregates time-series to daily time-step 
#' and then aggregates to the final time-step set in `period` parameter. 
#' In contrast with other myClim functions
#' returning myClim objects this wrapper function return long table.
#' Variables are named based on sensor name, height, and function e.g., 
#' (VPD.air_150_cm.mean, VPD.air_150_cm.max95p)
#' 
#' Standardised myClim vapor pressure deficit variables:
#'   - VPD.mean: Mean vapor pressure deficit = mean of daily mean VPD
#'   - VPD.max95p: Maximum vapor pressure deficit = 95th percentile of daily maximum VPD	
#'
#' @template param_myClim_object_cleaned
#' @template params_env_agg
#' @return table in long format with standardised myClim variables
#' @export
mc_env_vpd <- function(data, period, use_utc=TRUE, custom_start=NULL, custom_end=NULL, min_coverage=1) {
    is_agg <- .common_is_agg_format(data)
    if(!is_agg) {
        data <- mc_agg(data)
    }
    prepared <- .env_vpd_rename_sensors(data)
    table <- .env_vpd_get_table(prepared$vpd_sensors)
    if(nrow(table) == 0) {
        stop(.env_const_MESSAGE_ANY_VPD)
    }
    day_data <- .env_get_day_agg(prepared$data, table, use_utc)
    result_data <- .env_get_result_agg(day_data, table, period, TRUE, custom_start, custom_end,
                                       min_coverage, percentiles=.env_const_MAX_PERCENTILE)
    mc_reshape_long(result_data)
}

.env_vpd_rename_sensors <- function(data) {
    vpd_sensors <- .env_get_sensors_by_physical_or_id(data, sensor_id=.model_const_SENSOR_VPD)
    result <- list(vpd_sensors=character(), data=data)
    for(vpd_sensor in names(vpd_sensors$localities)) {
        height_name <- .agg_get_height_name(vpd_sensor, vpd_sensors$heights[[vpd_sensor]])
        new_name <- paste0(.env_const_VPD_PREFIX, height_name)
        result$vpd_sensors <- append(result$vpd_sensors, new_name)
        rename_rules <- list()
        rename_rules[[vpd_sensor]] <- new_name
        result$data <- mc_prep_meta_sensor(result$data, rename_rules, param_name="name")
    }
    return(result)
}

.env_vpd_get_table <- function(vpd_sensors) {
    sensor_function <- function(sensor) {
        count_rows <- 2
        sensor_base <- rep(sensor, count_rows)
        day_fun <- c(
            .agg_const_FUNCTION_MEAN,
            .agg_const_FUNCTION_MAX)
        sensor_day <- purrr::map2_chr(sensor_base, day_fun, ~ .agg_get_aggregated_sensor_name(.x, .y))
        period_fun <- c(
            .agg_const_FUNCTION_MEAN,
            .agg_const_FUNCTION_PERCENTILE)
        output_sensor_function <- function(sensor, fun) {
            if(fun == .agg_const_FUNCTION_PERCENTILE) {
                fun <- .agg_get_percentile_function_name(.env_const_MAX_PERCENTILE)
            }
            .agg_get_aggregated_sensor_name(sensor, fun)
        }
        sensor_period <- purrr::map2_chr(sensor_day, period_fun, output_sensor_function)
        result_texts <- c("mean", "max95p")
        height <- substring(sensor, nchar(.env_const_VPD_PREFIX) + 1)
        result_names <- stringr::str_glue("{.env_const_VPD_PREFIX}{height}.{result_texts}")
        list(sensor_base=sensor_base,
             day_fun=day_fun,
             sensor_prep=sensor_day,
             period_fun=period_fun,
             sensor_period=sensor_period,
             result_name=result_names)
    }

    purrr::map_dfr(vpd_sensors, sensor_function)
}

