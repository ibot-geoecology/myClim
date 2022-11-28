.env_const_MIN_PERCENTILE <- 5
.env_const_MAX_PERCENTILE <- 95
.env_const_GDD <- "GDD"
.env_const_FDD <- "FDD"
.env_const_FROSTDAYS <- "frostdays"
.env_const_SD <- "sd"
.env_const_T_PREFIX <- "T."
.env_const_VWC_PREFIX <- "VWC."
.env_const_VPD_PREFIX <- "VPD."

.env_const_MESSAGE_ANY_VWC <- "There isn't any moisture sensor in volumetric water content units."
.env_const_MESSAGE_ANY_VPD <- "There isn't any VPD sensor."

#' Tempereature environment variables
#'
#' @description
#' Function ...
#'
#' @details
#' - used all temperature sensors
#' - functions
#'     - min5p: fifth percentile of daily minimum
#'     - mean: mean of daily mean
#'     - max95p: ninety-fifth percentile of daily maximum
#'     - drange: mean of daily range
#'     - gdd{base}: growing degree days with base from parameter `gdd_t_base`
#'     - fdd{base}: freezing degree days with base from parameter `fdd_t_base`
#'     - frostdays: the number of days in which some temerature value was lower than parameter `fdd_t_base`
#'
#' @template param_myClim_object_cleaned
#' @template params_env_agg
#' @param gdd_t_base base temeprature for Growing degree days [myClim::mc_calc_gdd()] (default 5)
#' @param fdd_t_base base temeprature for freezing degree days [myClim::mc_calc_fdd()] (default 0)
#' @return table in long format with environment variables
#' @export
#' @examples
#' data <- mc_prep_crop(mc_data_example_clean, lubridate::ymd_h("2020-11-01 00"), lubridate::ymd_h("2021-02-01 00"), end_included = FALSE)
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
    result <- list(temp_sensors=character(), other_sensors=character())
    for(temp_sensor in names(temp_sensors$localities)) {
        localities <- temp_sensors$localities[[temp_sensor]]
        result$data <- mc_calc_gdd(data, temp_sensor, output_prefix=.env_const_GDD, t_base=gdd_t_base, localities=localities)
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
    agg_table <- dplyr::distinct(dplyr::select(table, sensor_base, day_fun))
    agg_table <- dplyr::group_by(agg_table, sensor_base)
    group_function <- function(fun_table, group) {
        fun_table$day_fun
    }
    fun <- dplyr::group_map(agg_table, group_function)
    names(fun) <- dplyr::group_keys(agg_table)$sensor_base
    mc_agg(data, fun, "day", use_utc=use_utc)
}

.env_get_result_agg <- function(day_data, table, period, use_utc, custom_start, custom_end, min_coverage, custom_functions=NULL,
                                percentiles=c(.env_const_MIN_PERCENTILE, .env_const_MAX_PERCENTILE)) {
    agg_table <- dplyr::select(table, sensor_prep, period_fun)
    agg_table <- dplyr::group_by(agg_table, sensor_prep)

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

#' Moisture environment variables
#'
#' @description
#' Function ...
#'
#' @details
#' - used all VWC sensors
#' - functions
#'     - 5p: fifth percentile
#'     - mean: mean
#'     - 95p: ninety-fifth percentile
#'     - sd: standard deviation
#'
#' @template param_myClim_object_cleaned
#' @template params_env_agg
#' @return table in long format with environment variables
#' @export
#' @examples
#' data <- mc_prep_crop(mc_data_example_calc, lubridate::ymd_h("2020-11-01 00"), lubridate::ymd_h("2021-02-01 00"), end_included = FALSE)
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
    result <- list(moist_sensors=character())
    for(moist_sensor in names(moist_sensors$localities)) {
        height_name <- .agg_get_height_name(moist_sensor, moist_sensors$heights[[moist_sensor]])
        new_name <- paste0(.env_const_VWC_PREFIX, height_name)
        result$moist_sensors <- append(result$moist_sensors, new_name)
        rename_rules <- list()
        rename_rules[[moist_sensor]] <- new_name
        result$data <- mc_prep_meta_sensor(data, rename_rules, param_name="name")
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

#' vapor pressure deficit environment variables
#'
#' @description
#' Function ...
#'
#' @details
#' - used all VPD sensors
#' - functions
#'     - mean: mean of daily mean
#'     - max95p: ninety-fifth percentile of daily maximum
#'
#' @template param_myClim_object_cleaned
#' @template params_env_agg
#' @return table in long format with environment variables
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
    result <- list(vpd_sensors=character())
    for(vpd_sensor in names(vpd_sensors$localities)) {
        height_name <- .agg_get_height_name(vpd_sensor, vpd_sensors$heights[[vpd_sensor]])
        new_name <- paste0(.env_const_VPD_PREFIX, height_name)
        result$vpd_sensors <- append(result$vpd_sensors, new_name)
        rename_rules <- list()
        rename_rules[[vpd_sensor]] <- new_name
        result$data <- mc_prep_meta_sensor(data, rename_rules, param_name="name")
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

