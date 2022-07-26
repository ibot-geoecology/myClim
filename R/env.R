.env_const_MIN_PERCENTILE <- 5
.env_const_MAX_PERCENTILE <- 95
.env_const_GDD <- "GDD"
.env_const_FDD <- "FDD"
.env_const_FROSTDAYS <- "frostdays"

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
#' @param data myClim object in cleaned Prep-format or Calc-formt see [myClim::mc_agg()] and [myClim-package]
#' @param period output period see [myClim::mc_agg()]
#' @param use_utc if FALSE, then local time is used for day aggregation see [myClim::mc_agg()]
#' @param custom_start start date for custom period see [myClim::mc_agg()]
#' @param custom_end end date for custom period see [myClim::mc_agg()]
#' @param gdd_t_base base temeprature for Growing degree days [myClim::mc_calc_gdd()]
#' @param fdd_t_base base temeprature for freezing degree days [myClim::mc_calc_fdd()]
#' @param use_utc if FALSE, then local time is used for day aggregation see [myClim::mc_agg()]
#' @return table in long format with environment variables
#' @export
#' @examples
#' data <- mc_prep_crop(mc_data_example_clean, lubridate::ymd_h("2020-11-01 00"), lubridate::ymd_h("2021-02-01 00"), end_included = FALSE)
#' mc_env_temp(data, "month")
mc_env_temp <- function(data, period, use_utc=TRUE, custom_start=NULL, custom_end=NULL, gdd_t_base=5, fdd_t_base=0) {
    is_calc <- myClim:::.common_is_calc_format(data)
    if(!is_calc) {
        data <- mc_agg(data)
    }
    prepared <- .env_temp_calc_fdd_gdd_and_rename(data, gdd_t_base, fdd_t_base)
    table <- .env_temp_get_table(prepared$temp_sensors, gdd_t_base, fdd_t_base)
    day_data <- .env_temp_get_day_agg(prepared$data, table, use_utc)
    result_data <- .env_temp_get_result_agg(day_data, table, period, custom_start, custom_end, fdd_t_base)
    mc_reshape_long(result_data)
}

.env_temp_calc_fdd_gdd_and_rename <- function(data, gdd_t_base, fdd_t_base) {
    temp_sensors <- .env_get_sensors_by_physical(data, myClim:::.model_const_PHYSICAL_T_C)
    result <- list(temp_sensors=character(), other_sensors=character())
    for(temp_sensor in names(temp_sensors$localities)) {
        localities <- temp_sensors$localities[[temp_sensor]]
        result$data <- mc_calc_gdd(data, temp_sensor, output_prefix=.env_const_GDD, t_base=gdd_t_base, localities=localities)
        result$data <- mc_calc_fdd(result$data, temp_sensor, output_prefix=.env_const_FDD, t_base=fdd_t_base, localities=localities)
        height_name <- myClim:::.agg_get_height_name(temp_sensor, temp_sensors$heights[[temp_sensor]])
        result$temp_sensors <- append(result$temp_sensors, height_name)
        rename_rules <- list()
        rename_rules[[temp_sensor]] <- height_name
        gdd_name <- myClim:::.calc_get_name_with_base(.env_const_GDD, gdd_t_base)
        new_gdd_name <- .env_get_calc_sensor_name(gdd_name, height_name)
        rename_rules[[gdd_name]] <- new_gdd_name
        fdd_name <- myClim:::.calc_get_name_with_base(.env_const_FDD, fdd_t_base)
        new_fdd_name <- .env_get_calc_sensor_name(fdd_name, height_name)
        rename_rules[[fdd_name]] <- new_fdd_name
        result$data <- mc_prep_meta_sensor(result$data, rename_rules, param_name="name")
    }
    return(result)
}

.env_get_sensors_by_physical <- function(data, physical) {
    result <- new.env()
    result$localities <- new.env()
    result$heights <- new.env()
    sensor_function <- function(locality_id, sensor) {
        if(!myClim:::.model_is_physical(sensor$metadata, physical)) {
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
        gdd_with_base <- myClim:::.calc_get_name_with_base(.env_const_GDD, gdd_t_base)
        fdd_with_base <- myClim:::.calc_get_name_with_base(.env_const_FDD, fdd_t_base)
        gdd_name <- .env_get_calc_sensor_name(gdd_with_base, sensor)
        fdd_name <- .env_get_calc_sensor_name(fdd_with_base, sensor)
        sensor_base <- c(rep(sensor, count_rows - 2), gdd_name, fdd_name)
        day_agg_fun <- c(
            myClim:::.agg_const_FUNCTION_MIN,
            myClim:::.agg_const_FUNCTION_MIN,
            myClim:::.agg_const_FUNCTION_MEAN,
            myClim:::.agg_const_FUNCTION_MAX,
            myClim:::.agg_const_FUNCTION_RANGE,
            myClim:::.agg_const_FUNCTION_SUM,
            myClim:::.agg_const_FUNCTION_SUM)
        sensor_day <- purrr::map2_chr(sensor_base, day_agg_fun, ~ myClim:::.agg_get_aggregated_sensor_name(.x, .y))
        period_agg_fun <- c(
            myClim:::.agg_const_FUNCTION_PERCENTILE,
            .env_const_FROSTDAYS,
            myClim:::.agg_const_FUNCTION_MEAN,
            myClim:::.agg_const_FUNCTION_PERCENTILE,
            myClim:::.agg_const_FUNCTION_MEAN,
            myClim:::.agg_const_FUNCTION_SUM,
            myClim:::.agg_const_FUNCTION_SUM)
        output_sensor_function <- function(sensor_day, fun) {
            if(fun == myClim:::.agg_const_FUNCTION_PERCENTILE) {
                if(stringr::str_ends(sensor_day, myClim:::.agg_const_FUNCTION_MIN)) {
                    fun <- .agg_get_percentile_function_name(.env_const_MIN_PERCENTILE)
                } else {
                    fun <- .agg_get_percentile_function_name(.env_const_MAX_PERCENTILE)
                }
            }
            myClim:::.agg_get_aggregated_sensor_name(sensor_day, fun)
        }
        sensor_period <- purrr::map2_chr(sensor_day, period_agg_fun, output_sensor_function)
        result_texts <- c("min5p", .env_const_FROSTDAYS, "mean", "max95p", "drange", gdd_with_base, fdd_with_base)
        result_names <- stringr::str_glue("T.{result_texts}.{sensor}")
        list(sensor_base=sensor_base,
             day_agg_fun=day_agg_fun,
             sensor_day=sensor_day,
             period_agg_fun=period_agg_fun,
             sensor_period=sensor_period,
             result_name=result_names)
    }

    purrr::map_dfr(temp_sensors, sensor_function)
}

.env_temp_get_day_agg <- function(data, table, use_utc) {
    agg_table <- dplyr::distinct(dplyr::select(table, sensor_base, day_agg_fun))
    agg_table <- dplyr::group_by(agg_table, sensor_base)
    group_function <- function(fun_table, group) {
        fun_table$day_agg_fun
    }
    fun <- dplyr::group_map(agg_table, group_function)
    names(fun) <- dplyr::group_keys(agg_table)$sensor_base
    mc_agg(data, fun, "day", use_utc=use_utc)
}

.env_temp_get_result_agg <- function(day_data, table, period, custom_start, custom_end, fdd_t_base) {
    custom_functions <- list(frostdays=function(values){sum(values < fdd_t_base)})
    agg_table <- dplyr::select(table, sensor_day, period_agg_fun)
    agg_table <- dplyr::group_by(agg_table, sensor_day)
    group_function <- function(fun_table, group) {
        fun_table$period_agg_fun
    }
    fun <- dplyr::group_map(agg_table, group_function)
    names(fun) <- dplyr::group_keys(agg_table)$sensor_day
    result_data <- mc_agg(day_data, fun, period, custom_start=custom_start, custom_end=custom_end,
                          percentiles=c(.env_const_MIN_PERCENTILE, .env_const_MAX_PERCENTILE), custom_functions=custom_functions)
    rename_rules <- as.list(table$result_name)
    names(rename_rules) <- table$sensor_period
    result_data <- mc_prep_meta_sensor(result_data, rename_rules, param_name="name")
    mc_filter(result_data, sensors=table$result_name)
}

