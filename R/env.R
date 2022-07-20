#' Tempereature environment variables
#'
#' @description
#' Function ...
#'
#' @details
#'
#' @param data myClim object in cleaned Prep-format or Calc-formt see [myClim::mc_agg()] and [myClim-package]
#' @return long table with environment variables
#' @export
#' @examples
mc_env_temp <- function(data, period=NULL, use_utc=TRUE, custom_start=NULL, custom_end=NULL, gdd_t_base=5, fdd_t_base=0) {
    is_calc <- myClim:::.common_is_calc_format(data)
    if(!is_calc) {
        data <- mc_agg(data)
    }
    temp_sensors <- .env_get_sensors_by_physical(data, myClim:::.model_const_PHYSICAL_T_C)
    new_temp_sensors <- character()
    other_sensors <- character()
    for(temp_sensor in names(temp_sensors$localities)) {
        localities <- temp_sensors$localities[[temp_sensor]]
        data <- mc_calc_gdd(data, temp_sensor, output_prefix="GDD", t_base=gdd_t_base, localities=localities)
        data <- mc_calc_fdd(data, temp_sensor, output_prefix="FDD", t_base=fdd_t_base, localities=localities)
        height_name <- myClim:::.agg_get_height_name(temp_sensor, temp_sensors$heights[[temp_sensor]])
        new_temp_sensors <- append(new_temp_sensors, height_name)
        rename_rules <- list()
        rename_rules[[temp_sensor]] <- height_name
        gdd_name <- myClim:::.calc_get_name_with_base("gdd", gdd_t_base)
        new_gdd_name <- stringr::str_glue("{gdd_name}.{height_name}")
        rename_rules[[gdd_name]] <- new_gdd_name
        other_sensors <- append(other_sensors, new_gdd_name)
        fdd_name <- myClim:::.calc_get_name_with_base("fdd", fdd_t_base)
        new_fdd_name <- stringr::str_glue("{fdd_name}.{height_name}")
        rename_rules[[fdd_name]] <- new_fdd_name
            other_sensors <- append(other_sensors, new_fdd_name)
        data <- mc_prep_meta_sensor(data, rename_rules, param_name="name")
    }
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