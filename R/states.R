.states_const_MESSAGE_NOT_EXISTS_LOCALITY <- "Locality {.y$locality_id} does not exist in the data."
.states_const_MESSAGE_NOT_EXISTS_LOGGER <- "Locality {locality_id} does not contain logger with index {.y$logger_index}."
.states_const_MESSAGE_NOT_EXISTS_LOGGER_SENSOR <- "Logger {.y$logger_index} in locality {locality_id} does not contain sensor {sensor_name}."
.states_const_MESSAGE_NOT_EXISTS_AGG_SENSOR <- "Locality {locality_id} does not contain sensor {sensor_name}."
.states_const_MESSAGE_LOGGERS_IN_AGG <- "You can not use logger_index in agg format."

#' Insert states of sensors
#'
#' @description
#' This function insert new states to sensor see [myClim-package]
#'
#' @template param_myClim_object_cleaned
#' @param states_table table in same format as return [mc_info_states()]
#'
#' The logger_index and value columns are optional. When logger_index is NA, states are inserted to all loggers with
#' right interval.
#' @return myClim object in the same format as input, with inserted sensor states
#' @export
#' @examples
#' states <- data.frame(locality_id="A1E05", sensor_name="Thermo_T", tag="error",
#'                      start=lubridate::ymd_hm("2020-10-28 9:00"),
#'                      end=lubridate::ymd_hm("2020-10-28 9:30"))
#' data <- mc_states_insert(mc_data_example_clean, states)
mc_states_insert <- function(data, states_table) {
    .prep_check_datetime_step_unprocessed(data, stop)
    .states_check_table(data, states_table)
    states_table <- .states_fix_table(states_table)
    is_agg <-.common_is_agg_format(data)
    data_env <- new.env()
    data_env$data <- data
    groupped_localities <- dplyr::group_by(states_table, .data$locality_id)

    sensor_function <- function(.x, .y) {
        sensor_name <- .y$sensor_name
        locality_id <- dplyr::first(.x$locality_id)
        logger_index <- dplyr::first(.x$logger_index)
        if(is_agg) {
            sensors_item <- data$localities[[locality_id]]
            if(!(sensor_name %in% names(sensors_item$sensors))) {
                warning(stringr::str_glue(.states_const_MESSAGE_NOT_EXISTS_AGG_SENSOR))
            }
        } else {
            sensors_item <- data$localities[[locality_id]]$loggers[[logger_index]]
        }
        if(!(sensor_name %in% names(sensors_item$sensors))) {
            return()
        }
        states_table <- dplyr::select(.x, "tag", "start", "end", "value")
        .states_insert(data_env, locality_id, logger_index, sensor_name, states_table)
    }

    logger_function <- function(.x, .y) {
        locality_id <- dplyr::first(.x$locality_id)
        locality <- data$localities[[locality_id]]
        table <- .x
        if(!is.na(.y$logger_index) && .y$logger_index > length(data$localities[[locality_id]]$loggers)) {
            warning(stringr::str_glue(.states_const_MESSAGE_NOT_EXISTS_LOGGER))
            return()
        }
        if(!is.na(.y$logger_index)) {
            logger <- locality$loggers[[.y$logger_index]]
            sensor_names <- unique(table$sensor_name)
            missing_sensors <- !(sensor_names %in% names(logger$sensors))
            for(sensor_name in sensor_names[missing_sensors]) {
                warning(stringr::str_glue(.states_const_MESSAGE_NOT_EXISTS_LOGGER_SENSOR))
            }
        } else {
            logger_index_table <- tibble::tibble(locality_id=locality_id, logger_index=seq_along(locality$loggers))
            table$logger_index <- NULL
            table <- dplyr::left_join(table, logger_index_table, by="locality_id")
        }

        for(current_index in unique(table$logger_index)) {
            filtered_table <- dplyr::filter(table, .data$logger_index == current_index)
            groupped_sensors <- dplyr::group_by(filtered_table, .data$sensor_name)
            dplyr::group_walk(groupped_sensors, sensor_function, .keep=TRUE)
        }
    }

    locality_function <- function(.x, .y) {
        if(!(.y$locality_id %in% names(data$localities))) {
            warning(stringr::str_glue(.states_const_MESSAGE_NOT_EXISTS_LOCALITY))
            return()
        }
        if(!is_agg) {
            groupped_loggers <- dplyr::group_by(.x, .data$logger_index)
            dplyr::group_walk(groupped_loggers, logger_function, .keep=TRUE)
        } else {
            groupped_sensors <- dplyr::group_by(.x, .data$sensor_name)
            dplyr::group_walk(groupped_sensors, sensor_function, .keep=TRUE)
        }
    }

    dplyr::group_walk(groupped_localities, locality_function, .keep=TRUE)
    return(data_env$data)
}

.states_check_table <- function(data, states_table) {
    is_agg <-.common_is_agg_format(data)
    if(is_agg && !all(is.na(states_table$logger_index))) {
        stop(.states_const_MESSAGE_LOGGERS_IN_AGG)
    }
}

.states_fix_table <- function(states_table) {
    if(!("logger_index" %in% names(states_table))) {
        states_table$logger_index <- NA_integer_
    }
    if(!("value" %in% names(states_table))) {
        states_table$value <- NA_character_
    }
    return(states_table)
}

.states_insert <- function(data_env, locality_id, logger_index, sensor_name, states_table) {
    date_interval <- .states_get_item_range(data_env$data, locality_id, logger_index)
    period <- .common_get_period_from_data(data_env$data, locality_id, logger_index)
    row_function <- function(tag, start, end, value){
        out_interval <- lubridate::intersect(lubridate::interval(start, end), date_interval)
        if(is.na(out_interval)) {
            return(list())
        }
        start <- lubridate::floor_date(lubridate::int_start(out_interval), period)
        end <- lubridate::floor_date(lubridate::int_end(out_interval), period)
        return(list(tag=tag,
                    start=start,
                    end=end,
                    value=value))
    }
    states_table <- purrr::pmap_dfr(states_table, row_function)

    if(.common_is_agg_format(data_env$data)) {
        data_env$data$localities[[locality_id]]$sensors[[sensor_name]]$states <-
            dplyr::bind_rows(data_env$data$localities[[locality_id]]$sensors[[sensor_name]]$states,
                             states_table)
    } else {
        data_env$data$localities[[locality_id]]$loggers[[logger_index]]$sensors[[sensor_name]]$states <-
            dplyr::bind_rows(data_env$data$localities[[locality_id]]$loggers[[logger_index]]$sensors[[sensor_name]]$states,
                             states_table)
    }
}

.states_get_item_range <- function(data, locality_id, logger_index) {
    if(.common_is_agg_format(data)) {
        datetime <- data$localities[[locality_id]]$datetime
    } else {
        datetime <- data$localities[[locality_id]]$loggers[[logger_index]]$datetime
    }
    return(lubridate::interval(dplyr::first(datetime), dplyr::last(datetime)))
}