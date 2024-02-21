.states_const_MESSAGE_NOT_EXISTS_LOCALITY <- "Locality {.y$locality_id} does not exist in the data."
.states_const_MESSAGE_NOT_EXISTS_LOGGER <- "Locality {locality_id} does not contain logger with index {.y$logger_index}."
.states_const_MESSAGE_NOT_EXISTS_LOGGER_SENSOR <- "Logger {logger_index} in locality {locality_id} does not contain sensor {sensor_name}."
.states_const_MESSAGE_NOT_EXISTS_AGG_SENSOR <- "Locality {locality_id} does not contain sensor {sensor_name}."
.states_const_MESSAGE_LOGGERS_IN_AGG <- "You can not use logger_index in agg format."
.states_const_MESSAGE_MISSED_LOGGER_INDEX <- "All values logger_index must be set."
.states_const_MESSAGE_MISSED_COLUMN <- "Columns {columns_text} are required."

#' Insert states of sensors
#'
#' @description
#' This function insert new states to sensor see [myClim-package]
#'
#' @template param_myClim_object_cleaned
#' @param states_table data.frame with columns:
#' * locality_id
#' * logger_index - index of logger in locality
#' * sensor_name - original sensor id if not modified, if renamed then new name (e.g.,"GDD5", "HOBO_T_mean" ,"TMS_T1_max", "my_sensor01")
#' * tag - category of state
#' * start - start datetime
#' * end - end datetime
#' * value - value of state
#'
#' As input can be used result of [mc_info_states()]. The sensor_name and value columns are optional.
#' When sensor_name is NA, states are inserted to all sensors.
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
            if(!(sensor_name %in% names(sensors_item$sensors))) {
                warning(stringr::str_glue(.states_const_MESSAGE_NOT_EXISTS_LOGGER_SENSOR))
            }
        }
        if(!(sensor_name %in% names(sensors_item$sensors))) {
            return()
        }
        states_table <- dplyr::select(.x, "tag", "start", "end", "value")
        .states_insert(data_env, locality_id, logger_index, sensor_name, states_table)
    }

    sensor_prep_function <- function(.x, .y) {
        sensor_name <- .y$sensor_name
        locality_id <- dplyr::first(.x$locality_id)
        logger_index <- dplyr::first(.x$logger_index)
        if(!is.na(sensor_name)) {
            sensor_function(.x, .y)
            return()
        }

        if(is_agg) {
            sensors_item <- data$localities[[locality_id]]
        } else {
            sensors_item <- data$localities[[locality_id]]$loggers[[logger_index]]
        }

        sensor_names_table <- tibble::tibble(logger_index=logger_index, sensor_name=names(sensors_item$sensors))
        .x$sensor_name <- NULL
        .x <- dplyr::left_join(.x, sensor_names_table, by="logger_index")
        groupped_sensors <- dplyr::group_by(.x, .data$sensor_name)
        dplyr::group_walk(groupped_sensors, sensor_function, .keep=TRUE)
    }

    logger_function <- function(.x, .y) {
        locality_id <- dplyr::first(.x$locality_id)
        if(!is.na(.y$logger_index) && .y$logger_index > length(data$localities[[locality_id]]$loggers)) {
            warning(stringr::str_glue(.states_const_MESSAGE_NOT_EXISTS_LOGGER))
            return()
        }

        groupped_sensors <- dplyr::group_by(.x, .data$sensor_name)
        dplyr::group_walk(groupped_sensors, sensor_prep_function, .keep=TRUE)
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
            dplyr::group_walk(groupped_sensors, sensor_prep_function, .keep=TRUE)
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
    if(!is_agg && any(is.na(states_table$logger_index))) {
        stop(.states_const_MESSAGE_MISSED_LOGGER_INDEX)
    }
    required_columns <- c("locality_id", "logger_index", "tag", "start", "end")
    if(is_agg){
        required_columns <- required_columns[-2]
    }
    if(!all(required_columns %in% names(states_table))) {
        columns_text <- paste(required_columns, collapse=", ")
        stop(stringr::str_glue(.states_const_MESSAGE_MISSED_COLUMN))
    }
}

.states_fix_table <- function(states_table) {
    if(!("logger_index" %in% names(states_table))) {
        states_table$logger_index <- NA_integer_
    }
    if(!("sensor_name" %in% names(states_table))) {
        states_table$sensor_name <- NA_character_
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