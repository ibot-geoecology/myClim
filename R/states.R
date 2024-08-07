.states_const_MESSAGE_NOT_EXISTS_LOCALITY <- "Locality {.y$locality_id} does not exist in the data."
.states_const_MESSAGE_NOT_EXISTS_LOGGER <- "Locality {locality_id} does not contain logger with index {.y$logger_index}."
.states_const_MESSAGE_NOT_EXISTS_LOGGER_SENSOR <- "Logger {logger_index} in locality {locality_id} does not contain sensor {sensor_name}."
.states_const_MESSAGE_NOT_EXISTS_AGG_SENSOR <- "Locality {locality_id} does not contain sensor {sensor_name}."
.states_const_MESSAGE_LOGGERS_IN_AGG <- "You can not use logger_index in agg format."
.states_const_MESSAGE_MISSED_LOGGER_INDEX <- "All values logger_index must be set."
.states_const_MESSAGE_MISSED_COLUMN <- "Columns {columns_text} are required."
.states_const_MESSAGE_NA_VALUE <- "All values of {column_name} must be set."
.states_const_MESSAGE_START_GREATER <- "The start date and time must be earlier than or identical to the end date and time."

.states_const_COLUMN_LOCALITY_ID <- "locality_id"
.states_const_COLUMN_LOGGER_INDEX <- "logger_index"
.states_const_COLUMN_SENSOR_NAME <- "sensor_name"
.states_const_COLUMN_TAG <- "tag"
.states_const_COLUMN_START <- "start"
.states_const_COLUMN_END <- "end"
.states_const_COLUMN_VALUE <- "value"

#' Insert new sensor states (tags)
#'
#' @description
#' This function inserts new states (tags) into the selected part of the sensor 
#' time-series. For more information about the structure of states (tags), 
#' see [myClim-package]. `mc_states_insert()` does not affect 
#' existing rows in the states (tags) table but only inserts new rows even if
#' the new ones are identical with existing (resulting in duplicated states). 
#' 
#' @details
#' As a template for inserting states (tags), it is recommended to use 
#' the output of [mc_info_states()], which will return the table with all necessary 
#' columns correctly named. The `sensor_name` and `value` columns are optional and do not 
#' need to be filled in. 
#' 
#' When `locality_id` is provided but `sensor_name` is NA in the states (tags) table, 
#' states are inserted for all sensors within the locality.
#' 
#' The states (tags) are associated with the sensor time-series, specifically to 
#' the defined part of the time-series identified by start and end date times. A 
#' single time series can contain multiple states (tags) of identical or different types, and these 
#' states (tags) can overlap. Start and end date times are adjusted to fit within 
#' the range of logger/locality datetime and are rounded according to the logger's step. For instance, 
#' if a user attempts to insert a tag beyond the sensor time-series range, `mc_states_insert` 
#' will adjust the start and end times to fit the available measurements. If a user defines a start time as 
#' '2020-01-01 10:23:00' on a logger with a 15-minute step, it will be rounded to '2020-01-01 10:30:00'. 
#' @template param_myClim_object_cleaned
#' @param states_table Output of [mc_info_states()] can be used as template for input data.frame. 
#' data.frame with columns:
#' * locality_id - the name of locality (in some cases identical to logger id, see [mc_read_files])
#' * logger_index - index of logger in myClim object at the locality. See [mc_info_logger].   
#' * sensor_name - sensor name either original (e.g., TMS_T1, T_C), or calculated/renamed (e.g., "TMS_T1_max", "my_sensor01") 
#' * tag - category of state (e.g., "conflict", "error", "source", "quality")  
#' * start - start datetime
#' * end - end datetime
#' * value - value of tag (e.g., "out of soil", "c:/users/John/tmsData/data_911235678.csv")
#' @return myClim object in the same format as input, with inserted sensor states
#' @export
#' @examples
#' states <- data.frame(locality_id="A1E05", logger_index=1, sensor_name="Thermo_T", tag="error",
#'                      start=lubridate::ymd_hm("2020-10-28 9:00"),
#'                      end=lubridate::ymd_hm("2020-10-28 9:30"))
#' data <- mc_states_insert(mc_data_example_clean, states)
mc_states_insert <- function(data, states_table) {
    states_table <- .states_prepare_and_check(data, states_table, FALSE)
    return(.states_run(data, states_table, .states_insert))
}

#' Update sensor states (tags)
#'
#' @description
#' This function updates (replaces) existing states (tags). For more information about 
#' the structure of states (tags), see [myClim-package]. 
#' In contrast with [mc_states_insert], which does not affect existing states (tags),
#' [mc_states_update] deletes all old states and replaces them with new ones, 
#' even if the new states table contains fewer states than original object.
#' 
#' @details
#' As a template for updating states (tags), it is recommended to use 
#' the output of [mc_info_states()], which will return the table with all necessary 
#' columns correctly named. The `sensor_name` and `value` columns are optional and do not 
#' need to be filled in. 
#' 
#' The states (tags) are associated with the sensor time-series, specifically to 
#' the defined part of the time-series identified by start and end date times. A 
#' single time series can contain multiple states (tags) of identical or different types, and these 
#' states (tags) can overlap. Start and end date times are adjusted to fit within 
#' the range of logger/locality datetime and are rounded according to the logger's step. For instance, 
#' if a user attempts to insert a tag beyond the sensor time-series range, `mc_states_insert` 
#' will adjust the start and end times to fit the available measurements. If a user defines a start time as 
#' '2020-01-01 10:23:00' on a logger with a 15-minute step, it will be rounded to '2020-01-01 10:30:00'.
#' 
#' In contrast with [mc_states_insert], the automatic filling of states when `locality_id` 
#' is provided but `sensor_name` is NA is not implemented in [mc_states_update]. When a user needs to update 
#' states (tags) for all sensors within the locality, each state (tag) needs to have a separate row in 
#' the input table. 
#' 
#' @template param_myClim_object_cleaned
#' @param states_table Output of [mc_info_states()] can be used as template for input data.frame. 
#' 
#' data.frame with columns:
#' * locality_id - the name of locality (in some cases identical to logger id, see details of [mc_read_files])
#' * logger_index - index of logger in myClim object at the locality. See [mc_info_logger].
#' * sensor_name - sensor name either original (e.g., TMS_T1, T_C), or calculated/renamed (e.g., "TMS_T1_max", "my_sensor01") 
#' * tag - category of state (e.g., "conflict", "error", "source", "quality")  
#' * start - start datetime
#' * end - end datetime
#' * value - value of tag (e.g., "out of soil", "c:/users/John/tmsData/data_911235678.csv")
#'
#' @return myClim object in the same format as input, with updated sensor states
#' @export
#' @examples
#' states <- mc_info_states(mc_data_example_clean)
#' states$value <- basename(states$value)
#' data <- mc_states_update(mc_data_example_clean, states)
mc_states_update <- function(data, states_table) {
    states_table <- .states_prepare_and_check(data, states_table, TRUE)
    data <- mc_states_delete(data)
    return(.states_run(data, states_table, .states_update))
}

.states_prepare_and_check <- function(data, states_table, is_strict) {
    .prep_check_datetime_step_unprocessed(data, stop)
    .states_check_table(data, states_table, is_strict)
    states_table <- .states_fix_table(states_table)
    return(states_table)
}

.states_run <- function(data, states_table, action_function, edit_datetimes=TRUE) {
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
        action_function(data_env, locality_id, logger_index, sensor_name, states_table, edit_datetimes)
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

.states_check_table <- function(data, states_table, is_strict) {
    .states_check_columns(data, states_table, is_strict)
    .states_check_na(data, states_table, is_strict)
    if(any(states_table$start > states_table$end)) {
        stop(.states_const_MESSAGE_START_GREATER)
    }
}

.states_check_columns <- function(data, states_table, is_strict) {
    is_agg <-.common_is_agg_format(data)
    required_columns <- c(.states_const_COLUMN_LOCALITY_ID, .states_const_COLUMN_LOGGER_INDEX,
                          .states_const_COLUMN_SENSOR_NAME, .states_const_COLUMN_TAG,
                          .states_const_COLUMN_START, .states_const_COLUMN_END,
                          .states_const_COLUMN_VALUE)
    if(!is_strict && !is_agg) {
        required_columns <- c(.states_const_COLUMN_LOCALITY_ID, .states_const_COLUMN_LOGGER_INDEX,
                              .states_const_COLUMN_TAG, .states_const_COLUMN_START, .states_const_COLUMN_END)
    } else if(!is_strict && is_agg){
        required_columns <- c(.states_const_COLUMN_LOCALITY_ID, .states_const_COLUMN_TAG,
                              .states_const_COLUMN_START, .states_const_COLUMN_END)
    }
    if(!all(required_columns %in% names(states_table))) {
        columns_text <- paste(required_columns, collapse=", ")
        stop(stringr::str_glue(.states_const_MESSAGE_MISSED_COLUMN))
    }
}

.states_check_na <- function(data, states_table, is_strict) {
    is_agg <-.common_is_agg_format(data)
    not_na_columns <- c(.states_const_COLUMN_LOCALITY_ID, .states_const_COLUMN_TAG,
                        .states_const_COLUMN_START, .states_const_COLUMN_END)
    if(is_strict) {
        not_na_columns <- c(.states_const_COLUMN_LOCALITY_ID, .states_const_COLUMN_TAG, .states_const_COLUMN_SENSOR_NAME,
                            .states_const_COLUMN_START, .states_const_COLUMN_END)
    }
    for(column_name in not_na_columns) {
        if(any(is.na(states_table[[column_name]]))) {
            stop(stringr::str_glue(.states_const_MESSAGE_NA_VALUE))
        }
    }
    if(is_agg && !all(is.na(states_table$logger_index))) {
        stop(.states_const_MESSAGE_LOGGERS_IN_AGG)
    }
    if(!is_agg && any(is.na(states_table$logger_index))) {
        stop(.states_const_MESSAGE_MISSED_LOGGER_INDEX)
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

.states_insert <- function(data_env, locality_id, logger_index, sensor_name, states_table, edit_datetimes) {
    if(edit_datetimes) {
        states_table <- .states_edit_datetimes(data_env$data, locality_id, logger_index, states_table)
    }
    states_table <- as.data.frame(states_table)
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

.states_update <- function(data_env, locality_id, logger_index, sensor_name, states_table, edit_datetimes) {
    if(edit_datetimes) {
        states_table <- .states_edit_datetimes(data_env$data, locality_id, logger_index, states_table)
    }
    states_table <- as.data.frame(states_table)
    if(.common_is_agg_format(data_env$data)) {
        data_env$data$localities[[locality_id]]$sensors[[sensor_name]]$states <- states_table
    } else {
        data_env$data$localities[[locality_id]]$loggers[[logger_index]]$sensors[[sensor_name]]$states <- states_table
    }
}

.states_edit_datetimes <- function(data, locality_id, logger_index, states_table) {
    period <- NULL
    step <- NULL
    is_agg <- .common_is_agg_format(data)
    date_interval <- .states_get_item_range(data, locality_id, logger_index)
    if(is_agg){
        period <- .common_get_period_from_data(data, locality_id, logger_index)
    } else {
        step <- data$localities[[locality_id]]$loggers[[logger_index]]$clean_info@step
    }
    row_function <- function(tag, start, end, value){
        out_interval <- lubridate::intersect(lubridate::interval(start, end), date_interval)
        if(is.na(out_interval)) {
            return(list())
        }
        if(is_agg){
            start <- lubridate::floor_date(lubridate::int_start(out_interval), period)
            end <- lubridate::floor_date(lubridate::int_end(out_interval), period)
        } else {
            start <- .states_floor_datetime(lubridate::int_start(out_interval), lubridate::int_start(date_interval), step)
            end <- .states_floor_datetime(lubridate::int_end(out_interval), lubridate::int_start(date_interval), step)
        }
        return(list(tag=tag,
                    start=start,
                    end=end,
                    value=value))
    }
    states_table <- purrr::pmap_dfr(states_table, row_function)
    return(states_table)
}

.states_get_item_range <- function(data, locality_id, logger_index) {
    if(.common_is_agg_format(data)) {
        datetime <- data$localities[[locality_id]]$datetime
    } else {
        datetime <- data$localities[[locality_id]]$loggers[[logger_index]]$datetime
    }
    return(lubridate::interval(dplyr::first(datetime), dplyr::last(datetime)))
}

#' Delete sensor states (tags)
#'
#' @description
#' This function removes states (tags) defined by locality ID, sensor name, or tag value,
#' or any combination of these three.
#'
#' @template param_myClim_object_cleaned
#' @param localities locality ids where delete states (tags). If NULL then all. (default NULL)
#' @param sensors sensor names where delete states (tags). If NULL then all. (default NULL)
#' @param tags specific tag to be deleted. If NULL then all. (default NULL)
#' @return myClim object in the same format as input, with deleted sensor states
#' @export
#' @examples
#' data <- mc_states_delete(mc_data_example_clean, localities="A1E05",
#'                          sensors=c(mc_const_SENSOR_Dendro_T, mc_const_SENSOR_Dendro_raw))
mc_states_delete <- function(data, localities=NULL, sensors=NULL, tags=NULL) {
    is_agg_format <- .common_is_agg_format(data)

    sensor_function <- function(sensor) {
        is_in <- is.null(sensors) || sensor$metadata@name %in% sensors
        if(!is_in || nrow(sensor$states) == 0) {
            return(sensor)
        }
        if(is.null(tags)) {
            sensor$states <- data.frame()
        } else {
            sensor$states <- dplyr::filter(sensor$states, !(.data$tag %in% tags))
        }
        return(sensor)
    }

    sensors_item_function <- function(item) {
        item$sensors <- purrr::map(item$sensors, sensor_function)
        return(item)
    }

    locality_function <- function(locality) {
        is_in <- is.null(localities) || locality$metadata@locality_id %in% localities

        if(!is_in) {
            return(locality)
        }

        if (!is_agg_format) {
            locality$loggers <- purrr::map(locality$loggers, sensors_item_function)
        } else {
            locality <- sensors_item_function(locality)
        }
        return(locality)
    }

    data$localities <- purrr::map(data$localities, locality_function)

    return(data)
}

.states_floor_sensor <- function(sensor, start_datetime, step) {
    if(nrow(sensor$states) == 0) {
        return(sensor)
    }

    sensor$states$start <- .states_floor_datetime(sensor$states$start, start_datetime, step)
    sensor$states$end <- .states_floor_datetime(sensor$states$end, start_datetime, step)

    return(sensor)
}

.states_floor_datetime <- function(datetime_values, start_datetime, step) {
    start_seconds <- as.numeric(start_datetime)
    datetime_seconds <- as.numeric(datetime_values) - start_seconds
    result <- .common_as_utc_posixct(datetime_seconds %/% step * step + start_seconds)
    return(result)
}

#' Replace values by states with tag
#'
#' @description
#' This function replace values of sensors by states with tag.
#'
#' @template param_myClim_object
#' @param tags specific tag to be replaced.
#' @param replace_value (default NA).
#' @return myClim object in the same format as input, with replaced values
#' @export
#' @examples
#' states <- data.frame(locality_id="A1E05", logger_index=1, sensor_name="Thermo_T", tag="error",
#'                      start=lubridate::ymd_hm("2020-10-28 9:00"),
#'                      end=lubridate::ymd_hm("2020-10-28 9:30"))
#' data <- mc_states_insert(mc_data_example_clean, states)
#' data <- mc_states_replace(data, "error")
mc_states_replace <- function(data, tags, replace_value=NA) {
    is_agg_format <- .common_is_agg_format(data)
    
    states_table <- mc_info_states(data)
    states_table <- dplyr::filter(states_table, .data$tag %in% tags)
    states_table <- dplyr::group_by(states_table, .data$locality_id, .data$logger_index, .data$sensor_name)
    
    result <- new.env()
    result$data <- data
    
    group_function <- function(data_table, group) {
        intervals <- lubridate::interval(data_table$start, data_table$end)
        if(is_agg_format) {
            datetime <- result$data$localities[[group$locality_id]]$datetime
        } else {
            datetime <- result$data$localities[[group$locality_id]]$loggers[[group$logger_index]]$datetime
        }
        conditions <- purrr::map(intervals, ~ lubridate::`%within%`(datetime, .x))
        condition <- purrr::reduce(conditions, `|`)
        if(is_agg_format) {
            result$data$localities[[group$locality_id]]$sensors[[group$sensor_name]]$values[condition] <- replace_value
        } else {
            result$data$localities[[group$locality_id]]$loggers[[group$logger_index]]$sensors[[group$sensor_name]]$values[condition] <- replace_value
        }
    }
    dplyr::group_walk(states_table, group_function)
    return(result$data)
}
