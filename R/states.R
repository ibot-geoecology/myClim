.states_const_MESSAGE_NOT_EXISTS_LOCALITY <- "Locality {.y$locality_id} does not exist in the data."
.states_const_MESSAGE_NOT_EXISTS_LOGGER <- "Locality {locality_id} does not contain logger with index {.y$logger_index}."
.states_const_MESSAGE_NOT_EXISTS_LOGGER_SENSOR <- "Logger {logger_index} in locality {locality_id} does not contain sensor {sensor_name}."
.states_const_MESSAGE_NOT_EXISTS_AGG_SENSOR <- "Locality {locality_id} does not contain sensor {sensor_name}."
.states_const_MESSAGE_LOGGERS_IN_AGG <- "You can not use logger_index in agg format."
.states_const_MESSAGE_MISSED_LOGGER_INDEX <- "All values logger_index must be set."
.states_const_MESSAGE_MISSED_COLUMN <- "Columns {columns_text} are required."
.states_const_MESSAGE_NA_VALUE <- "All values of {column_name} must be set."
.states_const_MESSAGE_START_GREATER <- "The start date and time must be earlier than or identical to the end date and time."
.states_const_MESSAGE_NOT_LOGICAL_TYPE <- "The source_sensor must be of logical type."
.states_const_MESSAGE_SNSORS_LENGTH <- "If multiple to_sensor the lenght of source_sensor must be the same."
.states_const_MESSAGE_NEGATIVE_JUMP <- "The jump value must be a non-negative number."

.states_const_COLUMN_LOCALITY_ID <- "locality_id"
.states_const_COLUMN_LOGGER_INDEX <- "logger_index"
.states_const_COLUMN_SENSOR_NAME <- "sensor_name"
.states_const_COLUMN_TAG <- "tag"
.states_const_COLUMN_START <- "start"
.states_const_COLUMN_END <- "end"
.states_const_COLUMN_VALUE <- "value"

.states_const_COLUMN_MIN_VALUE <- "min_value"
.states_const_COLUMN_MAX_VALUE <- "max_value"
.states_const_COLUMN_POSITIVE_JUMP <- "positive_jump"
.states_const_COLUMN_NEGATIVE_JUMP <- "negative_jump"

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
        period <- .common_get_period_from_agg_data(data)
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

#' Convert a sensor to a state
#'
#' @description
#' This function creates a new state from an existing logical (TRUE/FALSE) sensor 
#' and assigns this new state to selected existing sensors.
#'
#' @details
#' The function is applicable only for logical (TRUE/FALSE) sensors. It allows 
#' you to convert such sensors into a state, represented as a tag. For example, 
#' you might calculate the estimation of snow cover using [mc_calc_snow] (TRUE/FALSE) 
#' and then want to remove temperature records when the logger was covered by snow. 
#' In this case, you can convert the snow sensor to a state, and then replace the 
#' values with NA for that state using [mc_states_replace]. In opposite case 
#' when you wish to keep e.g. only the moisture records when sensor was covered by 
#' snow, use `inverse = TRUE`. 
#'
#' @template param_myClim_object
#' @param source_sensor A logical sensor to be converted to states.
#' @param tag A tag for the new states, e.g., "snow".
#' @param to_sensor A vector of sensor names to which the new states should be attributed.
#' @param value The value of the new states (default is NA)
#' @param inverse A logical value. If FALSE, states are created for periods when `source_sensor` is TRUE (default is FALSE).
#' @return Returns a myClim object in the same format as the input, with added states.
#' @export
#' @examples
#' data <- mc_calc_snow(mc_data_example_agg, "TMS_T2", output_sensor="snow")
#' data <- mc_states_from_sensor(data, source_sensor="snow", tag="snow", to_sensor="TMS_T2")
mc_states_from_sensor <- function(data, source_sensor, tag, to_sensor, value=NA, inverse=FALSE) {
    .prep_check_datetime_step_unprocessed(data, stop)
    is_agg_format <- .common_is_agg_format(data)
    
    sensors_item_function <- function(item) {
        if(!(source_sensor %in% names(item$sensors))) {
            return(item)
        }
        if(!any(to_sensor %in% names(item$sensors))) {
            return(item)
        }
        new_states_table <- .states_get_states_table_from_logical_sensor(item$sensors[[source_sensor]], item$datetime, tag, value, inverse)
        if(nrow(new_states_table) == 0) {
            return(item)
        }
        for(to_sensor_name in to_sensor) {
            to_sensor_item <- item$sensors[[to_sensor_name]]
            states_table <- dplyr::union(to_sensor_item$states, new_states_table)
            item$sensors[[to_sensor_name]]$states <- states_table
        }
        return(item)
    }

    locality_function <- function(locality) {
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

.states_get_states_table_from_logical_sensor <- function(source_sensor_item, datetime, tag, value, inverse) {
    sensor_type <- myClim::mc_data_sensors[[source_sensor_item$metadata@sensor_id]]
    if(sensor_type@value_type != .model_const_VALUE_TYPE_LOGICAL) {
        stop(.states_const_MESSAGE_NOT_LOGICAL_TYPE)           
    }

    return(.states_get_states_table_from_logical_values(source_sensor_item$values, datetime, tag, value, inverse))
}

.states_get_states_table_from_logical_values <- function(log_values, datetime, tag, value=NA, inverse=FALSE) {
    rle_values <- rle(log_values)
    end_values <- cumsum(rle_values$lengths)
    start_values <- c(1, head(end_values, -1) + 1)
    not_na_condition <- !is.na(rle_values$values)
    start_values <- start_values[not_na_condition]
    end_values <- end_values[not_na_condition]
    condition <- rle_values$values[not_na_condition]
    if(inverse) {
        condition <- !condition
    }
    start_states <- datetime[start_values[condition]]
    if(length(start_states) == 0) {
        return(data.frame())
    }
    end_states <- datetime[end_values[condition]]
    new_states <- data.frame(tag=tag,
                             start=start_states,
                             end=end_states,
                             value=value)
    return(new_states)
}

#' Convert states to logical (TRUE/FALSE) sensor
#'
#' @description
#' This function creates a logical (TRUE/FALSE) sensor from specified states.
#'
#' @details
#' The function allows you to create a TRUE/FALSE sensor based on a tag. By default, 
#' it generates a new sensor by combining all tags specified in the `tag` parameter 
#' from all available sensors at a particular logger or locality. If you specify a 
#' `source_sensor`, the function converts only the tags from that specific sensor. 
#' You can also create multiple new sensors from multiple tags by specifying more 
#' values in `to_sensor` and providing exactly the same number of corresponding values 
#' in `source_sensor`. For example, you can create one TRUE/FALSE sensor from states 
#' on a temperature sensor and another from tags on a moisture sensor.
#' 
#' If you use parameter `inverse = TRUE` you get FALSE for each record where tag is assigned to and
#' FALSE for the records where tag is absent. By default you get TRUE for all the records
#' where tag is assigned.   
#'
#' @template param_myClim_object
#' @param tag The tag of states to be converted into a sensor.
#' @param to_sensor A vector of names for the output sensors.
#'      
#'      If `to_sensor` is a single sensor name, the logical sensor is created 
#'      from the union of states across all sensors with the same tag. If `to_sensor` 
#'      contains multiple sensor names, the length of the vector must match the length 
#'      of `source_sensor`.
#' @param source_sensor A vector of sensors containing the states to be converted into a new sensor. 
#' If NULL, states from all sensors are used. (default is NULL)
#' @param inverse A logical value. If TRUE, the sensor value is FALSE for state intervals (default is FALSE).
#' @return Returns a myClim object in the same format as the input, with added sensors.
#' @export
#' @examples
#' states <- data.frame(locality_id="A1E05", logger_index=1, sensor_name="Thermo_T", tag="error",
#'                      start=lubridate::ymd_hm("2020-10-28 9:00"),
#'                      end=lubridate::ymd_hm("2020-10-28 9:30"))
#' data <- mc_states_insert(mc_data_example_clean, states)
#' data <- mc_states_to_sensor(data, tag="error", to_sensor="error_sensor")
mc_states_to_sensor <- function(data, tag, to_sensor, source_sensor=NULL, inverse=FALSE) {
    .prep_check_datetime_step_unprocessed(data, stop)
    if(length(to_sensor) > 1 && (is.null(source_sensor) || length(source_sensor) != length(to_sensor))) {
        stop(.states_const_MESSAGE_SNSORS_LENGTH)
    }
    
    tag_value <- tag
    states_table <- dplyr::filter(mc_info_states(data), .data$tag == tag_value)
    states_table <- dplyr::select(states_table, "locality_id", "logger_index", "sensor_name", "start", "end")
    states_table <- dplyr::group_by(states_table, .data$locality_id, .data$logger_index)
    
    data_env <- new.env()
    data_env$data <- data
    state_function <- function(states_table, group) {
        if(length(to_sensor) == 1) {
            if(!is.null(source_sensor)) {
                states_table <- dplyr::filter(states_table, .data$sensor_name %in% source_sensor)
            }
            .states_add_logical_sensors(data_env, states_table, group, to_sensor, inverse)
        } else {
            purrr::walk2(to_sensor, source_sensor, function(.x, .y){
                states_table <- dplyr::filter(states_table, .data$sensor_name == .y)
                .states_add_logical_sensors(data_env, states_table, group, .x, inverse)  
            }) 
        }
    }
    
    dplyr::group_walk(states_table, .f=state_function)
    
    return(data_env$data)
}

.states_add_logical_sensors <- function(data_env, states_table, group, to_sensor, inverse) {
    sensor_values <- .states_get_values_from_states(data_env, states_table, group, inverse)
    new_sensor <- .common_get_new_sensor(mc_const_SENSOR_logical, to_sensor, values=sensor_values)
    if(.common_is_agg_format(data_env$data)) {
        data_env$data$localities[[group$locality_id]]$sensors[[to_sensor]] <- new_sensor
    } else {
        data_env$data$localities[[group$locality_id]]$loggers[[group$logger_index]]$sensors[[to_sensor]] <- new_sensor
    }
}

.states_get_values_from_states <- function(data_env, states_table, group, inverse) {
    intervals <- lubridate::interval(states_table$start, states_table$end)
    if(.common_is_agg_format(data_env$data)) {
        datetime <- data_env$data$localities[[group$locality_id]]$datetime
    } else {
        datetime <- data_env$data$localities[[group$locality_id]]$loggers[[group$logger_index]]$datetime
    }
    if(length(intervals) == 0) {
        return(rep(inverse, length(datetime)))
    }
    interval_values <- purrr::map(intervals, ~ lubridate::`%within%`(datetime, .x))
    result <- purrr::reduce(interval_values, `|`)
    if(inverse) {
        result <- !result
    }
    return(result)
}
#' Create states for outlying values
#' 
#' This function creates a state (tag) for all values that are either above 
#' or below certain thresholds (`min_value`, `max_value`), or at break 
#' points where consecutive values of microclimate time-series suddenly 
#' jump down or up (`positive_jump`, `negative_jump`).
#' 
#' @details 
#' The best way to use this function is to first generate a 
#' table (data.frame) with pre-defined minimum, maximum, and jump thresholds 
#' using the [mc_info_range] function. Then modify the thresholds as needed 
#' and apply the function (see example). All values above `max_value` and below 
#' `min_value` are tagged by default with the `range` tag. When consecutive 
#' values suddenly decrease by more than `negative_jump` or increase 
#' by more than `positive_jump`, such break points are tagged with the `jump` tag. 
#' It is possible to use only the `range` case, only the `jump` case, or both.
#' 
#' When the `period` parameter is used, the jump values are modified; 
#' range values are not affected. Depending on the logger step, the 
#' value of jump is multiplied or divided. For example, when the loggers
#' are recording with a step of 15 minutes (900 s) and the user sets
#' `period = "1 hour"` together with `positive_jump = 10`, then consecutive
#' values differing by (10 * (15 / 60) = 2.5) would be tagged. In this example,
#' but with recording step 2 hours (7200 s), consecutive values differing
#' by (10 * (120 / 60) = 20) would be tagged.
#'  
#'
#' @template param_myClim_object
#' @param table The table with outlying values (thresholds). You can use the output of [mc_info_range()]. The columns of the table are:
#' * `sensor_name` - Name of the sensor (e.g., TMS_T1, TMS_moist, HOBO_T); see [mc_data_sensors]
#' * `min_value` - Minimal value (threshold; all below are tagged)
#' * `max_value` - Maximal value 
#' * `positive_jump` - Maximal acceptable increase between two consecutive values (next value is higher than the previous)
#' * `negative_jump` - Maximal acceptable decrease between two consecutive values (next value is lower than the previous)
#' @param period Period for standardizing the value of jump. If NULL, then the difference is not standardized (default NULL); see details.
#' 
#' It is a character string usable by [lubridate::period], for example, "1 hour", "30 minutes", "2 days".
#' @param range_tag The tag for states indicating that the value is out of range (default "range").
#' @param jump_tag The tag for states indicating that the difference between two consecutive values is too high (default "jump").
#' @return Returns a myClim object in the same format as the input, with added states.
#' @export
#' @examples
#' range_table <- mc_info_range(mc_data_example_clean)
#' range_table$negative_jump[range_table$sensor_name == "TMS_moist"] <- 500
#' data <- mc_states_outlier(mc_data_example_clean, range_table)
mc_states_outlier <- function(data, table, period=NULL, range_tag="range", jump_tag="jump") {
    .prep_check_datetime_step_unprocessed(data, stop)
    is_agg_format <- .common_is_agg_format(data)
    if(!is.null(period)) {
        period <- lubridate::as.period(period)
    }
    step_period <- NULL
    if(is_agg_format) {
        step_period <- lubridate::as.period(.common_get_period_from_agg_data(data))
    }

    sensor_function <- function(sensor, datetime, step_period) {
        if(!(sensor$metadata@name %in% table$sensor_name)) {
            return(sensor)
        }
        condition <- table[[.states_const_COLUMN_SENSOR_NAME]] == sensor$metadata@name
        min_value <- table[[.states_const_COLUMN_MIN_VALUE]][condition]
        max_value <- table[[.states_const_COLUMN_MAX_VALUE]][condition]
        positive_jump <- table[[.states_const_COLUMN_POSITIVE_JUMP]][condition]
        negative_jump <- table[[.states_const_COLUMN_NEGATIVE_JUMP]][condition]
        sensor <- .states_add_out_of_range_state(sensor, datetime, min_value, max_value, range_tag)
        sensor <- .states_add_jump_state(sensor, datetime, step_period, period, positive_jump, negative_jump, jump_tag)
        return(sensor)
    }
    
    sensors_item_function <- function(item) {
        if(is_agg_format) {
            current_step_period <- step_period
        } else {
            current_step_period <- lubridate::seconds_to_period(item$clean_info@step)
        }
        item$sensors <- purrr::map(item$sensors, ~ sensor_function(.x, item$datetime, current_step_period))
        
        return(item)
    }

    locality_function <- function(locality) {
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

.states_add_out_of_range_state <- function(sensor, datetime, min_value, max_value, range_tag) {
    outlier_min <- rep(FALSE, length(sensor$values))
    outlier_max <- rep(FALSE, length(sensor$values))
    if(!is.na(min_value)) {
        outlier_min <- sensor$values < min_value
        outlier_min[is.na(outlier_min)] <- FALSE
    }
    if(!is.na(max_value)) {
        outlier_max <- sensor$values > max_value
        outlier_max[is.na(outlier_max)] <- FALSE
    }
    outlier <- outlier_min | outlier_max
    if(any(outlier)) {
        new_states_table <- .states_get_states_table_from_logical_values(outlier, datetime, range_tag)
        states_table <- dplyr::union(sensor$states, new_states_table)
        sensor$states <- states_table
    }
    return(sensor)
}

.states_add_jump_state <- function(sensor, datetime, step_period, period_value, positive_jump, negative_jump, jump_tag) {
    outlier_positive <- rep(FALSE, length(sensor$values))
    outlier_negative <- rep(FALSE, length(sensor$values))
    if(is.na(positive_jump) && is.na(negative_jump)) {
        return(sensor)
    }
    diff_values <- c(NA, diff(sensor$values))
    period_constant <- 1
    if(!is.null(period_value)) {
        period_constant <- step_period / period_value
    }
    if(!is.na(positive_jump)) {
        if(positive_jump < 0) {
            stop(.states_const_MESSAGE_NEGATIVE_JUMP)
        }
        outlier_positive <- diff_values > (positive_jump * period_constant)
        outlier_positive[is.na(outlier_positive)] <- FALSE
    }
    if(!is.na(negative_jump)) {
        if(negative_jump < 0) {
            stop(.states_const_MESSAGE_NEGATIVE_JUMP)
        }
        outlier_negative <- diff_values < (-1 * negative_jump * period_constant)
        outlier_negative[is.na(outlier_negative)] <- FALSE
    }
    outlier <- outlier_positive | outlier_negative
    if(any(outlier)) {
        new_states_table <- .states_get_states_table_from_logical_values(outlier, datetime, jump_tag)
        states_table <- dplyr::union(sensor$states, new_states_table)
        sensor$states <- states_table
    }
    return(sensor)
}