#' Count data
#'
#' This function return data.frame with the number of localities, loggers and sensors of input myClim object. 
#'
#' @template param_myClim_object
#' @return data.frame with count of localities, loggers and sensors
#' @export
#' @examples
#' count_table <- mc_info_count(mc_data_example_raw)
mc_info_count <- function(data) {
    count_env <- .common_get_count_items(data)
    result <- data.frame(item=c("localities", "loggers", "sensors"),
                         count=c(count_env$localities, count_env$loggers, count_env$sensors))

    if(.common_is_agg_format(data)) {
        result <- result[-2, ]
    }
    result
}

#' Call cleaning log
#'
#' @description 
#' This function return data.frame with information from cleaning the loggers time series see [myClim::mc_prep_clean()] 
#' 
#' @template param_myClim_object_raw
#' @return data.frame with columns:
#' * locality_id - when provided by user then locality ID, when not provided identical with serial number
#' * logger_name - Logger name at the locality.
#' * serial_number - serial number of logger when provided or automatically detected from file name or header
#' * start_date - date of the first record on the logger
#' * end_date  - date of the last record on the logger
#' * step_seconds - detected time step in seconds of the logger measurements.
#' * count_duplicities - number of duplicated records (identical time)
#' * count_missing - number of missing records (logger outage in time when it should record)
#' * count_disordered - number of records incorrectly ordered in time (newer followed by older)
#' * rounded - T/F indication whether myClim automatically rounded time series minutes to the closes half (HH:00, HH:30) e.g. 13:07 -> 13:00 
#' @seealso [myClim::mc_prep_clean()]
#' @export
mc_info_clean <- function(data) {
    .common_stop_if_not_raw_format(data)

    logger_function <- function (logger) {
        list(serial_number = logger$metadata@serial_number,
             logger_name = logger$metadata@name,
             start_date = min(logger$datetime),
             end_date = max(logger$datetime),
             step_seconds = logger$clean_info@step,
             count_duplicities = logger$clean_info@count_duplicities,
             count_missing = logger$clean_info@count_missing,
             count_disordered = logger$clean_info@count_disordered,
             rounded = logger$clean_info@rounded)
    }

    locality_function <- function(locality) {
        locality_table <- purrr::map_dfr(locality$loggers, logger_function)
        locality_table$locality_id <- locality$metadata@locality_id
        locality_table <- dplyr::select(locality_table, "locality_id", dplyr::everything())
        return(locality_table)
    }

    result <- purrr::map_dfr(data$localities, locality_function)
    return(result)
}


#' Get sensors info table
#'
#' This function return data.frame with info about sensors
#'
#' @template param_myClim_object
#' @return data.frame with columns:
#' * locality_id - when provided by user then locality ID, when not provided identical with serial number
#' * serial_number - serial number of logger when provided or automatically detected from file name or header
#' * sensor_id - original sensor id (e.g.,"GDD", "HOBO_T" ,"TMS_T1", "TMS_T2")
#' * sensor_name - original sensor id if not modified, if renamed then new name (e.g.,"GDD5", "HOBO_T_mean" ,"TMS_T1_max", "my_sensor01")
#' * start_date - the oldest record on the sensor
#' * end_date - the newest record on the sensor
#' * step_seconds - time step of records series (seconds)
#' * period - time step of records series (text)
#' * min_value - minimal recorded values
#' * max_value - maximal recorded value
#' * count_values - number of non NA records
#' * count_na - number of NA records
#' @export
#' @examples
#' mc_info(mc_data_example_agg)
mc_info <- function(data) {
    is_raw_format <- .common_is_raw_format(data)

    function_with_check_empty <- function(values, f) {
        values <- values[!is.na(values)]
        if(length(values) == 0) {
            return(NA_real_)
        }
        f(values)
    }

    sensors_item_function <- function(locality_id, item, step, period) {
        serial_number <- NA_character_
        if(is_raw_format) {
            serial_number <- item$metadata@serial_number
            step <- as.integer(item$clean_info@step)
        }

        count <- length(item$sensors)

        tibble::tibble(locality_id=rep(locality_id, count),
                       serial_number=rep(serial_number, count),
                       sensor_id=purrr::map_chr(item$sensors, function(x) x$metadata@sensor_id),
                       sensor_name=names(item$sensors),
                       start_date=rep(min(item$datetime), count),
                       end_date=rep(max(item$datetime), count),
                       step_seconds=rep(step, count),
                       period=rep(period, count),
                       min_value=purrr::map_dbl(item$sensors, function(x) function_with_check_empty(x$values, min)),
                       max_value=purrr::map_dbl(item$sensors, function(x) function_with_check_empty(x$values, max)),
                       count_values=purrr::map_int(item$sensors, function(x) length(x$values[!is.na(x$values)])),
                       count_na=purrr::map_int(item$sensors, function(x) length(x$values[is.na(x$values)])))
    }

    prep_locality_function <- function(locality) {
        purrr::pmap_dfr(list(locality_id=locality$metadata@locality_id,
                             item=locality$loggers,
                             step=NA_integer_,
                             period=NA_character_),
                        sensors_item_function)
    }

    if(is_raw_format) {
        result <- purrr::map_dfr(data$localities, prep_locality_function)
    } else {
        result <- purrr::pmap_dfr(list(locality_id=names(data$localities),
                                       item=data$localities,
                                       step=as.integer(data$metadata@step),
                                       period=data$metadata@period),
                                  sensors_item_function)
    }
    as.data.frame(result)
}

#' Get localities metadata table
#'
#' This function return data.frame with localities metadata
#'
#' @template param_myClim_object
#' @return data.frame with columns:
#' * locality_id
#' * lon_wgs84
#' * lat_wgs84
#' * elevation
#' * tz_offset
#' @export
#' @examples
#' mc_info_meta(mc_data_example_agg)
mc_info_meta <- function(data) {
    localities <- data$localities

    locality_function <- function (locality) {
        list(locality_id = locality$metadata@locality_id,
             lon_wgs84 = locality$metadata@lon_wgs84,
             lat_wgs84 = locality$metadata@lat_wgs84,
             elevation = locality$metadata@elevation,
             tz_offset = locality$metadata@tz_offset
        )
    }

    result <- purrr::map_dfr(localities, locality_function)
    as.data.frame(result)
}

#' Get loggers info table
#' 
#' This function returns a data.frame with information about loggers. 
#' 
#' This function is designed to work only with
#' myClim objects in **Raw-format**, where the loggers are organized at localities.
#' In **Agg-format**, myClim objects do not support loggers; sensors are directly connected to the locality.
#' See [myClim-package]. `mc_info_logger` does not work in Agg-format.
#'
#' @template param_myClim_object_raw
#' @return A data.frame with the following columns:
#' * locality_id - If provided by the user, it represents the locality ID; if not provided, it is identical to the logger's serial number.
#' * logger_name - Logger name.
#' * serial_number - Serial number of the logger, either provided by the user or automatically detected from the file name or header.
#' * logger_type - Logger type.
#' * start_date - The oldest record on the logger.
#' * end_date - The newest record on the logger.
#' * step_seconds - Time step of the record series (in seconds).
#' @export
#' @examples
#' mc_info_logger(mc_data_example_raw)
mc_info_logger <- function(data) {
    .common_stop_if_not_raw_format(data)

    logger_function <- function(locality_id, logger) {
        step <- as.integer(logger$clean_info@step)

        return(
             list(locality_id=locality_id,
                  logger_name=logger$metadata@name,
                  serial_number=logger$metadata@serial_number,
                  logger_type=logger$metadata@type,
                  start_date=min(logger$datetime),
                  end_date=max(logger$datetime),
                  step_seconds=step))
    }

    locality_function <- function(locality) {
        purrr::map2_dfr(locality$metadata@locality_id,
                        locality$loggers,
                        logger_function)
    }

    result <- purrr::map_dfr(data$localities, locality_function)

    as.data.frame(result)
}

#' Get states (tags) info table
#'
#' This function return data.frame with information about sensor states (tags) see [myClim-package]
#'
#' This function is useful not only for inspecting actual states (tags) but also as 
#' a template for manually manipulating states (tags) in a table editor such as Excel. 
#' The output of `mc_info_states()` can be saved as a table, adjusted outside R (adding/removing/modifying rows),
#' and then read back into R to be used as input for [mc_states_insert] or [mc_states_update].
#'
#' @template param_myClim_object
#' @return data.frame with columns:
#' * locality_id - when provided by user then locality ID, when not provided identical with serial number
#' * logger_name - name of logger in myClim object at the locality (e.g., "Thermo_1", "TMS_2")
#' * sensor_name - sensor name either original (e.g., TMS_T1, T_C), or calculated/renamed (e.g., "TMS_T1_max", "my_sensor01")
#' * tag - category of state (e.g., "error", "source", "quality")  
#' * start - start datetime
#' * end - end datetime
#' * value - value of tag (e.g., "out of soil", "c:/users/John/tmsData/data_911235678.csv")
#' @export
#' @examples
#' mc_info_states(mc_data_example_raw)
mc_info_states <- function(data) {
    is_raw_format <- .common_is_raw_format(data)

    sensor_function <- function(locality_id, logger_name, sensor) {
        count <- nrow(sensor$states)
        if(count == 0) {
            return(tibble::tibble())
        }
        result <- tibble::tibble(locality_id=rep(locality_id, count),
                                 logger_name=rep(logger_name, count),
                                 sensor_name=rep(sensor$metadata@name),
                                 tag=sensor$states$tag,
                                 start=sensor$states$start,
                                 end=sensor$states$end,
                                 value=sensor$states$value)
        return(result)
    }

    sensors_item_function <- function(locality_id, logger_name, item) {
        count <- length(item$sensors)
        purrr::pmap_dfr(list(locality_id=rep(locality_id, count),
                             logger_name=rep(logger_name, count),
                             sensor=item$sensors),
                        sensor_function)
    }

    raw_locality_function <- function(locality) {
        purrr::pmap_dfr(list(locality_id=locality$metadata@locality_id,
                             logger_name=names(locality$loggers),
                             item=locality$loggers),
                        sensors_item_function)
    }

    if(is_raw_format) {
        result <- purrr::map_dfr(data$localities, raw_locality_function)
    } else {
        result <- purrr::pmap_dfr(list(locality_id=names(data$localities),
                                       logger_name=NA_character_,
                                       item=data$localities),
                                  sensors_item_function)
    }
    as.data.frame(result)
}

#' Get table of sensors range
#'
#' This function return data.frame with sensors range (min value, max value) and possible jumps.
#'
#' This function is mainly useful to prepare input parameter for [mc_states_outlier()] function.
#' The range values are taken from `mc_data_sensors`. Those are manually defined 
#' ranges based on logger/sensor technical limits and biologically meaningful values.
#'
#' @template param_myClim_object
#' @return data.frame with columns:
#' * sensor_name - name of sensor (e.g., TMS_T1, TMS_moist, HOBO_T) see [mc_data_sensors]
#' * min_value - minimal value
#' * max_value - maximal value
#' * positive_jump - Maximal difference between two consecutive values. Next value is higher than previous. (Positive number)
#' * negative_jump - Maximal difference between two consecutive values. Next value is lower than previous. (Positive number)
#' @export
#' @examples
#' mc_info_range(mc_data_example_raw)
mc_info_range <- function(data) {
    is_raw_format <- .common_is_raw_format(data)

    sensor_types <- new.env()

    sensor_function <- function(sensor) {
        if(exists(sensor$metadata@sensor_id, sensor_types)) {
            return()
        }
        if(exists(sensor$metadata@sensor_id, myClim::mc_data_sensors)) {
            sensor_types[[sensor$metadata@name]] <- myClim::mc_data_sensors[[sensor$metadata@sensor_id]]
        }
    }

    sensors_item_function <- function(item) {
        purrr::walk(item$sensors, sensor_function)
    }

    locality_function <- function(locality) {
        purrr::walk(locality$loggers, sensors_item_function)
    }

    if(is_raw_format) {
        purrr::walk(data$localities, locality_function)
    } else {
        purrr::walk(data$localities, sensors_item_function)
    }

    row_function <- function(sensor_name) {
        result <- list()
        result[[.states_const_COLUMN_SENSOR_NAME]] <- sensor_name
        result[[.states_const_COLUMN_MIN_VALUE]] <- NA
        result[[.states_const_COLUMN_MAX_VALUE]] <- NA
        result[[.states_const_COLUMN_POSITIVE_JUMP]] <- NA
        result[[.states_const_COLUMN_NEGATIVE_JUMP]] <- NA
        if(is.null(sensor_types[[sensor_name]])) {
            return(result)
        }
        sensor_id <- sensor_types[[sensor_name]]@sensor_id
        sensor <- myClim::mc_data_sensors[[sensor_id]]

        result[[.states_const_COLUMN_MIN_VALUE]] <- sensor@min_value
        result[[.states_const_COLUMN_MAX_VALUE]] <- sensor@max_value
        return(result)
    }
    result <- purrr::map_dfr(names(sensor_types), row_function)

    as.data.frame(result)
}