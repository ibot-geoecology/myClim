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
        list(logger$metadata@serial_number,
             min(logger$datetime),
             max(logger$datetime),
             logger$clean_info@step,
             logger$clean_info@count_duplicities,
             logger$clean_info@count_missing,
             logger$clean_info@count_disordered,
             logger$clean_info@rounded)
    }

    locality_function <- function(locality) {
        items <- purrr::map(locality$loggers, logger_function)
        purrr::map(items, ~ append(.x, locality$metadata@locality_id, after=0))
    }

    rows <- purrr::flatten(purrr::map(data$localities, locality_function))
    columns <- purrr::transpose(rows)
    data.frame(locality_id=unlist(columns[[1]]), serial_number=unlist(columns[[2]]),
               start_date=.common_as_utc_posixct(unlist(columns[[3]])),
               end_date=.common_as_utc_posixct(unlist(columns[[4]])),
               step_seconds=unlist(columns[[5]]), count_duplicities=unlist(columns[[6]]),
               count_missing=unlist(columns[[7]]), count_disordered=unlist(columns[[8]]),
               rounded=unlist(columns[[9]]))
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
#' * index - Logger index at the locality.
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

    logger_function <- function(locality_id, logger_index, logger) {
        step <- as.integer(logger$clean_info@step)

        return(
             list(locality_id=locality_id,
                  index=logger_index,
                  serial_number=logger$metadata@serial_number,
                  logger_type=logger$metadata@type,
                  start_date=min(logger$datetime),
                  end_date=max(logger$datetime),
                  step_seconds=step))
    }

    locality_function <- function(locality) {
        purrr::pmap_dfr(list(
                        locality_id = locality$metadata@locality_id,
                        logger_index = seq_along(locality$loggers),
                        logger = locality$loggers),
                        logger_function)
    }

    result <- purrr::map_dfr(data$localities, locality_function)

    as.data.frame(result)
}

#' Get joining info table
#'
#' This function returns a data.frame that contains information about the join operations.
#' Although this function performs the join process, it only returns an overview table,
#' not the actual joined data.
#'
#' This function is designed to work only with
#' myClim objects in **Raw-format**, where the loggers are organized at localities.
#' In **Agg-format**, myClim objects do not support loggers; sensors are directly connected to the locality.
#' See [myClim-package]. `mc_info_join` does not work in Agg-format.
#'
#' @template param_myClim_object_raw
#' @param comp_sensors parameter for [mc_join()] function (default NULL)
#' @return A data.frame with the following columns:
#' * locality_id - The ID of the locality.
#' * count_loggers - Number of loggers before the join operation.
#' * count_joined_loggers - Number of loggers after the join operation.
#' * count_data_conflicts - Number of different values in overlapping sensors.
#' * count_errors - Number of join-related errors. An error occurs when all sensors of the loggers have different names.
#' @export
mc_info_join <- function(data, comp_sensors=NULL) {
    localities <- as.list(.join_main(data, comp_sensors, TRUE))
    param_df <- purrr::map_dfr(localities, ~ .x)
    result <- data.frame(locality_id=names(localities))
    result[colnames(param_df)] <- param_df
    for(colname in colnames(param_df)) {
        result[[colname]] <- as.integer(result[[colname]])
    }
    return(result)
}
