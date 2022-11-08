#' Count data
#'
#' This function return data.frame with the number of localities, loggers and sensors of input myClim object. 
#'
#' @template param_myClim_object
#' @return data.frame with count of localities, loggers and sensors
#' @export
#' @examples
#' count_table <- mc_info_count(example_tomst_data1)
mc_info_count <- function(data) {
    count_env <- new.env()
    count_env$localities <- length(data$localities)
    count_env$loggers <- 0
    count_env$sensors <- 0

    sensors_item_function <- function(item) {
        count_env$sensors <- count_env$sensors + length(item$sensors)
    }

    raw_locality_function <- function(locality) {
        count_env$loggers <- count_env$loggers + length(locality$loggers)
        purrr::walk(locality$loggers, sensors_item_function)
    }

    if(myClim:::.common_is_agg_format(data)) {
        purrr::walk(data$localities, sensors_item_function)
    } else {
        purrr::walk(data$localities, raw_locality_function)
    }

    result <- data.frame(item=c("localities", "loggers", "sensors"),
                         count=c(count_env$localities, count_env$loggers, count_env$sensors))

    if(myClim:::.common_is_agg_format(data)) {
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
#' * step - detected time step in seconds of the logger measurements.
#' * count_duplicits - number of duplicated records (identical time and value)
#' * count_missed - number of missing records (logger outage in time it should record)
#' * count_disordered - number of records incorrectly ordered in time (newer followed by older)
#' * rounded - T/F indication whether myClim automatically rounded time series to the closes half (06, 12) e.g. 13:07 -> 13:00 
#' @seealso [myClim::mc_prep_clean()]
#' @export
mc_info_clean <- function(data) {
    myClim:::.common_stop_if_not_raw_format(data)

    logger_function <- function (logger) {
        list(logger$metadata@serial_number,
             min(logger$datetime),
             max(logger$datetime),
             logger$clean_info@step,
             logger$clean_info@count_duplicits,
             logger$clean_info@count_missed,
             logger$clean_info@count_disordered,
             logger$clean_info@rounded)
    }

    locality_function <- function(locality) {
        items <- purrr::map(locality$loggers, logger_function)
        purrr::map(items, function(x) purrr::prepend(x, locality$metadata@locality_id))
    }

    rows <- purrr::flatten(purrr::map(data$localities, locality_function))
    columns <- purrr::transpose(rows)
    data.frame(locality_id=unlist(columns[[1]]), serial_number=unlist(columns[[2]]),
               start_date=myClim:::.common_as_utc_posixct(unlist(columns[[3]])),
               end_date=myClim:::.common_as_utc_posixct(unlist(columns[[4]])),
               step=unlist(columns[[5]]), count_duplicits=unlist(columns[[6]]),
               count_missed=unlist(columns[[7]]), count_disordered=unlist(columns[[8]]),
               rounded=unlist(columns[[9]]))
}


#' Get sensors info table
#'
#' This function return data.frame with info about sensors
#'
#' @template param_myClim_object
#' @return data.frame with columns:
#' * locality_id 
#' * serial_number
#' * sensor_id
#' * sensor_name
#' * start_date - the oldest record on the sensor  
#' * end_date - the newest record on the sensor
#' * step - time step of records series (seconds)
#' * period - time step of records series (text)
#' * min_value - minimal recorded values
#' * max_value - maximal recorded value
#' * count_values - number of non NA records
#' * count_na - number of NA records
#' @export
#' @examples
#' mc_info(mc_data_example_calc)
mc_info <- function(data) {
    is_raw_format <- myClim:::.common_is_raw_format(data)

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
                       step=rep(step, count),
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
#' * altitude
#' * tz_offset
#' @export
#' @examples
#' mc_info_meta(mc_data_example_calc)
mc_info_meta <- function(data) {
    localities <- data$localities

    locality_function <- function (locality) {
        list(locality_id = locality$metadata@locality_id,
             lon_wgs84 = locality$metadata@lon_wgs84,
             lat_wgs84 = locality$metadata@lat_wgs84,
             altitude = locality$metadata@altitude,
             tz_offset = locality$metadata@tz_offset
        )
    }

    result <- purrr::map_dfr(localities, locality_function)
    as.data.frame(result)
}
