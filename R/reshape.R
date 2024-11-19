.reshape_const_MESSAGE_UNCLEANED <- "Logger {serial_number} isn't cleaned. I can't detect the last time_to."

#' Export values to wide table
#'
#' This function converts myClim object to the R data.frame with values of sensor in wide format.
#'
#' @details First column of the output data.frame is datetime followed by the
#' columns for every sensor. Name of the column is in format:
#' * localityid_loggerid_serialnumber_sensorname for Raw-format and `show_logger_name=FALSE`
#' * localityid_loggername_sensorname for Raw-format and `show_logger_name=TRUE`
#' * localityid_sensorname for Agg-format
#' 
#' The less complex wide table is returned when exporting single sensor ascross localities. 
#'
#' @template param_myClim_object
#' @template param_localities
#' @template param_sensors
#' @template param_use_utc
#' @param show_logger_name if TRUE, the logger name is included in the column name (default FALSE)
#' @return data.frame with columns:
#' * datetime 
#' * locality1_sensor1
#' * ...
#' * ...
#' * localityn_sensorn
#' 
#' @export
#' @examples
#' example_tms_wideformat <- mc_reshape_wide(mc_data_example_raw, c("A6W79", "A2E32"),
#'                                           c("TMS_T1", "TMS_T2"))
mc_reshape_wide <- function(data, localities=NULL, sensors=NULL, use_utc=TRUE, show_logger_name=FALSE) {
    data <- mc_filter(data, localities, sensors)
    if(.common_is_agg_format(data)) {
        use_utc <- .common_check_agg_use_utc(use_utc, data$metadata@period)
    }
    datetimes <- .reshape_get_all_datetimes(data, use_utc)
    tables <- c(tibble::tibble(datetimes), .reshape_get_sensor_tables(data, use_utc, show_logger_name))
    tables[[1]] <- tibble::as_tibble(tables[[1]])
    colnames(tables[[1]]) <- "datetime"
    as.data.frame(purrr::reduce(tables, function(.x, .y) dplyr::left_join(.x, .y, by="datetime")))
}

.reshape_get_all_datetimes <- function(data, use_utc) {
    is_agg_format <- .common_is_agg_format(data)

    locality_function <- function(locality) {
        tz_offset <- if(use_utc) 0 else locality$metadata@tz_offset
        if(is_agg_format) {
            return(.calc_get_datetimes_with_offset(locality$datetime, tz_offset))
        }
        datetimes <- purrr::map(locality$loggers, function(x) x$datetime)
        result <- purrr::reduce(datetimes, union)
        return(.calc_get_datetimes_with_offset(result, tz_offset))
    }
    locality_datetimes <- purrr::map(data$localities, locality_function)
    datetimes <- purrr::reduce(locality_datetimes, union)
    datetimes <- sort(datetimes)
    .common_as_utc_posixct(datetimes)
}

.reshape_get_sensor_tables <- function(data, use_utc, show_logger_name) {
    sensors_function <- function(item, name_prefix, tz_offset) {
        table <- .common_sensor_values_as_tibble(item)
        table$datetime <- .calc_get_datetimes_with_offset(table$datetime, tz_offset)
        colnames(table)[-1] <- purrr::map_chr(colnames(table)[-1], function(x) stringr::str_glue("{name_prefix}_{x}"))
        table
    }

    if(.common_is_agg_format(data)) {
        items <- list(
            item=data$localities,
            name_prefix=names(data$localities),
            tz_offset=purrr::map(data$localities, ~ if(use_utc) 0 else .x$metadata@tz_offset)
        )
        return(purrr::pmap(items, sensors_function))
    }

    raw_locality_function <- function(locality) {
        name_function <- function(logger, i) {
            parts <- locality$metadata@locality_id
            if(show_logger_name) {
                parts <- c(parts, logger$metadata@name)
            } else {
                parts <- c(parts, as.character(i))
                if(!is.na(logger$metadata@serial_number)) {
                    parts <- c(parts, logger$metadata@serial_number)
                }
            }
            paste0(parts, collapse="_")
        }

        prefixes <- purrr::map2_chr(locality$loggers, seq_along(locality$loggers), name_function)
        items <- list(
            item=locality$loggers,
            name_prefix=prefixes,
            tz_offset=if(use_utc) 0 else locality$metadata@tz_offset
        )
        purrr::pmap(items, sensors_function)
    }
    result <- purrr::map(data$localities, raw_locality_function)
    purrr::flatten(result)
}

#' Export values to long table
#'
#' This function converts myClim object to long R data.frame.
#'
#' @template param_myClim_object
#' @template param_localities
#' @template param_sensors
#' @template param_use_utc
#' @return data.frame
#'
#' columns:
#' * locality_id
#' * serial_number
#' * sensor_name
#' * height
#' * datetime
#' * time_to
#' * value
#' @export
#' @examples
#' head(mc_reshape_long(mc_data_example_clean, c("A6W79", "A2E32"), c("TMS_T1", "TMS_T2")), 10)
mc_reshape_long <- function(data, localities=NULL, sensors=NULL, use_utc=TRUE) {
    data <- mc_filter(data, localities, sensors)
    is_raw_format <- .common_is_raw_format(data)
    period <- NULL
    if (!is_raw_format && !(data$metadata@period %in% .agg_const_INTERVAL_PERIODS)) {
        period <- lubridate::period(data$metadata@period)
    }
    if(!is_raw_format) {
        use_utc <- .common_check_agg_use_utc(use_utc, data$metadata@period)
    }

    sensor_function <- function(locality_id, serial_number, sensor_item, datetime, time_to) {
        count <- length(datetime)
        tibble::tibble(locality_id=rep(locality_id, count),
                       serial_number=rep(serial_number, count),
                       sensor_name=rep(sensor_item$metadata@name, count),
                       height=rep(sensor_item$metadata@height, count),
                       datetime=datetime,
                       time_to=time_to,
                       value=sensor_item$values)
    }

    sensors_item_function <- function(locality_id, tz_offset, item) {
        serial_number <- NA_character_
        tz_offset <- if(use_utc) 0 else tz_offset
        datetime <- .calc_get_datetimes_with_offset(item$datetime, tz_offset)
        if(is_raw_format) {
            serial_number <- item$metadata@serial_number
            if(is.na(item$clean_info@step)) {
                warning(stringr::str_glue(.reshape_const_MESSAGE_UNCLEANED))
            }
            period <- lubridate::seconds(item$clean_info@step)
        }
        if(!is.null(period)) {
            time_to <- c(datetime[-1], dplyr::last(datetime) + period)
        } else {
            first_index <- match(dplyr::first(datetime), data$metadata@intervals_start)
            intervals_end <- data$metadata@intervals_end[seq(first_index, first_index + length(datetime) - 1)]
            time_to <- intervals_end + lubridate::seconds(1)
        }
        tables <- purrr::pmap_dfr(list(locality_id=locality_id, serial_number=serial_number,
                                       sensor_item=item$sensors, datetime=list(datetime),
                                       time_to=list(time_to)),
                                  sensor_function)
    }

    raw_locality_function <- function(locality) {
        purrr::pmap_dfr(list(locality_id=locality$metadata@locality_id,
                             tz_offset=locality$metadata@tz_offset,
                             item=locality$loggers), sensors_item_function)
    }

    if(is_raw_format) {
        result <- purrr::map_dfr(data$localities, raw_locality_function)
    } else {
        result <- purrr::pmap_dfr(list(locality_id=names(data$localities),
                                       tz_offset=purrr::map(data$localities, ~ .x$metadata@tz_offset),
                                       item=data$localities), sensors_item_function)
    }
    as.data.frame(result)
}


