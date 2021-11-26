# constants ================================================================================

.clean_const_DETECT_STEP_LENGTH <- 100

#' @export
mc_const_CLEAN_DATETIME_STEP <- "datetime_step"
#' @export
mc_const_CLEAN_CROP <- "crop"

#' Cleaning datetime series
#'
#' This function change datetime and values series. Result series has constant
#' step without duplicits and missed values are filled in as NA.
#'
#' @param data character data in standard format
#' @return cleaned data in standard format
#' @export
#' @examples
#' cleaned_example_tomst_data1 <- mc_clean_datetime_step(example_tomst_data1)
mc_clean_datetime_step <- function(data) {
    logger_function <- function(logger) {
        .clean_datetime_step_logger(logger)
    }
    locality_function <- function(locality) {
        locality$loggers <- purrr::map(locality$loggers, logger_function)
        locality
    }
    purrr::map(data, locality_function)
}

.clean_datetime_step_logger <- function(logger) {
    logger$metadata@step <- .clean_detect_step_minutes(logger$datetime)
    logger$datetime <- lubridate::round_date(logger$datetime, stringr::str_glue("{logger$metadata@step} min"))
    logger <- .clean_datetime_step_log_wrong(logger)
    .clean_datetime_step_edit_series(logger)
}

.clean_detect_step_minutes <- function(datetime) {
    if(length(datetime) > .clean_const_DETECT_STEP_LENGTH) {
        datetime <- datetime[1:.clean_const_DETECT_STEP_LENGTH]
    }
    datetime <- as.numeric(datetime)
    round(unname(quantile(diff(datetime), p=0.5, type=1)/60))
}

.clean_add_method_to_clean_log_if_need <- function(clean_log, method) {
    if(!(method %in% names(clean_log))) {
        clean_log[[method]] <- character()
    }
    clean_log
}

.clean_add_log <- function(clean_log, method, message) {
    clean_log <- .clean_add_method_to_clean_log_if_need(clean_log, method)
    clean_log[[method]][[length(clean_log[[method]]) + 1]] <- message
    clean_log
}

.clean_datetime_step_edit_series <- function(logger) {
    if(!.clean_was_error_in_logger_datetime_step(logger)){
        return(logger)
    }
    table <- microclim:::.common_logger_values_as_tibble(logger)
    grouped_table <- dplyr::group_by(table, datetime)
    table_noduplicits <- dplyr::summarise_all(grouped_table, mean)
    datetime_range <- range(table_noduplicits$datetime)
    datetime_seq <- tibble::as_tibble(seq(datetime_range[[1]], datetime_range[[2]], by=stringr::str_glue("{logger$metadata@step} min")))
    colnames(datetime_seq) <- "datetime"
    output_table <- dplyr::left_join(datetime_seq, table_noduplicits, by="datetime")
    logger$datetime <- output_table$datetime
    sensor_names <- purrr::set_names(names(logger$sensors))
    logger$sensors <- purrr::map(sensor_names, function(x) {
        logger$sensors[[x]]$values <- output_table[[x]]
        logger$sensors[[x]]
    })
    logger
}

.clean_datetime_step_log_wrong <- function(logger) {
    .clean_add_method_to_clean_log_if_need(logger$clean_log, mc_const_CLEAN_DATETIME_STEP)
    diff_datetime <- tibble::as_tibble(diff(as.numeric(logger$datetime)) %/% 60)
    count_table <- dplyr::count(diff_datetime, value)
    wrong_diff <- dplyr::filter(count_table, value != logger$metadata@step)
    log_message_function <- function(value, n) {
        if(value == 0) {
            return(stringr::str_glue("data contains {n}x duplicits"))
        }
        stringr::str_glue("data contains {n}x {value} min gap")
    }
    new_items <- purrr::map2_chr(wrong_diff$value, wrong_diff$n, log_message_function)
    logger$clean_log[[mc_const_CLEAN_DATETIME_STEP]] <- c(logger$clean_log[[mc_const_CLEAN_DATETIME_STEP]], new_items)
    logger
}

.clean_is_logger_datetime_step_proceessed <- function(logger) {
    !is.na(logger$metadata@step)
}

.clean_was_error_in_logger_datetime_step <- function(logger) {
    length(logger$clean_log[[mc_const_CLEAN_DATETIME_STEP]]) > 1
}

.clean_warn_if_datetime_step_unprocessed <- function(logger) {
    if(!.clean_is_logger_datetime_step_proceessed(logger)){
        warning(stringr::str_glue("Detected step miss in logger {logger$metadata@type} {logger$metadata@serial_number}. Probably logger wasn't cleaned"))
    }
}

#' Get all clean log messages
#'
#' This function return dataframe with all clean log messages
#'
#' @param data in standard format
#' @return dataframe with columns locality_id, serial_number, clean_type, message
#' @export
#' @examples
#' log_table <- mc_clean_logs(cleaned_example_tomst_data1)
mc_clean_logs <- function(data) {
    logger_function <- function (logger) {
        log_function <- function(type, messages) {
            purrr::map(messages, function(message) c(logger$metadata@serial_number, type, message))
        }
        items <- purrr::map2(names(logger$clean_log), logger$clean_log, log_function)
        purrr::flatten(items)
    }

    locality_function <- function(locality) {
        items <- purrr::map(locality$loggers, logger_function)
        purrr::map(purrr::flatten(items), function(x) purrr::prepend(x, locality$metadata@locality_id))
    }

    rows <- purrr::flatten(purrr::map(data, locality_function))
    columns <- purrr::transpose(rows)
    data.frame(locality_id=unlist(columns[[1]]), serial_number=unlist(columns[[2]]),
               clean_type=unlist(columns[[3]]), message=unlist(columns[[4]]))
}

#' Set user defined TZ offset
#'
#' This function set user defined TZ offsets in localities
#'
#' @param data in standard format
#' @param tz_offsets named list (name: locality_id, item: tz_offset in rounded minutes)
#' @return data with changed TZ offset in standard format
#' @export
#' @examples
#' example_tomst_data2 <- mc_clean_solar_tz(example_tomst_data2, list(None=60))
mc_clean_user_tz <- function(data, tz_offsets) {
    for (locality_id in names(tz_offsets))
    {
        data[[locality_id]]$metadata@tz_offset <- tz_offsets[[locality_id]]
        data[[locality_id]]$metadata@tz_type <- microclim::mc_const_TZ_USER_DEFINED
    }
    data
}

#' Solar TZ offset
#'
#' This function compute TZ offset in localities by solar time
#'
#' @param data in standard format
#' @return data with changed TZ offset in standard format
#' @export
#' @examples
#' cleaned_example_tomst_data1 <- mc_clean_solar_tz(cleaned_example_tomst_data1)
mc_clean_solar_tz <- function(data) {
    locality_function <- function(locality) {
        if(is.na(locality$metadata@lon_wgs84)) {
            warning(stringr::str_glue("missing longitude in locality {locality$metadata@locality_id} - skip"))
            return(locality)
        }
        locality$metadata@tz_offset <- round(locality$metadata@lon_wgs84 / 180 * 12 * 60)
        locality$metadata@tz_type <- microclim::mc_const_TZ_SOLAR
        locality
    }

    purrr::map(data, locality_function)
}

.clean_warn_if_unset_tz_offset <- function(locality) {
    if(locality$metadata@tz_type == mc_const_TZ_UTC){
        warning(stringr::str_glue("TZ offset in locality {locality$metadata@locality_id} is not set - UTC used"))
    }
}

#' Crop datetime
#'
#' This function crop data by datetime
#'
#' @param data in standard format
#' @param start POSIXct datetime in UTC; is optional
#' @param end POSIXct datetime in UTC; is optional
#' @return cropped data in standard format
#' @export
#' @examples
#' cleaned_example_tomst_data1 <- mc_clean_crop(example_tomst_data1, end=as.POSIXct("2020-02-01"))
mc_clean_crop <- function(data, start=NULL, end=NULL) {
    if(!is.null(start) && format(start, format="%Z") != "UTC") {
        warning(stringr::str_glue("start datetime is not in UTC"))
    }
    if(!is.null(end) && format(end, format="%Z") != "UTC") {
        warning(stringr::str_glue("end datetime is not in UTC"))
    }
    logger_function <- function(logger) {
        table <- microclim:::.common_logger_values_as_tibble(logger)
        logger <- .clean_log_crop(logger, start, end)
        if(!is.null(start)) {
            table <- dplyr::filter(table, datetime >= start)
        }
        if(!is.null(end)) {
            table <- dplyr::filter(table, datetime <= end)
        }
        logger$datetime <- table$datetime
        logger$sensors <- purrr::map(logger$sensors, function(sensor) {
            sensor$values <- table[[sensor$metadata@sensor_id]]
            sensor})
        logger
    }

    locality_function <- function(locality) {
        locality$loggers <- purrr::map(locality$loggers, logger_function)
        locality
    }

    purrr::map(data, locality_function)
}

.clean_log_crop <- function(logger, start, end) {
    datetime_range <- range(logger$datetime)
    if(is.null(start)) {
        start <- datetime_range[[1]]
    }
    if(is.null(end)) {
        end <- datetime_range[[2]]
    }
    message <- stringr::str_glue("original datetime range {datetime_range[[1]]} - {datetime_range[[2]]} cropped to {start} - {end}")
    logger$clean_log <- .clean_add_log(logger$clean_log, mc_const_CLEAN_CROP, message)
    logger
}
