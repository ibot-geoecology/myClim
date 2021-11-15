# constants ================================================================================

.clean_const_DETECT_STEP_LENGTH <- 100

#' @export
mc_const_CLEAN_DATETIME_STEP <- "datetime_step"

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

.add_method_to_clean_log_if_need <- function(clean_log, method) {
    if(!(method %in% names(clean_log))) {
        clean_log[[method]] <- character()
    }
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
    .add_method_to_clean_log_if_need(logger$clean_log,  mc_const_CLEAN_DATETIME_STEP)
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
#' @param data character data in standard format
#' @return dataframe with columns locality_id, serial_number, clean_type, message
#' @export
#' @examples
#' cleaned_example_tomst_data1 <- mc_clean_datetime_step(example_tomst_data1)
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
        purrr::map(purrr::flatten(items), function(x) purrr::prepend(x, locality$metadata@id))
    }

    rows <- purrr::flatten(purrr::map(data, locality_function))
    columns <- purrr::transpose(rows)
    data.frame(locality_id=unlist(columns[[1]]), serial_number=unlist(columns[[2]]),
               clean_type=unlist(columns[[3]]), message=unlist(columns[[4]]))
}

