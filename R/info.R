#' count data
#'
#' This function return count of localities, loggers and sensors
#'
#' @param data in standard format
#' @return data.frame with count localities, loggers and sensors
#' @export
#' @examples
#' count_table <- mc_info_count(example_tomst_data1)
mc_info_count <- function(data) {
    count_env <- new.env()
    count_env$localities <- length(data)
    count_env$loggers <- 0
    count_env$sensors <- 0

    sensors_item_function <- function(item) {
        count_env$sensors <- count_env$sensors + length(item$sensors)
    }

    prep_locality_function <- function(locality) {
        count_env$loggers <- count_env$loggers + length(locality$loggers)
        purrr::walk(locality$loggers, sensors_item_function)
    }

    if(microclim:::.common_is_calc_format(data)) {
        purrr::walk(data, sensors_item_function)
    } else {
        purrr::walk(data, prep_locality_function)
    }

    result <- data.frame(item=c("localities", "loggers", "sensors"),
                         count=c(count_env$localities, count_env$loggers, count_env$sensors))

    if(microclim:::.common_is_calc_format(data)) {
        result <- result[-2, ]
    }
    result
}

#' Get all clean table
#'
#' This function return dataframe with all clean info about loggers
#'
#' @param data in standard format
#' @return dataframe with columns locality_id, serial_number, start_date, end_date, step, count_duplicits, count_missed, count_disordered
#' @export
#' @examples
#' log_table <- mc_prep_logs(cleaned_example_tomst_data1)
mc_info_clean <- function(data) {
    microclim:::.common_stop_if_not_prep_format(data)

    logger_function <- function (logger) {
        list(logger$metadata@serial_number,
             min(logger$datetime),
             max(logger$datetime),
             logger$clean_info@step,
             logger$clean_info@count_duplicits,
             logger$clean_info@count_missed,
             logger$clean_info@count_disordered)
    }

    locality_function <- function(locality) {
        items <- purrr::map(locality$loggers, logger_function)
        purrr::map(items, function(x) purrr::prepend(x, locality$metadata@locality_id))
    }

    rows <- purrr::flatten(purrr::map(data, locality_function))
    columns <- purrr::transpose(rows)
    data.frame(locality_id=unlist(columns[[1]]), serial_number=unlist(columns[[2]]),
               start_date=microclim:::.common_as_utc_posixct(unlist(columns[[3]])),
               end_date=microclim:::.common_as_utc_posixct(unlist(columns[[4]])),
               step=unlist(columns[[5]]), count_duplicits=unlist(columns[[6]]),
               count_missed=unlist(columns[[7]]), count_disordered=unlist(columns[[8]]))
}

