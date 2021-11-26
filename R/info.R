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

    logger_function <- function(logger) {
        count_env$sensors <- count_env$sensors + length(logger$sensors)
    }

    locality_function <- function(locality) {
        count_env$loggers <- count_env$loggers + length(locality$loggers)
        purrr::walk(locality$loggers, logger_function)
    }

    purrr::walk(data, locality_function)
    data.frame(item=c("localities", "loggers", "sensors"),
               count=c(count_env$localities, count_env$loggers, count_env$sensors))
}
