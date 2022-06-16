#' Joining sensors from different loggers
#'
#' @description
#' Function join sensors from different loggers. Loggers with same type [mc_LoggerMetadata] and step are joined.
#' Every sensore from first logger must have pair sensor from second one with same height.
#'
#' @details
#'
#' @param data myClim object in Prep-format. See [myClim-package]
#' @return myClim object with joined sensors.
#' @export
#' @examples
mc_join <- function(data) {
    myClim:::.common_stop_if_not_prep_format(data)
    myClim:::.prep_check_if_datetime_step_unprocessed(data, stop)
    purrr::map(data, .join_locality)
}

.join_locality <- function(locality) {
    types <- purrr::map_chr(locality$loggers, ~ .x$metadata@type)
    unique_types <- unique(types)
    type_function <- function(logger_type) {
        indexes <- which(types == logger_type)
        if(length(indexes) == 1) {
            return(locality$loggers[[indexes]])
        }
        .join_loggers_same_type(locality$loggers[indexes])
    }
    purrr::map(unique_types, type_function)
}

.join_loggers_same_type <- function(loggers) {
    steps <- purrr::map_int(loggers, ~ as.integer(.x$clean_info@step))
    shifts <- purrr::map_int(loggers, ~ as.integer(myClim:::.common_get_logger_shift(.x)))
    heights <- .join_get_heights(loggers)
    table <- tibble::tibble(logger_id=seq_along(loggers), steps=steps, shifts=shifts, heights=heights)
    table <- dplyr::group_by(table, steps, shifts, heights)
    group_function <- function(group) {
        purrr::reduce(loggers[group$logger_id], .join_loggers)
    }
    dplyr::group_map(table, group_function)
}

.join_get_heights <- function(loggers) {
    logger_function <- function(logger) {
        stringr::str_c(sort(purrr:map_chr(logger$sensors, ~ .x$metadata@height)), sep = ",")
    }

    purrr::map_chr(loggers, logger_function)
}

.join_loggers <- function(logger1, logger2) {
    if(logger2$datetime[[1]] < logger1$datetime[[1]]) {
        temp <- logger1
        logger1 <- logger2
        logger2 <- temp
    }
    start <- logger1$datetime[[1]]
    end <- max(dplyr::last(logger1$datetime), dplyr::last(logger2$datetime))
    table <- tibble::tibble(datetime = seq(start, end, logger1$clean_info@step * 60))
    names_table <- .join_get_names_table(logger1, logger2)
    l1_table <- myClim:::.common_sensor_values_as_tibble(logger1)
    colnames(l1_table) <- .join_get_logger_table_column_names(colnames(l1_table), names_table, TRUE)
    l2_table <- myClim:::.common_sensor_values_as_tibble(logger2)
    colnames(l2_table) <- .join_get_logger_table_column_names(colnames(l2_table), names_table, FALSE)
}

.join_get_names_table <- function(logger1, logger2){
    l1_names <- purrr::map_chr(logger1$sensors, ~ .x$metadata@name)
    l1_heights <- purrr::map_chr(logger1$sensors, ~ .x$metadata@height)
    l2_names <- purrr::map_chr(logger2$sensors, ~ .x$metadata@name)
    l2_heights <- purrr::map_chr(logger2$sensors, ~ .x$metadata@height)
    l1 <- tibble:tibble(height=l1_heights, l1_name=l1_names, l1_new_name=paste0("l1_", l1_names))
    l2 <- tibble:tibble(height=l2_heights, l2_name=l2_names, l2_new_name=paste0("l2_", l2_names))
    dplyr::left_join(l1, l2)
}

.join_get_logger_table_column_names <- function (old_names, names_table, is_l1) {
    search_column <- if(is_l1) "l1_name" else "l2_name"
    result_column <- if(is_l1) "l1_new_name" else "l2_new_name"

    new_name_function <- function (old_name) {
        if(old_name == "datetime") {
            return(old_name)
        }
        row_index <- which(names_table[[search_column]] == old_name)
        names_table[[result_column]][[row_index]]
    }

    purrr::map_chr(old_names, new_name_function)
}