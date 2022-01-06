#' count data
#'
#' This function return count of localities, loggers and sensors
#'
#' @param data in format for preparing or calculation
#' @return data.frame with count localities, loggers and sensors
#' @export
#' @examples
#' count_table <- mc_info_count(example_tomst_data1)
mc_info_count <- function(data) {
    count_env <- new.env()
    count_env$localities <- length(.common_get_localities(data))
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
        purrr::walk(data$localities, sensors_item_function)
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

#' get clean info table
#'
#' This function return dataframe with info about cleaning loggers
#'
#' @param data in format for preparing
#' @return dataframe with columns locality_id, serial_number, start_date, end_date, step, count_duplicits, count_missed, count_disordered
#' @export
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


#' get sensors info table
#'
#' This function return dataframe with info about sensors
#'
#' @param data in format for preparing or calculation
#' @return dataframe with columns locality_id, serial_number, sensor_id, sensor_name, start_date, end_date, step, min_value, max_value, count_values, count_na
#' @export
mc_info <- function(data) {
    is_prep_format <- microclim:::.common_is_prep_format(data)

    sensors_item_function <- function(locality_id, item, step) {
        serial_number <- NA_character_
        if(is_prep_format) {
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
                       min_value=purrr::map_dbl(item$sensors, function(x) min(x$values, na.rm=TRUE)),
                       max_value=purrr::map_dbl(item$sensors, function(x) max(x$values, na.rm=TRUE)),
                       count_values=purrr::map_int(item$sensors, function(x) length(x$values[!is.na(x$values)])),
                       count_na=purrr::map_int(item$sensors, function(x) length(x$values[is.na(x$values)])))
    }

    prep_locality_function <- function(locality) {
        purrr::pmap_dfr(list(locality_id=locality$metadata@locality_id,
                             item=locality$loggers,
                             step=NA_integer_),
                        sensors_item_function)
    }

    if(is_prep_format) {
        result <- purrr::map_dfr(data, prep_locality_function)
    } else {
        result <- purrr::pmap_dfr(list(locality_id=names(data$localities),
                                       item=data$localities,
                                       step=as.integer(data$metadata@step)),
                                  sensors_item_function)
    }
    result
}

