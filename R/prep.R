# constants ================================================================================

.prep_const_DETECT_STEP_LENGTH <- 100

#' Cleaning datetime series
#'
#' This function change datetime and values series. Result series has constant
#' step without duplicits and missed values are filled in as NA.
#'
#' @param data character data in standard format
#' @param silent if true, then informations aren't printed (default FALSE)
#' @return cleaned data in standard format
#' @export
#' @examples
#' cleaned_example_tomst_data1 <- mc_prep_clean(example_tomst_data1)
mc_prep_clean <- function(data, silent=FALSE) {
    logger_function <- function(logger) {
        .prep_clean_logger(logger)
    }
    locality_function <- function(locality) {
        locality$loggers <- purrr::map(locality$loggers, logger_function)
        locality
    }
    result <- purrr::map(data, locality_function)
    if(silent) {
        return(result)
    }
    info_table <- mc_info_clean(result)
    count_loggers <- nrow(info_table)
    print(stringr::str_glue("{count_loggers} loggers"))
    start_date <- min(info_table$start_date)
    end_date <- max(info_table$end_date)
    print(stringr::str_glue("datetime range: {start_date} - {end_date}"))
    steps <- paste(unique(info_table$step), sep = ", ", collapse = "")
    print(stringr::str_glue("detected steps: {steps}"))
    print.data.frame(info_table)
    result
}

.prep_clean_logger <- function(logger) {
    logger$clean_info@step <- .prep_detect_step_minutes(logger$datetime)
    logger$datetime <- lubridate::round_date(logger$datetime, stringr::str_glue("{logger$clean_info@step} min"))
    logger <- .prep_clean_write_info(logger)
    logger <- .prep_clean_edit_series(logger)
}

.prep_detect_step_minutes <- function(datetime) {
    datetime <- tail(datetime, .prep_const_DETECT_STEP_LENGTH)
    datetime <- sort(as.numeric(datetime))
    diff_datetime <- Filter(function(x) x > 0, diff(datetime))
    round(unname(quantile(diff_datetime, p=0.5, type=1)/60))
}

.prep_clean_write_info <- function(logger) {
    diff_datetime <- diff(as.numeric(logger$datetime))
    logger$clean_info@count_disordered <- length(purrr::keep(diff_datetime, function(x) x < 0))
    sorted_datetime <- sort(as.numeric(logger$datetime))
    diff_datetime <- diff(sorted_datetime) %/% 60
    logger$clean_info@count_duplicits <- length(purrr::keep(diff_datetime, function(x) x == 0))
    right_count_datetime <- diff(c(sorted_datetime[[1]], tail(sorted_datetime, n=1))) %/% 60 %/% logger$clean_info@step + 1
    logger$clean_info@count_missed <- right_count_datetime - (length(logger$datetime) - logger$clean_info@count_duplicits)
    logger
}

.prep_clean_edit_series <- function(logger) {
    if(!.prep_clean_was_error_in_logger(logger)){
        return(logger)
    }
    table <- microclim:::.common_logger_values_as_tibble(logger)
    table <- dplyr::arrange(table, datetime)
    grouped_table <- dplyr::group_by(table, datetime)
    table_noduplicits <- dplyr::summarise_all(grouped_table, dplyr::first)
    datetime_range <- range(table_noduplicits$datetime)
    datetime_seq <- tibble::as_tibble(seq(datetime_range[[1]], datetime_range[[2]], by=stringr::str_glue("{logger$clean_info@step} min")))
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

.prep_is_logger_cleaned <- function(logger) {
    !is.na(logger$clean_info@step)
}

.prep_clean_was_error_in_logger <- function(logger) {
    is_ok <- c(logger$clean_info@count_disordered == 0,
               logger$clean_info@count_duplicits == 0,
               logger$clean_info@count_missed == 0)
    !all(is_ok)
}

.prep_get_uncleaned_loggers <- function(data) {
    locality_function <- function(locality) {
        unprocessed <- purrr::discard(locality$loggers, .prep_is_logger_cleaned)
        purrr::map_chr(unprocessed, function(x) x$metadata@serial_number)
    }
    loggers <- purrr::map(data, locality_function)
    purrr::reduce(loggers, c)
}

.prep_warn_if_datetime_step_unprocessed <- function(data) {
    unprocessed_loggers <- .prep_get_uncleaned_loggers(data)
    if(length(unprocessed_loggers) > 0){
        loggers_text <- paste(unprocessed_loggers, sep=", ", collapse="")
        warning(stringr::str_glue("Detected missed step in loggers {loggers_text}. Probably loggers weren't cleaned."))
    }
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
#' example_tomst_data2 <- mc_prep_solar_tz(example_tomst_data2, list(`91184101`=60))
mc_prep_user_tz <- function(data, tz_offsets) {
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
#' cleaned_example_tomst_data1 <- mc_prep_solar_tz(cleaned_example_tomst_data1)
mc_prep_solar_tz <- function(data) {
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

.prep_get_utc_localities <- function(data) {
    items <- purrr::keep(data, function(x) x$metadata@tz_type == mc_const_TZ_UTC)
    unname(purrr::map_chr(items, function(x) x$metadata@locality_id))
}

.prep_warn_if_unset_tz_offset <- function(data) {
    utc_localities <- .prep_get_utc_localities(data)
    if(length(utc_localities) > 0){
        localities_text <- paste(utc_localities, sep=", ", collapse="")
        warning(stringr::str_glue("TZ offset in localities {localities_text} is not set - UTC used."))
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
#' cleaned_example_tomst_data1 <- mc_prep_crop(example_tomst_data1, end=as.POSIXct("2020-02-01"))
mc_prep_crop <- function(data, start=NULL, end=NULL) {
    if(!is.null(start) && format(start, format="%Z") != "UTC") {
        warning(stringr::str_glue("start datetime is not in UTC"))
    }
    if(!is.null(end) && format(end, format="%Z") != "UTC") {
        warning(stringr::str_glue("end datetime is not in UTC"))
    }
    logger_function <- function(logger) {
        table <- microclim:::.common_logger_values_as_tibble(logger)
        logger <- .prep_log_crop(logger, start, end)
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

.prep_log_crop <- function(logger, start, end) {
    datetime_range <- range(logger$datetime)
    if(is.null(start)) {
        start <- datetime_range[[1]]
    }
    if(is.null(end)) {
        end <- datetime_range[[2]]
    }
    logger
}
