# constants ================================================================================

.prep_const_DETECT_STEP_LENGTH <- 100

#' Cleaning datetime series
#'
#' This function change datetime and values series. Result series has constant
#' step without duplicits and missed values are filled in as NA.
#'
#' @param data in format for preparing
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
    steps <- paste(unique(info_table$step), collapse = ", ")
    print(stringr::str_glue("detected steps: {steps}"))
    print.data.frame(info_table)
    result
}

.prep_clean_logger <- function(logger) {
    logger$clean_info@step <- .prep_detect_step_minutes(logger$datetime)
    if(is.na(logger$clean_info@step)) {
        warning(stringr::str_glue("step cannot be detected for logger {logger$metadata@serial_number} - skip"))
        return(logger)
    }
    logger$datetime <- lubridate::round_date(logger$datetime, stringr::str_glue("{logger$clean_info@step} min"))
    logger <- .prep_clean_write_info(logger)
    logger <- .prep_clean_edit_series(logger)
    logger
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
    table <- myClim:::.common_sensor_values_as_tibble(logger)
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
        loggers_text <- paste(unprocessed_loggers, collapse=", ")
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
        data[[locality_id]]$metadata@tz_type <- myClim::mc_const_TZ_USER_DEFINED
    }
    data
}

#' Solar TZ offset
#'
#' This function compute TZ offset in localities by solar time
#'
#' The function require filled longitude of locality in slot lon_wgs84 of metadata.
#' TZ offset in minutes is calculated as `longitude / 180 * 12 * 60`.
#'
#' @param data in format for preparing or calculation
#' @return data with changed TZ offset in same format as input data
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
        locality$metadata@tz_type <- myClim::mc_const_TZ_SOLAR
        locality
    }

    localities <- purrr::map(myClim:::.common_get_localities(data), locality_function)
    myClim:::.common_set_localities(data, localities)
}

.prep_get_utc_localities <- function(data) {
    items <- purrr::keep(myClim:::.common_get_localities(data), function(x) x$metadata@tz_type == mc_const_TZ_UTC)
    unname(purrr::map_chr(items, function(x) x$metadata@locality_id))
}

.prep_warn_if_unset_tz_offset <- function(data) {
    utc_localities <- .prep_get_utc_localities(data)
    if(length(utc_localities) > 0){
        localities_text <- paste(utc_localities, collapse=", ")
        warning(stringr::str_glue("TZ offset in localities {localities_text} is not set - UTC used."))
    }
}

#' Crop datetime
#'
#' This function crop data by datetime
#'
#' @param data in format for preparing or calculation
#' @param start POSIXct datetime in UTC; is optional; start datetime is included
#' @param end POSIXct datetime in UTC; is optional
#' @param end_included if TRUE then  end datetime is included (default TRUE)
#' @return cropped data in standard format
#' @export
#' @examples
#' cleaned_example_tomst_data1 <- mc_prep_crop(example_tomst_data1, end=as.POSIXct("2020-02-01", tz="UTC"))
mc_prep_crop <- function(data, start=NULL, end=NULL, end_included=TRUE) {
    if(!is.null(start) && format(start, format="%Z") != "UTC") {
        warning(stringr::str_glue("start datetime is not in UTC"))
    }
    if(!is.null(end) && format(end, format="%Z") != "UTC") {
        warning(stringr::str_glue("end datetime is not in UTC"))
    }

    sensors_item_function <- function(item) {
        .prep_crop_data(item, start, end, end_included)
    }

    prep_locality_function <- function(locality) {
        locality$loggers <- purrr::map(locality$loggers, sensors_item_function)
        locality
    }

    if(myClim:::.common_is_calc_format(data)) {
        data$localities <- purrr::map(data$localities, sensors_item_function)
        return(data)
    } else {
        return(purrr::map(data, prep_locality_function))
    }
}

.prep_crop_data <- function(item, start, end, end_included) {
    table <- myClim:::.common_sensor_values_as_tibble(item)
    if(!is.null(start)) {
        table <- dplyr::filter(table, datetime >= start)
    }
    if(!is.null(end)) {
        table <- dplyr::filter(table, datetime < end | (end_included & datetime == end))
    }
    item$datetime <- table$datetime
    item$sensors <- purrr::map(item$sensors, function(sensor) {
        sensor$values <- table[[sensor$metadata@name]]
        sensor})
    item
}

#' Rename sensor
#'
#' This function rename sensors. It is usefull for flatting data format.
#'
#' @param data in format for preparing or calculation
#' @param sensor_names list with new names of sensors; names of items are old ones
#' @param localities vector of locality_ids; if NULL than all (default NULL)
#' @param serial_numbers vector of serial_numbers; if NULL than all (default NULL); parameter is usefull only for
#' preparing format of data
#' @return data with changed sensor names
#' @export
mc_prep_rename_sensor <- function(data, sensor_names, localities=NULL, serial_numbers=NULL) {
    is_calc_format <- myClim:::.common_is_calc_format(data)

    locality_function <- function(locality) {
        if(!(is.null(localities) || locality$metadata@locality_id %in% localities)) {
            return(locality)
        }
        if(is_calc_format) {
            return(.prepare_process_sensor_renaming(locality, sensor_names))
        }
        .prepare_process_sensor_renaming_in_loggers(locality, serial_numbers, sensor_names)
    }

    localities <- purrr::map(myClim:::.common_get_localities(data), locality_function)
    myClim:::.common_set_localities(data, localities)
}

.prepare_process_sensor_renaming <- function(item, sensor_names) {
    is_changed <- FALSE
    for(old_name in names(sensor_names)) {
        if(old_name %in% names(item$sensors)) {
            item$sensors[[old_name]]$metadata@name <- sensor_names[[old_name]]
            is_changed <- TRUE
        }
    }
    if(is_changed) {
        names(item$sensors) <- purrr::map_chr(item$sensors, function(x) x$metadata@name)
    }
    item
}

.prepare_process_sensor_renaming_in_loggers <- function(locality, serial_numbers, sensor_names) {
    logger_function <- function(logger) {
        if(!(is.null(serial_numbers) || logger$metadata@serial_number %in% serial_numbers)) {
            return(logger)
        }
        .prepare_process_sensor_renaming(logger, sensor_names)
    }
    locality$loggers <- purrr::map(locality$loggers, logger_function)
    locality
}

#' Merge data
#'
#' @description
#' This function merge two instances of data to one
#'
#' @details
#' If data1 and data2 contains locality with same locality_id, than locality_id from data2 is renamed.
#'
#' @param data1 in format for preparing or calculation
#' @param data2 in format for preparing or calculation but same as data1
#' @return merged data
#' @export
mc_prep_merge <- function(data1, data2) {
    .prep_merge_check_data(data1, data2)

    localities1 <- unname(myClim:::.common_get_localities(data1))
    localities2 <- unname(myClim:::.common_get_localities(data2))
    localities <- c(localities1, localities2)
    existed <- new.env()
    existed$ids <- list()

    locality_function <- function(locality) {
        locality_name <- .prep_merge_get_locality_id(locality$metadata@locality_id, existed$ids)
        locality$metadata@locality_id <- locality_name
        existed$ids <- c(existed$ids, locality_name)
        locality
    }

    localities <- purrr::map(localities, locality_function)
    names(localities) <- purrr::map_chr(localities, ~ .x$metadata@locality_id)
    if(myClim:::.common_is_prep_format(data1)) {
        return(localities)
    }
    data1$localities <- localities
    data1
}

.prep_merge_check_data <- function(data1, data2) {
    is_data1_calc_format <- myClim:::.common_is_calc_format(data1)
    is_data2_calc_format <- myClim:::.common_is_calc_format(data2)

    if(xor(is_data1_calc_format, is_data2_calc_format)) {
        stop("There is different format in data1 and data2.")
    }

    if(is_data1_calc_format &&
        (lubridate::as.period(data1$metadata@step_text) != lubridate::as.period(data2$metadata@step_text))) {
        stop("There is different step in data1 and data2.")
    }
}

.prep_merge_get_locality_id <- function(original_locality_name, existed_names) {
    locality_name <- original_locality_name
    number <- 1
    while(locality_name %in% existed_names) {
        locality_name <- stringr::str_glue("{original_locality_name}_{number}")
        number <- number + 1
    }
    if(locality_name != original_locality_name) {
        warning(stringr::str_glue("locality {original_locality_name} is renamed to {locality_name}"))
    }
    locality_name
}

