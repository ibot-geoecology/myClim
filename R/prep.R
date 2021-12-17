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
    table <- microclim:::.common_sensor_values_as_tibble(logger)
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
        localities_text <- paste(utc_localities, collapse=", ")
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
        table <- microclim:::.common_sensor_values_as_tibble(logger)
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

#' Flattening data
#'
#' This function flatten data. Logger lever from data hierarchy is deleted.
#' Sensors are moved to locality and datetimes are merged to one series.
#'
#' @param data in format for preparing
#' @return flattened data in format for calculation
#' @export
#' @examples
#' calc_data <- mc_prep_flat(example_cleaned_tomst_data1)
mc_prep_flat <- function(data) {
    locality_function <- function(locality) {
        steps <- purrr::map_int(locality$loggers, function(.x) as.integer(.x$clean_info@step))
        if(any(is.na(steps))){
            stop(stringr::str_glue("Some Logger in locality {locality$metadata@locality_id} has NA step."))
        }
        if(length(steps) > 1 && var(steps) != 0) {
            stop(stringr::str_glue("Loggers in locality {locality$metadata@locality_id} has different step."))
        }

        new_sensors <- .prep_get_flat_sensors(locality)
        .prep_get_flat_locality(locality, new_sensors, steps[[1]])
    }

    purrr::map(data, locality_function)
}

.prep_get_flat_sensors <- function(locality) {
    result <- new.env()
    result$sensor_names=list()
    if(length(locality$loggers) == 0) {
        return(result)
    }
    min_datetime_function <- function(.x) {
        if(length(.x$datetime) == 0) return(NA_integer_)
        as.integer(.x$datetime[[1]])}
    min_datetime <- microclim:::.common_as_utc_posixct(min(purrr::map_int(locality$loggers, min_datetime_function), na.rm=TRUE))
    max_datetime_function <- function(.x) {
        if(length(.x$datetime) == 0) return(NA_integer_)
        as.integer(tail(.x$datetime, n=1))}
    max_datetime <- microclim:::.common_as_utc_posixct(max(purrr::map_int(locality$loggers, max_datetime_function), na.rm=TRUE))
    if(is.infinite(min_datetime) || is.infinite(max_datetime)) {
        datetimes <- .POSIXct(integer(0))
    } else {
        datetimes <- seq(min_datetime, max_datetime, by=stringr::str_glue("{locality$loggers[[1]]$clean_info@step} min"))
    }
    sensor_name_function <- function(original_sensor_name, logger_index, logger_serial_number) {
        sensor_name <- .prep_get_flat_sensor_name(original_sensor_name, names(result$sensor_names),
                                                  logger_serial_number)
        result$sensor_names[[sensor_name]] <- list(logger_index=logger_index, original_name=original_sensor_name)
        sensor_name
    }
    logger_table_function <- function(logger, idx) {
        result <- microclim:::.common_sensor_values_as_tibble(logger)
        sensor_names <- purrr::map_chr(logger$sensors, function(.x) sensor_name_function(.x$metadata@name, idx, logger$metadata@serial_number))
        colnames(result) <- c("datetime", sensor_names)
        result
    }
    tables <- c(tibble::as_tibble(datetimes), purrr::imap(locality$loggers, logger_table_function))
    tables[[1]] <- tibble::as_tibble(tables[[1]])
    colnames(tables[[1]]) <- "datetime"
    result$table <- purrr::reduce(tables, function(.x, .y) dplyr::left_join(.x, .y, by="datetime"))
    result
}

.prep_get_flat_sensor_name <- function(original_sensor_name, existed_names, logger_serial_number) {
    sensor_name <- original_sensor_name
    number <- 1
    while(sensor_name %in% existed_names) {
        sensor_name <- stringr::str_glue("{original_sensor_name}_{formatC(number, width=3, flag='0')}")
        number <- number + 1
    }
    if(sensor_name != original_sensor_name) {
        warning(stringr::str_glue("sensor {original_sensor_name} from {logger_serial_number} is renamed to {sensor_name}"))
    }
    sensor_name
}

.prep_get_flat_locality <- function(locality, new_sensors, step) {
    sensor_function <- function(sensor_name) {
        sensor_names_item <- new_sensors$sensor_names[[sensor_name]]
        sensor <- locality$loggers[[sensor_names_item$logger_index]]$sensors[[sensor_names_item$original_name]]
        sensor$metadata@name <- sensor_name
        sensor$values <- new_sensors$table[[sensor_name]]
        sensor
    }

    sensors <- purrr::map(colnames(new_sensors$table)[-1], sensor_function)
    names(sensors) <- purrr::map(sensors, function(.x) .x$metadata@name)

    list(metadata = locality$metadata,
         step = step,
         datetime = new_sensors$table$datetime,
         sensors = sensors)
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
#' @examples
#'
mc_prep_rename_sensor <- function(data, sensor_names, localities=NULL, serial_numbers=NULL) {
    is_calc_format <- microclim:::.common_is_calc_format(data)

    locality_function <- function(locality) {
        if(!(is.null(localities) || locality$metadata@locality_id %in% localities)) {
            return(locality)
        }
        if(is_calc_format) {
            return(.prepare_process_sensor_renaming(locality, sensor_names))
        }
        .prepare_process_sensor_renaming_in_loggers(locality, serial_numbers, sensor_names)
    }

    purrr::map(data, locality_function)
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

