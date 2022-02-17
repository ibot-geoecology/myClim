# constants ================================================================================

.prep_const_DETECT_STEP_LENGTH <- 100

#' Cleaning datetime series
#'
#' @description
#' This function guess time step from regular time series. After that produce perfectly regular time series based on guessed step together with the first and last record. 
#' Using clean time series, function check weather the original time series is continual without missing values, check duplicated and disordered records. 
#' Resulting myClim object time series has constant step without duplicated and disordered records. Resulting time series is nicely rounded. See details. Disordered records are reordered chronologically, missing values are filled with NAs. 
#' 
#' 
#' @details
#' `mc_prep_clean` is initial, mandatory step before further work with data in `myClim` library. 
#' 
#' This function guarantee time series with constant time step, without duplicated and disordered records which is crucial for next steps in data analysis. 
#' `mc_prep_clean` assume constant time step in microclimatic records. The step is guessed from input time series based on last 100 records. In case of microclimatic logger with irregular time series, function returns warning and skip logger. 
#' 
#' In case the time step is regular, but is not nicely rounded, function round the time series to the closest nice time and shift original data to nicely rounded time series. (e.g. original records in 10 min regular step c(11:58, 12:08, 12:18, 12:28) are shifted to newly generated nice sequence c(12:00, 12:10, 12:20, 12:30) microclimatic records are not modified but only shifted).   
#' 
#' @param data myClim object in raw format (output of `mc_read` functions family) see e.g. [myClim::mc_read_directory()]
#' @param silent if true, then cleaning log table is not printed in console (default FALSE), see [myClim::mc_info_clean()]
#' @return 
#' * myClim object in clean format
#' * cleaning log is by default printed in console, and can be called ex post by [myClim::mc_info_clean()]
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

#' Set user defined offset against original time 
#' 
#' @description
#' This function allow user to set the offset in minutes against original time series of microclimatic records. `MyClim` generally assume the records are in UTM Time Zone. When offset is provided, myClim functions by default use the time corrected with this offset in calculations. See details.
#' 
#' @details 
#' For analysis of microclimatic data it is important to think of time zones because of diurnal or seasonal microclimatic rhythms. `mc_prep_user_tz` allow user to set time offset for individual localities to harmonize e.g.day/night cycles across vhole data set.
#' 
#' This function can be used also inversely, for heterogeneous data sets containing loggers recording in local time and user wish to unify them by setting individual offest e.g. to UTM. This function is also useful for corrections of shifted loggers time series e.g. due to technical issue. 
#' 
#' In case user is sure, the loggers recorded in UTC and wants to harmonize data set to solar time, there is [myClim::mc_prep_solar_tz()] calculating offset based on coordinates to harmonize the midday across vhole dataset.
#' 
#' @param data myClim object in raw, clean, or calculation format
#' @param tz_offsets named list (name: `locality_id`, list item: `tz_offset` in rounded minutes)
#' @return MyClim object in the same format as input, with `tz_offset` filled in locality metadata 
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

#' Set solar time offset against original time 
#' 
#' @description
#' This function calculates the offset against UTC on the locality  to get the solar time. This is based on coordinates. If coordinates not provided, then not working.
#' 
#' @details
#' The function require at least longitude provided in locality metadata slot `lon_wgs84`. If longitude not provided, function not works. Coordinates of locality can be provided e. g. during data reading see [myClim::mc_read_data_frame()], [myClim::mc_read_csv()]
#' 
#' TZ offset in minutes is calculated as `longitude / 180 * 12 * 60`.
#'
#' @param data myClim object in raw, clean, or calculation format
#' @return MyClim object in the same format as input, with `tz_offset` filled in locality metadata 
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
#' This function rename sensors.
#'
#' @param data in format for preparing or calculation
#' @param sensor_names list with new names of sensors; names of items are old ones
#' @param localities vector of locality_ids; if NULL than all (default NULL)
#' @param serial_numbers vector of serial_numbers; if NULL than all (default NULL); parameter is usefull only for
#' preparing format of data
#' @return data with changed sensor names
#' @example
#' data <- mc_prep_rename_sensor(example_tomst_data1, list(TMS_T1="TMS_Tsoil"))
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

    locality_items <- purrr::map(myClim:::.common_get_localities(data), locality_function)
    myClim:::.common_set_localities(data, locality_items)
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
        unique_names <- unique(names(item$sensors))
        if(length(unique_names) != length(names(item$sensors))) {
            stop("Sensor names must be unique.")
        }
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
#' @param data_items list of data in format for preparing or calculation; Format of data must be same.
#' @return merged data
#' @examples
#' merged_tomst_data <- mc_prep_merge(list(example_tomst_data1, example_tomst_data2))
#' @export
mc_prep_merge <- function(data_items) {
    purrr::reduce(data_items, .prep_do_merge)
}

.prep_do_merge <- function(data1, data2) {
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
    myClim:::.common_set_localities(data1, localities)
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

#' rename locality_id
#'
#' @description
#' This function change locality_ids.
#'
#' @param data in format for preparing or calculation
#' @param locality_ids list with new locality_ids; names of items are old ones
#' @return data with changed locality_ids
#' @examples
#' data <- mc_prep_rename_locality(example_tomst_data1, list(A1E05="ABC05", A2E32="CDE32"))
#' @export
mc_prep_rename_locality <- function(data, locality_ids) {
    locality_function <- function(locality) {
        if(!(locality$metadata@locality_id %in% names(locality_ids))) {
            return(locality)
        }
        locality$metadata@locality_id <- locality_ids[[locality$metadata@locality_id]]
        locality
    }

    localities <- purrr::map(myClim:::.common_get_localities(data), locality_function)
    names(localities) <- purrr::map_chr(localities, ~ .x$metadata@locality_id)
    unique_names <- unique(names(localities))
    if(length(unique_names) != length(names(localities))) {
        stop("Locality_ids must be unique.")
    }
    myClim:::.common_set_localities(data, localities)
}

#' load calibration parameters
#'
#' @description
#' This function load calibration parameters from data.frame
#'
#' @details
#' It is not possible change calibration parameters in calibrated sensor.
#'
#' @param data in format for preparing
#' @param calib_table data.frame with columns (serial_number, sensor_id, datetime, slope, intercept)
#' @return data with loaded calibration informations.
#' @examples
#' @export
mc_prep_calib_load <- function(data, calib_table) {
    myClim:::.common_stop_if_not_prep_format(data)
    calib_table <- dplyr::group_nest(dplyr::group_by(calib_table, serial_number))

    sensor_function <- function(sensor, logger_calib_table) {
        sensor_calib_table <- dplyr::filter(logger_calib_table, sensor_id == sensor$metadata@sensor_id)
        if(nrow(sensor_calib_table) == 0) {
            return(sensor)
        }
        if(sensor$metadata@calibrated) {
            stop("It is not possible change calibration parameters in calibrated sensor.")
        }
        sensor_calib_table <- dplyr::select(sensor_calib_table, datetime, slope, intercept)
        sensor$calibration <- as.data.frame(dplyr::arrange(sensor_calib_table, datetime))
        sensor
    }

    logger_function <- function(logger) {
        filtered_table <- dplyr::filter(calib_table, serial_number == logger$metadata@serial_number)
        if(nrow(filtered_table) == 0) {
            return(logger)
        }
        logger_calib_table <- filtered_table$data[[1]]
        logger$sensors <- purrr::map(logger$sensors, ~ sensor_function(.x, logger_calib_table))
        logger
    }

    locality_function <- function(locality) {
        locality$loggers <- purrr::map(locality$loggers, logger_function)
        locality
    }

    purrr::map(data, locality_function)
}

#' Sensor calibration
#'
#' @description
#' This function calibrate values of sensor by sensor$calibration parameters. Values are changed
#' and parameter sensor$metadata@calibrated is set to TRUE. It isn't possible calibrate calibrated sensor again.
#'
#' @details
#'
#' @param data in format for preparing or calculation
#' @param sensors vector of sensor names for calibration
#'
#' It is not possible calibrate TMSmoisture sensor with this function.
#' Calibration of TMSmoisture sensor is processed in mc_calc_vwc during conversion to volumetric water content.
#' Only sensors with real value type can be calibrated.
#' @param localities vector of locality_ids, if NULL, then calibrate in all localities (default NULL)
#' @return data with calibrated sensors.
#' @examples
#' @export
mc_prep_calib <- function(data, sensors, localities=NULL) {
    is_prep_format <- myClim:::.common_is_prep_format(data)

    sensor_function <- function(sensor, datetime, locality_id) {
        if(!(sensor$metadata@name %in% sensors)) {
            return(sensor)
        }
        if(nrow(sensor$calibration) == 0) {
            warning(stringr::str_glue("Calibration parameters missed in sensor {sensor$metadata@name} in {locality_id}."))
            return(sensor)
        }
        if(myClim:::.model_is_physical_TMSmoisture(sensor$metadata)) {
            stop(stringr::str_glue("Sensor {sensor$metadata@name} has physical TMSmoisture it is prohibited."))
        }
        if(sensor$metadata@calibrated) {
            stop(stringr::str_glue("Sensor {sensor$metadata@name} is calibrated. It isn't possible recalibrate sensor."))
        }
        if(!myClim:::.model_is_type_real(sensor$metadata)) {
            stop(stringr::str_glue("Value type of sensor {sensor$metadata@name} isn't real."))
        }

        values_table <- tibble::tibble(datetime = datetime,
                                       values = sensor$values)
        input_data <- .prep_split_data_by_calibration(values_table, sensor$calibration)
        data_function <- function(intercept, slope, data){
            if(is.na(intercept) || is.na(slope)) {
                return(data$values)
            }
            data$values * slope + intercept
        }
        values <- purrr::pmap(dplyr::select(input_data, intercept, slope, data), data_function)
        sensor$values <- purrr::flatten_dbl(values)
        sensor$metadata@calibrated <- TRUE
        sensor
    }

    logger_function <- function(logger, locality_id) {
        logger$sensors <- purrr::map(logger$sensors, ~ sensor_function(.x, logger$datetime, locality_id))
        logger
    }

    locality_function <- function(locality) {
        if(!(is.null(localities) || locality$metadata@locality_id %in% localities)) {
           return(locality)
        }
        if(is_prep_format) {
            locality$loggers <- purrr::map(locality$loggers, ~ logger_function(.x, locality$metadata@locality_id))
        } else {
            locality$sensors <- purrr::map(locality$sensors, ~ sensor_function(.x, locality$datetime, locality$metadata@locality_id))
        }
        locality
    }

    data_localities <- purrr::map(myClim:::.common_get_localities(data), locality_function)
    myClim:::.common_set_localities(data, data_localities)
}

.prep_split_data_by_calibration <- function(values_table, calib_table) {
    if(nrow(calib_table) == 0) {
        calib_table <- tibble::tibble(datetime = dplyr::first(values_table$datetime),
                                      slope = NA_real_,
                                      intercept = NA_real_)
    } else if (dplyr::first(values_table$datetime) < dplyr::first(calib_table$datetime)) {
        calib_table <- tibble::add_row(calib_table,
                                       datetime = dplyr::first(values_table$datetime),
                                       slope = NA_real_,
                                       intercept = NA_real_,
                                       .before = 1)
    }
    calib_table[["end_datetime"]] <- c(as.numeric(calib_table$datetime), Inf)[-1]
    subset_function <- function(start, end) {
        dplyr::filter(values_table, datetime >= start & datetime < end)
    }
    calib_table$data <- purrr::map2(calib_table$datetime, calib_table$end_datetime, subset_function)
    calib_table
}

