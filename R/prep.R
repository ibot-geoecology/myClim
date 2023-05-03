# constants ================================================================================

.prep_const_DETECT_STEP_LENGTH <- 100
.prep_const_FILLNA_METHOD_LINEAR <- "linear"

.prep_const_MESSAGE_PARAM_NAME_NOT_NULL <- "param_name can not be NULL"
.prep_const_MESSAGE_PARAM_NAME_NULL <- "param_name must be NULL"
.prep_const_MESSAGE_UNEXISTED_LOCALITY <- "There isn't locality {locality_id}."
.prep_const_MESSAGE_SENSOR_METADATA_WRONG_SLOT <- "Sensor metadata doesn't cointain slot {param_name}."
.prep_const_MESSAGE_UNIQUE_SENSOR_NAMES <- "Sensor names must be unique."
.prep_const_MESSAGE_UNIQUE_LOCALITY_IDS <- "Locality_ids must be unique."
.prep_const_MESSAGE_UNCLEANED_DATA<- "Data aren't cleaned."
.prep_const_MESSAGE_NA_APPROX_METHOD_NOT_IMPLEMENTED <- "Method is not implemented."
.prep_const_MESSAGE_CLEAN_AGG <- "It isn't possible to clean myClim object in Agg-format."
.prep_const_MESSAGE_RECLEAN <- "MyClim object is already cleaned. Repeated cleaning overwrite cleaning informations."

#' Cleaning datetime series
#'
#' @description
#' By default `mc_prep_clean` runs automatically when [myClim::mc_read_files()], 
#' [myClim::mc_read_data()] are called. `mc_prep_clean` check time-series in 
#' myClim object in Raw-format for missing, duplicated, and disordered records 
#' and regularize microclimatic time-series to constant time-step.
#' Duplicated records are removed and missing values are filled with NA.
#'
#' See details.
#' 
#' @details
#' Processing the data with `mc_prep_clean` is a mandatory step 
#' required for further data handling in `myClim` library.
#' 
#' This function guarantee that all time series are in chronological order 
#' and have regular time-step, without duplicated records.
#' Function `mc_prep_clean` use time-step provided by user on import `mc_read`
#' (is stored in metadata of logger [myClim::mc_LoggerMetadata]. 
#' If time step is not provided by user on import (NA),than myClim automatically 
#' identify the  time step from input time series based on last 100 records. 
#' In case of irregular time series, function returns warning and skip series.
#' 
#' In case the time step is regular, but is not nicely rounded, function round 
#' the time series to the closest nice time and shift original data. 
#' E.g., original records in 10 min regular step c(11:58, 12:08, 12:18, 12:28) 
#' are shifted to newly generated nice sequence c(12:00, 12:10, 12:20, 12:30)
#' microclimatic records are not modified but only shifted. 
#' Maximal allowed shift of time series is 30 minutes. I.e. when the time step 
#' is 2h and goes like (13:33, 15:33, 17:33) then shifted to (13:30, 15:30, 17:30). 
#' When you have 2h time step and wish to round to the whole hour 
#' (13:33 -> 14:00, 15:33 -> 16:00) than after clening use  `mc_agg(period="2 hours")` 
#' 
#' @template param_myClim_object_raw
#' @param silent if true, then cleaning log table is not printed in console (default FALSE), see [myClim::mc_info_clean()]
#' @return 
#' * cleaned myClim object in Raw-format
#' * cleaning log is by default printed in console, and can be called ex post by [myClim::mc_info_clean()]
#' @export
#' @examples
#' cleaned_data <- mc_prep_clean(mc_data_example_raw)
mc_prep_clean <- function(data, silent=FALSE) {
    if(.common_is_agg_format(data)) {
        stop(.prep_const_MESSAGE_CLEAN_AGG)
    }
    if(.prep_is_datetime_step_processed_in_object(data)) {
        warning(.prep_const_MESSAGE_RECLEAN)
    }
    locality_function <- function(locality) {
        locality$loggers <- purrr::map(locality$loggers, .prep_clean_logger)
        locality
    }
    data$localities <- purrr::map(data$localities, locality_function)
    if(silent) {
        return(data)
    }
    info_table <- mc_info_clean(data)
    count_loggers <- nrow(info_table)
    message(stringr::str_glue("{count_loggers} loggers"))
    start_date <- min(info_table$start_date)
    end_date <- max(info_table$end_date)
    message(stringr::str_glue("datetime range: {start_date} - {end_date}"))
    step_repr_function <- function(step) {
        return(stringr::str_glue("({step}s = {round(step/60, 2)}min)"))
    }
    steps <- paste(purrr::map(sort(unique(info_table$step)), step_repr_function), collapse = ", ")
    message(stringr::str_glue("detected steps: {steps}"))
    print.data.frame(info_table)
    return(data)
}

.prep_clean_logger <- function(logger) {
    if(is.na(logger$metadata@step)) {
        logger$clean_info@step <- .prep_detect_step_seconds(logger$datetime)
    } else {
        logger$clean_info@step <- logger$metadata@step
    }
    if(is.na(logger$clean_info@step)) {
        warning(stringr::str_glue("step cannot be detected for logger {logger$metadata@serial_number} - skip"))
        return(logger)
    }
    new_datetime <- .prep_get_rounded_datetime(logger)
    rounded <- !all(new_datetime == logger$datetime)
    logger$datetime <- new_datetime
    logger <- .prep_clean_write_info(logger, rounded)
    logger <- .prep_clean_edit_series(logger)
    logger <- .prep_clean_edit_source_state(logger)
    logger
}

.prep_detect_step_seconds <- function(datetime) {
    datetime <- tail(datetime, .prep_const_DETECT_STEP_LENGTH)
    datetime <- sort(as.numeric(datetime))
    diff_datetime <- Filter(function(x) x > 0, diff(datetime))
    round(unname(quantile(diff_datetime, p=0.5, type=1)))
}

.prep_get_rounded_datetime <- function(logger) {
    datetime_seconds <- as.numeric(logger$datetime)
    shift <- 0
    if(logger$clean_info@step > 30 * 60) {
        last_datetime <- dplyr::last(datetime_seconds)
        new_last_datetime <- (last_datetime + logger$clean_info@step %/% 2) %/% logger$clean_info@step * logger$clean_info@step
        diff <- new_last_datetime - last_datetime
        diff_rounding <- 30 * 60
        if(abs(diff) > diff_rounding %/% 2) {
            shift <- (diff + diff_rounding %/% 2) %/% diff_rounding * diff_rounding
        }
    }
    .common_as_utc_posixct((datetime_seconds + logger$clean_info@step %/% 2) %/%
                                    logger$clean_info@step * logger$clean_info@step) - shift
}

.prep_clean_write_info <- function(logger, rounded) {
    diff_datetime <- diff(as.numeric(logger$datetime))
    logger$clean_info@count_disordered <- length(purrr::keep(diff_datetime, function(x) x < 0))
    sorted_datetime <- sort(as.numeric(logger$datetime))
    diff_datetime <- diff(sorted_datetime)
    logger$clean_info@count_duplicities <- length(purrr::keep(diff_datetime, function(x) x == 0))
    right_count_datetime <- diff(c(sorted_datetime[[1]], tail(sorted_datetime, n=1))) %/% logger$clean_info@step + 1
    logger$clean_info@count_missing <- right_count_datetime - (length(logger$datetime) - logger$clean_info@count_duplicities)
    logger$clean_info@rounded <- rounded
    logger
}

.prep_clean_edit_series <- function(logger) {
    if(!.prep_clean_was_error_in_logger(logger)){
        return(logger)
    }
    table <- .common_sensor_values_as_tibble(logger)
    table <- dplyr::arrange(table, .data$datetime)
    unique_rows <- !duplicated(table$datetime)
    table_noduplicits <- table[unique_rows, ]
    datetime_range <- range(table_noduplicits$datetime)
    datetime_seq <- tibble::as_tibble(seq(datetime_range[[1]], datetime_range[[2]], by=stringr::str_glue("{logger$clean_info@step} sec")))
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
               logger$clean_info@count_duplicities == 0,
               logger$clean_info@count_missing == 0,
               !logger$clean_info@rounded)
    !all(is_ok)
}

.prep_clean_edit_source_state <- function(logger) {
    if(!.prep_clean_was_error_in_logger(logger)){
        return(logger)
    }

    sensor_function <- function(sensor) {
        states_index <- which(sensor$states$tag == .model_const_SENSOR_STATE_SOURCE)
        if(length(states_index) != 1) {
            return(sensor)
        }
        sensor$states[[states_index, "start"]] <- dplyr::first(logger$datetime)
        sensor$states[[states_index, "end"]] <- dplyr::last(logger$datetime)
        sensor
    }

    logger$sensors <- purrr::map(logger$sensors, sensor_function)
    logger
}

.prep_get_uncleaned_loggers <- function(data) {
    locality_function <- function(locality) {
        unprocessed <- purrr::discard(locality$loggers, .prep_is_logger_cleaned)
        purrr::map_chr(unprocessed, function(x) x$metadata@serial_number)
    }
    loggers <- purrr::map(data$localities, locality_function)
    purrr::reduce(loggers, c)
}

.prep_check_datetime_step_unprocessed <- function(data, func=warning) {
    if(!.prep_is_datetime_step_processed_in_object(data)){
        func(.prep_const_MESSAGE_UNCLEANED_DATA)
    }
}

.prep_is_datetime_step_processed_in_object <- function(data) {
    unprocessed_loggers <- .prep_get_uncleaned_loggers(data)
    return(length(unprocessed_loggers) == 0)
}

#' Set metadata of localities
#' 
#' @description
#' This function allows you to add or modify locality metadata including 
#' locality names. See [mc_LocalityMetadata].
#' You can import metadata from named list or from data frame. See details. 
#'  
#' @details
#' Locality metadata is especially useful for handling time zones, considering temporal cycling.
#' E.g. while providing coordinates, and in case you are sure, the loggers recorded in UTC, 
#' you can harmonize all data to the solar time (midday) with [myClim::mc_prep_solar_tz()] 
#' calculating offset to the UTC based on coordinates. Or you can directly provide 
#' the offset in minutes yourself for individual localities. This is useful e.g. 
#' for heterogeneous data sets containing loggers recording in local time
#' and you wish to unify them by setting individual offset e.g. back to UTC. 
#' If tz_offset is set manually, than tz_type is set to `user defined`.  
#' 
#' For minor metadata modification it is practical to use named list in combination
#' with `param_name` specification. E.g. when you wish to modify only time zone offset,  
#' then set `param_name="tz_offset"` and provide named list with locality name and 
#' offset value `list(A1E05=60)`. Similarly for other metadata slots [mc_LocalityMetadata].
#' 
#' For batch or generally more complex metadata modification you can provide data.frame
#' with columns specifying `locality_id` and one of `new_locality_id, elevation, lat_wgs84, lon_wgs84, tz_offset`.
#' Provide locality_id (name) and the value in column of metadata you wish to update. 
#' In case of using data.frame use `param_name = NULL`  
#' 
#' @template param_myClim_object
#' @param values for localities can be named list or table
#'
#' * named list: `metadata <- list(locality_id=value)`; `param_name` must be set
#' * table with column `locality_id` and another columns named by metadata parameter name;
#' to rename locality use `new_locality_id`. Parameter `param_name` must be NULL.
#' @param param_name name of locality metadata parameter; Default names are `locality_id`, `elevation`, `lat_wgs84`, `lon_wgs84`, `tz_offset`.
#' Another names are inserted to `user_data` list. see [myClim::mc_LocalityMetadata]
#' @return myClim object in the same format as input, with updated metadata
#' @export
#' @examples
#' data <- mc_prep_meta_locality(mc_data_example_raw, list(A1E05=60), param_name="tz_offset")
mc_prep_meta_locality <- function(data, values, param_name=NULL) {
    if(!is.data.frame(values)) {
        values <- tibble::tibble(locality_id=names(values),
                                 value=values)
        if(is.null(param_name)) {
            stop(.prep_const_MESSAGE_PARAM_NAME_NOT_NULL)
        }
        if(param_name == "locality_id") {
            param_name <- "new_locality_id"
        }
        names(values)[2] <- param_name
    } else if(!is.null(param_name)) {
        stop(.prep_const_MESSAGE_PARAM_NAME_NULL)
    }

    localities <- as.environment(data$localities)

    change_param_function <- function(locality_id, slot_name, value){
        if(!(locality_id %in% names(localities))) {
            warning(stringr::str_glue(.prep_const_MESSAGE_UNEXISTED_LOCALITY))
            return()
        }

        if(slot_name == "new_locality_id") {
            slot_name <- "locality_id"
        }

        if(!(slot_name %in% .model_const_EDITABLE_LOCALITY_METADATA_PARAMETERS)) {
            localities[[locality_id]]$metadata@user_data[[slot_name]] <- value
            return()
        }
        slot(localities[[locality_id]]$metadata, slot_name) <- value
        if(slot_name == "tz_offset") {
            localities[[locality_id]]$metadata@tz_type <- .model_const_TZ_USER_DEFINED
        }
    }

    slot_names <- colnames(values)[-1]

    for(slot_name in slot_names) {
        purrr::pwalk(list(locality_id=values$locality_id,
                          slot_name=slot_name,
                          value=values[[slot_name]]),
                     change_param_function)
    }

    localities <- as.list(localities)
    if("new_locality_id" %in% slot_names) {
        names(localities) <- purrr::map_chr(localities, ~ .x$metadata@locality_id)
        unique_names <- unique(names(localities))
        if(length(unique_names) != length(names(localities))) {
            stop(.prep_const_MESSAGE_UNIQUE_LOCALITY_IDS)
        }
    }

    data$localities <- localities
    return(data)
}

#' Set metadata of sensors
#'
#' @description
#' This function allows you to modify sensor metadata including sensor name. See [mc_SensorMetadata]
#'
#' @template param_myClim_object
#' @param values named list with metadata values; names of items are sensor_names e.g. 
#' for changing sensor height use `list(TMS_T1="soil 8 cm")`
#' @param param_name name of the sensor metadata parameter you want to change; 
#' You can change `name` and `height` of sensor.
#' @param localities optional filter; vector of `locality_id` 
#' where to change sensor metadata; if NULL than all localities (default NULL)
#' @param logger_types optional filter; vector of `logger_type` 
#' where to change metadata; if NULL than all logger types (default NULL);
#' `logger_type`is useful only for Raw-format of myClim having the level of logger see [myClim-package]
#' @return myClim object in the same format as input, with updated sensor metadata
#' @export
#' @examples
#' data <- mc_prep_meta_sensor(mc_data_example_raw, list(TMS_T1="my_TMS_T1"), param_name="name")
mc_prep_meta_sensor <- function(data, values, param_name, localities=NULL, logger_types=NULL) {
    is_agg_format <- .common_is_agg_format(data)

    locality_function <- function(locality) {
        if(!(is.null(localities) || locality$metadata@locality_id %in% localities)) {
            return(locality)
        }

        if(!(param_name %in% .model_const_EDITABLE_SENSOR_METADATA_PARAMETERS)) {
            stop(stringr::str_glue(.prep_const_MESSAGE_SENSOR_METADATA_WRONG_SLOT))
        }

        .prep_edit_sensors_metadata_in_locality(locality, param_name, values, logger_types, is_agg_format)
    }

    data$localities <- purrr::map(data$localities, locality_function)
    return(data)
}

.prep_edit_sensors_metadata_in_locality <- function(locality, param_name, values, logger_types, is_agg_format) {
    logger_function <- function(logger) {
        if(!(is.null(logger_types) || logger$metadata@type %in% logger_types)) {
            return(logger)
        }
        .prep_edit_sensors_metadata(logger, param_name, values)
    }

    if(is_agg_format) {
        return(.prep_edit_sensors_metadata(locality, param_name, values))
    }

    locality$loggers <- purrr::map(locality$loggers, logger_function)
    locality
}

.prep_edit_sensors_metadata <- function(item, param_name, values) {
    if(length(dplyr::intersect(names(item$sensors), names(values))) == 0) {
        return(item)
    }
    sensor_function <- function(sensor) {
        if(!sensor$metadata@name %in% names(values)) {
            return(sensor)
        }
        slot(sensor$metadata, param_name) <- values[[sensor$metadata@name]]
        sensor
    }
    item$sensors <- purrr::map(item$sensors, sensor_function)
    if(param_name == "name") {
        names(item$sensors) <- purrr::map_chr(item$sensors, function(x) x$metadata@name)
        unique_names <- unique(names(item$sensors))
        if(length(unique_names) != length(names(item$sensors))) {
            stop(.prep_const_MESSAGE_UNIQUE_SENSOR_NAMES)
        }
    }
    item
}

#' Set solar time offset against UTC time
#' 
#' @description
#' This function calculates the offset against UTC on the locality to get the solar time.
#' This is based on coordinates (longitude). If longitude is not provided, then not working.
#'  
#' @details
#' myClim library presumes the data in UTC by default. This function require at least longitude provided in locality
#' metadata slot `lon_wgs84`. If longitude is not provided, function does not work. Coordinates of locality can be provided
#' e.g. during data reading see [myClim::mc_read_data()] or ex post with [myClim::mc_prep_meta_locality()] function.
#' 
#' TZ offset in minutes is calculated as `longitude / 180 * 12 * 60`.
#'
#' @template param_myClim_object
#' @return myClim object in the same format as input, with `tz_offset` filled in locality metadata
#' @export
#' @examples
#' data_solar <- mc_prep_solar_tz(mc_data_example_clean)
mc_prep_solar_tz <- function(data) {
    locality_function <- function(locality) {
        if(is.na(locality$metadata@lon_wgs84)) {
            warning(stringr::str_glue("missing longitude in locality {locality$metadata@locality_id} - skip"))
            return(locality)
        }
        locality$metadata@tz_offset <- round(locality$metadata@lon_wgs84 / 180 * 12 * 60)
        locality$metadata@tz_type <- .model_const_TZ_SOLAR
        locality
    }

    data$localities <- purrr::map(data$localities, locality_function)
    return(data)
}

.prep_get_utc_localities <- function(data) {
    items <- purrr::keep(data$localities, function(x) x$metadata@tz_type == .model_const_TZ_UTC)
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
#' @details
#' Function is able to crop data from start to end but works also 
#' with only start and only end. When only start provided, then crop only start and
#' do not touch the end and vice versa.   
#'
#' @template param_myClim_object
#' @param start POSIXct datetime in UTC; is optional; start datetime is included
#' @param end POSIXct datetime in UTC; is optional
#' @param end_included if TRUE then end datetime is included (default TRUE)
#' @return cropped data in the same myClim format as input. 
#' @export
#' @examples
#' cropped_data <- mc_prep_crop(mc_data_example_clean, end=as.POSIXct("2020-02-01", tz="UTC"))
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

    raw_locality_function <- function(locality) {
        locality$loggers <- purrr::map(locality$loggers, sensors_item_function)
        locality
    }

    if(.common_is_agg_format(data)) {
        data$localities <- purrr::map(data$localities, sensors_item_function)
    } else {
        data$localities <- purrr::map(data$localities, raw_locality_function)
    }
    return(data)
}

.prep_crop_data <- function(item, start, end, end_included) {
    table <- .common_sensor_values_as_tibble(item)
    if(!is.null(start)) {
        table <- dplyr::filter(table, .data$datetime >= start)
    }
    last_datetime <- NULL
    if(!is.null(end)) {
        table <- dplyr::filter(table, .data$datetime < end | (end_included & .data$datetime == end))
        last_datetime <- end
        if(length(table$datetime) > 0) {
            last_datetime <- dplyr::last(table$datetime)
        }
    }
    item$datetime <- table$datetime
    item$sensors <- purrr::map(item$sensors, function(sensor) {
        sensor$values <- table[[sensor$metadata@name]]
        sensor})
    item <- .prep_crop_edit_states(item, start, last_datetime)
    item
}

.prep_crop_edit_states <- function(item, start, end) {
    if(length(item$datetime) == 0) {
        rm_states <- function(sensor) {
            sensor$states <- data.frame()
            sensor
        }

        item$sensors <- purrr::map(item$sensors, rm_states)
        return(item)
    }

    if(is.null(start)) {
        start <- min(item$datetime)
    }
    if(is.null(end)) {
        end <- max(item$datetime)
    }
    interval <- lubridate::interval(start, end)

    sensor_function <- function(sensor) {
        sensor$states <- .common_crop_states_table(sensor$states, interval)
        sensor
    }

    item$sensors <- purrr::map(item$sensors, sensor_function)
    item
}

#' Merge myClim objects
#'
#' @description
#' This function is designed to merge more existing myClim objects into one.
#' 
#' @details
#' This function works only when the input myClim objects have the same format 
#' (Raw-format, Agg-format) It is not possible to merge Raw wit Agg format. 
#' Identical time-step is required for Agg-format data.
#' 
#' When the merged myClim objects in Raw-format contains locality with same names (locality_id),
#' than list of loggers are merged on the locality. Sensors with the same name does not matter here. 
#' Loggers with the same name within the locality are allowed in the Raw-format.
#' 
#' When the merged myClim objects in Agg-format contains locality with same names (locality_id).
#' than the sensors are merged on the locality. Sensors with same names are renamed. 
#'
#' @param data_items list of myClim objects see [myClim-package]; Format (Raw/Agg) of merged objects must be same.
#' @return merged myClim object in the same format as input objects
#' @examples
#' merged_data <- mc_prep_merge(list(mc_data_example_raw, mc_data_example_raw))
#' @export
mc_prep_merge <- function(data_items) {
    purrr::reduce(data_items, .prep_do_merge)
}

.prep_do_merge <- function(data1, data2) {
    .prep_merge_check_data(data1, data2)
    is_raw_format <- .common_is_raw_format(data1)

    common_locality_ids <- intersect(names(data1$localities), names(data2$localities))

    merge_localities_function <- function (locality_id) {
        locality1 <- data1$localities[[locality_id]]
        locality2 <- data2$localities[[locality_id]]
        if(is_raw_format) {
            return(.prep_merge_prep_localities(locality1, locality2))
        }
        .prep_merge_calc_localities(locality1, locality2, data1$metadata@period)
    }

    common_localities <- purrr::map(common_locality_ids, merge_localities_function)
    localities <- c(purrr::discard(data1$localities, ~ .x$metadata@locality_id %in% common_locality_ids),
                    purrr::discard(data2$localities, ~ .x$metadata@locality_id %in% common_locality_ids),
                    common_localities)
    names(localities) <- purrr::map_chr(localities, ~ .x$metadata@locality_id)
    data1$localities <- localities
    return(data1)
}

.prep_merge_check_data <- function(data1, data2) {
    is_data1_agg_format <- .common_is_agg_format(data1)
    is_data2_agg_format <- .common_is_agg_format(data2)

    if(xor(is_data1_agg_format, is_data2_agg_format)) {
        stop("There is different format in data1 and data2.")
    }

    if(is_data1_agg_format &&
        (lubridate::as.period(data1$metadata@period) != lubridate::as.period(data2$metadata@period))) {
        stop("There is different step in data1 and data2.")
    }
}

.prep_merge_prep_localities <- function(locality1, locality2){
    locality1$loggers <- c(locality1$loggers, locality2$loggers)
    locality1
}

.prep_merge_calc_localities <- function(locality1, locality2, period){
    localities <- list(locality1, locality2)
    datetime <- .agg_get_datetimes_from_sensor_items(localities, period)
    sensors <- .agg_get_merged_sensors(datetime, localities)
    locality1$datetime <- datetime
    locality1$sensors <- sensors
    locality1
}

#' Load calibration to correct microclimatic records
#'
#' @description
#' This function loads calibration parameters from data.frame 
#' and writes them into myClim object metadata. This function
#' does not calibrate data. For calibration itself run [myClim::mc_prep_calib()]
#'
#' @details
#' This function allows user to provide calibration values either from DIY or 
#' certified calibration procedure. Calibration data have by default the form 
#' of linear function determined by the `cor_factor` and `cor_slope`:
#' 
#' `calibrated = original * (cor_slope + 1) + cor_factor` 
#' 
#' This is useful in 
#' case of multi-point calibration typically performed by certified calibration 
#' labs. In case of one-point calibration typically DIY 
#' calibrations only `cor_factor` is used and `cor_slope=0`. One point calibration
#' is thus only the addition of correction factor. This function loads sensor specific 
#' calibration values from data frame and writs them into myClim Raw-format 
#' object metadata. The structure of input data frame is as follows:
#'
#'  * serial_number = unique identification of logger hosting the sensors e.g. 91184101 
#'  * sensor_id = the name of sensor to calibrate e.g. TMS_T1
#'  * datetime = the date of the calibration
#'  * cor_factor = the correction factor, in case of multi-point calibration the intercept of calibration curve.
#'  * cor_slope = the slope of calibration curve (in case of one-point calibration slope = 0)
#'
#' It is not possible to change calibration parameters for already calibrated sensor. 
#' This prevents repeated calibrations. Once [myClim::mc_prep_calib()] is called then 
#' it is not allowed to provide new calibration data, neither run calibration again. 
#'
#' @template param_myClim_object_raw
#' @param calib_table data.frame with columns (serial_number, sensor_id, datetime, slope, intercept)
#' @return myClim object with loaded calibration information in metadata. 
#' Microclimatic records are not calibrated, only ready for calibration. 
#' To calibrate records run [myClim::mc_prep_calib()]
#' @export
mc_prep_calib_load <- function(data, calib_table) {
    .common_stop_if_not_raw_format(data)
    calib_table <- dplyr::group_nest(dplyr::group_by(calib_table, .data$serial_number))

    sensor_function <- function(sensor, logger_calib_table) {
        sensor_calib_table <- dplyr::filter(logger_calib_table, .data$sensor_id == sensor$metadata@sensor_id)
        if(nrow(sensor_calib_table) == 0) {
            return(sensor)
        }
        if(sensor$metadata@calibrated) {
            stop("It is not possible change calibration parameters in calibrated sensor.")
        }
        if(!("cor_slope" %in% colnames(sensor_calib_table))) {
            sensor_calib_table$cor_slope <- 0
        }
        sensor_calib_table <- dplyr::select(sensor_calib_table, "datetime", "cor_factor", "cor_slope")
        sensor$calibration <- as.data.frame(dplyr::arrange(sensor_calib_table, .data$datetime))
        sensor
    }

    logger_function <- function(logger) {
        filtered_table <- dplyr::filter(calib_table, .data$serial_number == logger$metadata@serial_number)
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

    data$localities <- purrr::map(data$localities, locality_function)
    return(data)
}

#' Sensors calibration
#'
#' @description
#' This function calibrate values of sensor (microclimatic records) using the 
#' myClim object `sensor$calibration` parameters provided by [myClim::mc_prep_calib_load()]. 
#' Microclimatic records are changed and myClim object parameter `sensor$metadata@calibrated` 
#' is set to TRUE. It isn't allowed to calibrate sensor multiple times.
#'
#' @details
#' This function performs calibration itself. It uses the calibration values (cor_factor, cor_slope) stored
#' in myClim object sensor metadata sensor calibration loaded with [myClim::mc_prep_calib_load()]. 
#' As it is possible to have multiple calibration values for one sensor in time (re-calibration after some time)
#' different calibration values can be applied based on the calibration time. Older microclimatic records 
#' then first calibration `datetime` available are calibrated anyway (in case sensor was calibrated ex-post)
#' with the first calibration parameters available.
#' 
#' This function is not designed for TMSmoisture calibration 
#' (conversion to volumetric water content) for this use [myClim::mc_calc_vwc()]
#' 
#' Only sensors with real value type can be calibrated. see [myClim::mc_data_sensors()]
#' 
#' @param data myClim object in Raw-format or Agg-format having calibration data in metadata slot `sensor$calibration`
#' @param localities vector of locality_ids where to perform calibration, if NULL, then calibrate sensors on all localities (default NULL)
#' @param sensors vector of sensor names where to perform calibration see `names(mc_data_sensors)`; if NULL,
#' then calibrate all sensors having calibration parameters loaded (default NULL)
#' @return same myClim object as input but with calibrated sensor values.
#' @export
mc_prep_calib <- function(data, localities=NULL, sensors=NULL) {
    is_raw_format <- .common_is_raw_format(data)
    if(is_raw_format) {
        .prep_check_datetime_step_unprocessed(data, stop)
    }

    sensor_function <- function(sensor, datetime, locality_id) {
        if(is.null(sensors) && sensor$metadata@sensor_id %in% .model_const_WRONG_CALIBRATION_SENSOR_ID) {
            return(sensor)
        }
        if(!is.null(sensors) && !(sensor$metadata@name %in% sensors)) {
            return(sensor)
        }
        if(!is.null(sensors) && nrow(sensor$calibration) == 0) {
            warning(stringr::str_glue("Calibration parameters are missing in sensor {sensor$metadata@name} in {locality_id}."))
            return(sensor)
        }
        if(.model_is_physical_TMSmoisture(sensor$metadata)) {
            warning(stringr::str_glue("Using simple linear correction of raw moisture values in sensor {sensor$metadata@name}, for more precisse correction use function mc_calc_vwc."))
        }
        if(sensor$metadata@calibrated) {
            stop(stringr::str_glue("Sensor {sensor$metadata@name} was already calibrated. It isn't possible recalibrate sensor."))
        }
        if(!.model_is_type_real(sensor$metadata)) {
            stop(stringr::str_glue("Value type of sensor {sensor$metadata@name} isn't real."))
        }

        values_table <- tibble::tibble(datetime = datetime,
                                       values = sensor$values)
        input_data <- .prep_split_data_by_calibration(values_table, sensor$calibration)
        data_function <- function(cor_factor, cor_slope, data){
            if(is.na(cor_factor) || is.na(cor_slope)) {
                return(data$values)
            }
            data$values * (cor_slope + 1) + cor_factor
        }
        values <- purrr::pmap(dplyr::select(input_data, "cor_factor", "cor_slope", "data"), data_function)
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
        if(is_raw_format) {
            locality$loggers <- purrr::map(locality$loggers, ~ logger_function(.x, locality$metadata@locality_id))
        } else {
            locality$sensors <- purrr::map(locality$sensors, ~ sensor_function(.x, locality$datetime, locality$metadata@locality_id))
        }
        locality
    }

    data$localities <- purrr::map(data$localities, locality_function)
    return(data)
}

.prep_split_data_by_calibration <- function(values_table, calib_table) {
    min_datetime <- dplyr::first(values_table$datetime)
    if(nrow(calib_table) == 0) {
        calib_table <- tibble::tibble(datetime = min_datetime,
                                      cor_factor = NA_real_,
                                      cor_slope = NA_real_)
    } else if (min_datetime < dplyr::first(calib_table$datetime)) {
        calib_table[1, "datetime"] <- min_datetime
    }
    calib_table[["end_datetime"]] <- c(as.numeric(calib_table$datetime), Inf)[-1]
    subset_function <- function(start, end) {
        dplyr::filter(values_table, .data$datetime >= start & .data$datetime < end)
    }
    calib_table$data <- purrr::map2(calib_table$datetime, calib_table$end_datetime, subset_function)
    calib_table
}

#' Fill NA
#' 
#' @description
#' This function approximate NA (missing) values. It was designed to fill 
#' only small gaps in microclimatic time-series therefore, the default maximum 
#' length of the gap is 5 missing records and longer gaps are not filled
#' Only linear method is implemented from [zoo::na.approx] function.
#'
#' @template param_myClim_object
#' @template param_localities_sensors
#' @param maxgap maximum number of consecutively NA values to fill (default 5)
#' @param method used for approximation. It is implemented now only "linear". (default "linear")
#' @return myClim object with filled NA values
#' @export
mc_prep_fillNA <- function(data, localities=NULL, sensors=NULL, maxgap=5, method="linear") {
    is_agg <- .common_is_agg_format(data)
    if(!is_agg) {
        .prep_check_datetime_step_unprocessed(data, stop)
    }

    sensor_function <- function (sensor) {
        if(!(is.null(sensors) || sensor$metadata@name %in% sensors)) {
            return(sensor)
        }
        if(method == .prep_const_FILLNA_METHOD_LINEAR){
            sensor$values <- zoo::na.approx(sensor$values, na.rm=FALSE, maxgap=maxgap)
        } else {
            stop(.prep_const_MESSAGE_NA_APPROX_METHOD_NOT_IMPLEMENTED)
        }
        return(sensor)
    }

    logger_function <- function (logger) {
        logger$sensors <- purrr::map(logger$sensors, sensor_function)
        return(logger)
    }

    locality_function <- function (locality) {
        if(!(is.null(localities) || locality$metadata@locality_id %in% localities)) {
            return(locality)
        }
        if(is_agg) {
            locality$sensors <- purrr::map(locality$sensors, sensor_function)
        } else {
            locality$loggers <- purrr::map(locality$loggers, logger_function)
        }
        return(locality)
    }

    data$localities <- purrr::map(data$localities, locality_function)
    return(data)
}
