# constants ================================================================================

.prep_const_DETECT_STEP_LENGTH <- 100
.prep_const_FILLNA_METHOD_LINEAR <- "linear"

.prep_const_MESSAGE_PARAM_NAME_NOT_NULL <- "param_name can not be NULL"
.prep_const_MESSAGE_PARAM_NAME_NULL <- "param_name must be NULL"
.prep_const_MESSAGE_UNEXISTED_LOCALITY <- "There isn't locality {locality_id}."
.prep_const_MESSAGE_SENSOR_METADATA_WRONG_SLOT <- "Sensor metadata doesn't cointain slot {param_name}."
.prep_const_MESSAGE_UNIQUE_SENSOR_NAMES <- "Sensor names must be unique."
.prep_const_MESSAGE_UNIQUE_LOCALITY_IDS <- "Locality_ids must be unique."
.prep_const_MESSAGE_UNCLEANED_DATA<- "Data is not cleaned."
.prep_const_MESSAGE_NA_APPROX_METHOD_NOT_IMPLEMENTED <- "Method is not implemented."
.prep_const_MESSAGE_CLEAN_AGG <- "It isn't possible to clean myClim object in Agg-format."
.prep_const_MESSAGE_RECLEAN <- "MyClim object is already cleaned. Repeated cleaning overwrite cleaning informations."
.prep_const_MESSAGE_ALREADY_CALIBRATED <- "It is not possible change calibration parameters in calibrated sensor."
.prep_const_MESSAGE_DATETIME_WRONG_TYPE <- "Type of datetime column must be POSIXct."
.prep_const_MESSAGE_CROP_DATETIME_LENGTH <- paste0("Start and end datetime can be NULL, ",
                                                   "single value or vector with same length as localities.")
.prep_const_MESSAGE_VALUES_SAME_TIME <- "In logger {serial_number} are different values of {sensor_name} in same time."
.prep_const_MESSAGE_STEP_PROBLEM <- "step cannot be detected for logger {logger$metadata@serial_number} - skip"
.prep_const_MESSAGE_CLEAN_CONFLICT <- "Object not cleaned. The function only tagged (states) measurements with cleaning conflicts."

#' Cleaning datetime series
#'
#' @description
#' By default, `mc_prep_clean` runs automatically when [myClim::mc_read_files()]
#' or [myClim::mc_read_data()] are called. `mc_prep_clean` checks the time-series 
#' in the myClim object in Raw-format for missing, duplicated, and disordered records. 
#' The function can either directly regularize microclimatic 
#' time-series to a constant time-step, remove duplicated records, and 
#' fill missing values with NA (`resolve_conflicts=TRUE`); or it can
#' insert new states (tags) see [mc_states_insert] to highlight records with conflicts
#' i.e. duplicated datetime but different measurement values (`resolve_conflicts=FALSE`) 
#' but not perform the cleaning itself. When  there were no conflicts, 
#' cleaning is performed in both cases (`resolve_conflicts=TRUE or FALSE`) See details.
#'
#' @details
#' The function `mc_prep_clean` can be used in two different ways depending on 
#' the parameter `resolve_conflicts`. When `resolve_conflicts=TRUE`, the function
#' performs automatic cleaning and returns a cleaned myClim object. When `resolve_conflicts=FALSE`,
#' and myClim object contains conflicts, the function returns the original, 
#' uncleaned object with tags (states) see [mc_states_insert]
#' highlighting records with duplicated datetime but different measurement values.
#' When there were no conflicts, cleaning is performed in both cases (`resolve_conflicts=TRUE OR FALSE`)
#' 
#' Processing the data with `mc_prep_clean` and resolving the conflicts is a mandatory step
#' required for further data handling in the `myClim` library.
#'
#' This function guarantee that all time series are in chronological order,
#' have regular time-step and no duplicated records.
#' Function `mc_prep_clean` use either time-step provided by user during data import with `mc_read`
#' (used time-step is permanently stored in logger metadata [myClim::mc_LoggerMetadata];
#' or if time-step is not provided by the user (NA),than myClim automatically
#' detects the time-step from input time series based on the last 100 records.
#' In case of irregular time series, function returns warning and skip the series.
#'
#' In case the time-step is regular, but is not nicely rounded, function rounds
#' the time series to the closest nice time and shifts original data.
#' E.g., original records in 10 min regular step c(11:58, 12:08, 12:18, 12:28)
#' are shifted to newly generated nice sequence c(12:00, 12:10, 12:20, 12:30).
#' Note that microclimatic records are not modified but only shifted.
#' Maximum allowed shift of time series is 30 minutes. For example, when the time-step
#' is 2h (e.g. 13:33, 15:33, 17:33), the measurement times are shifted to (13:30, 15:30, 17:30).
#' When you have 2h time step and wish to go to the whole hour  
#' (13:33 -> 14:00, 15:33 -> 16:00) the only way is aggregation - 
#' use `mc_agg(period="2 hours")` command after data cleaning.
#' 
#' In cases when the user provides a time-step during data import in `mc_read` functions 
#' instead of relying on automatic step detection, and the provided step does not correspond 
#' with the actual records (i.e., the logger records data every 900 seconds but the user 
#' provides a step of 3600 seconds), the myClim rounding routine consolidates multiple 
#' records into an identical datetime. The resulting value corresponds to the one closest 
#' to the provided step (i.e., in an original series like ...9:50, 10:05, 10:20, 10:35, 10:50, 11:05..., 
#' the new record would be 10:00, and the value will be taken from the original record at 10:05). 
#' This process generates numerous warnings in `resolve_conflicts=TRUE` and a multitude of tags 
#' in `resolve_conflicts=FALSE`.
#'  
#' @template param_myClim_object_raw
#' @param silent if true, then cleaning log table and progress bar is not printed in console (default FALSE), see [myClim::mc_info_clean()]
#' @param resolve_conflicts by default the object is automatically cleaned and conflict 
#' measurements with closest original datetime to rounded datetime are selected, see details. (default TRUE)
#' If FALSE and conflict records exist the function returns the original, uncleaned object with tags (states) "conflict"
#' highlighting records with duplicated datetime but different measurement values.When conflict records 
#' does not exist, object is cleaned in both TRUE and FALSE cases. 
#' @return
#' * cleaned myClim object in Raw-format (default) `resolve_conflicts=TRUE` or `resolve_conflicts=FALSE` but no conflicts exist 
#' * cleaning log is by default printed in console, but can be called also later by [myClim::mc_info_clean()]
#' * non cleaned myClim object in Raw-format with "conflict" tags `resolve_conflicts=FALSE` and conflicts exist
#' 
#' @export
#' @examples
#' cleaned_data <- mc_prep_clean(mc_data_example_raw)
mc_prep_clean <- function(data, silent=FALSE, resolve_conflicts=TRUE) {
    if(.common_is_agg_format(data)) {
        stop(.prep_const_MESSAGE_CLEAN_AGG)
    }
    if(.prep_is_datetime_step_processed_in_object(data)) {
        warning(.prep_const_MESSAGE_RECLEAN)
    }
    clean_env <- new.env()
    clean_env$resolve_conflicts <- resolve_conflicts
    clean_env$states <- tibble::tibble()
    clean_env$clean_bar <- NULL
    count_table <- mc_info_count(data)
    if(!silent) {
        clean_env$clean_bar <- progress::progress_bar$new(format = "clean [:bar] :current/:total loggers",
                                                          total=count_table$count[count_table$item == "loggers"])
    }
    locality_function <- function(locality) {
        locality$loggers <- purrr::imap(locality$loggers, ~ .prep_clean_logger(locality$metadat@locality_id,
                                                                               .x, .y, clean_env))
        locality
    }

    cleaned_data <- data
    cleaned_data$localities <- purrr::map(cleaned_data$localities, locality_function)
    if(nrow(clean_env$states) > 0) {
        data <- .states_run(data, clean_env$states, .states_insert, FALSE)
        warning(.prep_const_MESSAGE_CLEAN_CONFLICT)
        return(data)
    }
    if(silent) {
        return(cleaned_data)
    }
    .prep_print_clean_success_info(cleaned_data)
    return(cleaned_data)
}

.prep_clean_logger <- function(locality_id, logger, logger_index, clean_env) {
    if(is.na(logger$metadata@step)) {
        logger$clean_info@step <- .prep_detect_step_seconds(logger$datetime)
    } else {
        logger$clean_info@step <- logger$metadata@step
    }
    if(is.na(logger$clean_info@step)) {
        warning(stringr::str_glue(.prep_const_MESSAGE_STEP_PROBLEM))
        if(!is.null(clean_env$clean_bar)) clean_env$clean_bar$tick()
        return(logger)
    }
    rounded_datetime <- .prep_get_rounded_datetime(logger)
    rounded <- !all(rounded_datetime == logger$datetime)
    original_datetime <- logger$datetime
    logger$datetime <- rounded_datetime
    logger <- .prep_clean_write_info(logger, rounded)
    logger <- .prep_clean_edit_series(locality_id, logger, logger_index, original_datetime, clean_env)
    logger <- .prep_clean_round_states(logger)
    if(!is.null(clean_env$clean_bar)) clean_env$clean_bar$tick()
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
    sorted_datetime <- as.numeric(logger$datetime)
    if(logger$clean_info@count_disordered > 0)
    {
        sorted_datetime <- sort(sorted_datetime)
        diff_datetime <- diff(sorted_datetime)
    }
    logger$clean_info@count_duplicities <- length(purrr::keep(diff_datetime, function(x) x == 0))
    right_count_datetime <- diff(c(sorted_datetime[[1]], tail(sorted_datetime, n=1))) %/% logger$clean_info@step + 1
    logger$clean_info@count_missing <- right_count_datetime - (length(logger$datetime) - logger$clean_info@count_duplicities)
    logger$clean_info@rounded <- rounded
    logger
}

.prep_clean_edit_series <- function(locality_id, logger, logger_index, original_datetime, clean_env) {
    if(!.prep_clean_was_error_in_logger(logger)){
        return(logger)
    }
    table <- .common_sensor_values_as_tibble(logger)
    table$original_diff <- abs(table$datetime - original_datetime)
    table <- dplyr::relocate(table, "original_diff", .after="datetime")
    datetime_range <- range(table$datetime)
    datetime_seq <- tibble::as_tibble(seq(datetime_range[[1]], datetime_range[[2]], by=stringr::str_glue("{logger$clean_info@step} sec")))
    colnames(datetime_seq) <- "datetime"
    get_sorted_unique_values <- function(sensor_name) {
        sensor_table <- table[c("datetime", "original_diff", sensor_name)]
        sensor_table <- sensor_table[!is.na(sensor_table[sensor_name]),]
        sensor_table <- dplyr::arrange(sensor_table, .data$datetime, .data$original_diff)
        duplicated_datetime <- duplicated(sensor_table$datetime)
        sensor_table$original_diff <- NULL
        if(any(duplicated_datetime))
        {
            .prep_clean_check_different_values_in_duplicated(locality_id, logger_index, sensor_table,
                                                             logger$metadata@serial_number, clean_env)
            return(sensor_table[!duplicated_datetime, ])
        }
        return(sensor_table)
    }
    sensor_tables <- purrr::map(colnames(table)[3:ncol(table)], get_sorted_unique_values)
    output_table <- purrr::reduce(c(list(datetime_seq), sensor_tables), ~ dplyr::left_join(.x, .y, by="datetime"))
    logger$datetime <- output_table$datetime
    sensor_names <- purrr::set_names(names(logger$sensors))
    logger$sensors <- purrr::map(sensor_names, function(x) {
        logger$sensors[[x]]$values <- output_table[[x]]
        logger$sensors[[x]]
    })
    logger
}

.prep_clean_check_different_values_in_duplicated <- function(locality_id, logger_index, sensor_table, serial_number, clean_env){
    duplicated_rows <- duplicated(sensor_table$datetime) | duplicated(sensor_table$datetime, fromLast = TRUE)
    duplicated_table <- dplyr::filter(sensor_table, duplicated_rows)
    groupped_duplicated <- dplyr::group_by(duplicated_table, .data$datetime)
    is_different_function <- function(.x, .y) {
        result <- !all(diff(.x[[1]]) == 0)
        return(rep(result, nrow(.x)))
    }
    is_different <- as.logical(purrr::flatten(dplyr::group_map(groupped_duplicated, is_different_function)))
    if(any(is_different)) {
        sensor_name <- names(sensor_table)[[2]]
        warning(stringr::str_glue(.prep_const_MESSAGE_VALUES_SAME_TIME))
        if(!clean_env$resolve_conflicts) {
            .prep_add_conflict_states(locality_id, logger_index, sensor_name, sensor_table, is_different, clean_env)
        }
    }
}

.prep_add_conflict_states <- function(locality_id, logger_index, sensor_name,
                                      sensor_table, is_different, clean_env) {
    diff_parts <- rle(is_different)
    ends <- cumsum(diff_parts$lengths)
    starts <- c(1, ends[-length(ends)] + 1)
    ends <- ends[diff_parts$values]
    starts <- starts[diff_parts$values]
    states_table <- tibble::tibble(locality_id=locality_id, logger_index=logger_index,
                                   sensor_name=sensor_name, tag=.model_const_SENSOR_STATE_CONFLICT,
                                   start=sensor_table$datetime[starts], end=sensor_table$datetime[ends],
                                   value=NA_character_)
    clean_env$states <- dplyr::bind_rows(clean_env$states, states_table)
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

.prep_clean_round_states <- function(logger) {
    step <- logger$clean_info@step
    start_datetime <- dplyr::first(logger$datetime)

    sensor_function <- function(sensor) {
        return(.states_floor_sensor(sensor, start_datetime, step))
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

.prep_print_clean_success_info <- function(data) {
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
}

#' Set metadata of localities
#'
#' @description
#' This function allows you to add or modify locality metadata including
#' locality names. See [mc_LocalityMetadata].
#' You can import metadata from named list or from data frame. See details.
#'
#' @details
#' Locality metadata is critical e.g. for correctly handling time zones.
#' By providing geographic coordinates in locality metadata, the user can later harmonize all data to the local solar time (midday) #' with [myClim::mc_prep_solar_tz()] or calculate temporal offset to the UTC base on local time-zone. 
#' Alternatively, the user can directly provide the offset (in minutes) for individual localities. This can be useful especially
#' for heterogeneous data sets containing various localities with loggers recording in local time. By providing temporal offset for #' each locality separately, you can unify the whole dataset to UTC.
#' Note that when tz_offset is set manually, than tz_type is set to `user defined`.
#'
#' For minor metadata modification it is practical to use named list in combination
#' with `param_name` specification. E.g. when you wish to modify only time zone offset,
#' then set `param_name="tz_offset"` and provide named list with locality name and
#' offset value `list(A1E05=60)`. 
#' Similarly, you can modify other metadata slots [mc_LocalityMetadata].
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
#' This function calculates the temporal offset between local solar time and UTC time zone.
#' Calculation is based on geographic coordinates of each locality. 
#' Therefore, the function does not work when longitude coordinate is not provided.
#'
#' @details
#' myClim assumes that the data are in UTC. To calculate temporal offset based on local solar time, this function requires 
#' geographic coordinates (at least longitude) to be provided in locality metadata slot `lon_wgs84` (in decimal degrees). 
#' Geographic coordinates for each locality can be provided already during data reading, see [myClim::mc_read_data()], or added 
#' later  with [myClim::mc_prep_meta_locality()] function.
#'
#' TZ offset (in minutes) is calculated as `longitude / 180 * 12 * 60`.
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
#' Function is able to crop data from `start` to `end` but works also
#' with `start` only and `end` only. When only `start` is provided, then function crops only
#' the beginning of the tim-series and vice versa with end.
#'
#' If `start` or `end` is a single POSIXct value, it is used for all or selected localities (regular crop).
#' However, if `start` and `end` are vectors of POSIXct values with the same length as the localities vector,
#' each locality is cropped by its own time window (irregular crop).
#'
#' The `end_included` parameter is used for selecting, whether to return data which contains `end`
#' time or not. For example when cropping the data to rounded days, typically users use midnight.
#' 2023-06-15 00:00:00 UTC. But midnight is the last date of ending day and the same
#' time first date of the next day. Thus, there will be the last day with single record.
#' This can be confusing in aggregation (e.g. daily mean of single record per day, typically NA) so
#' sometimes it is better to exclude end and crop on 2023-06-14 23:45:00 UTC (15 minutes records).
#'
#' @template param_myClim_object
#' @param start optional; POSIXct datetime **in UTC**; single value or vector; start datetime is included (default NULL)
#' @param end optional, POSIXct datetime **in UTC**; single value or vector (default NULL)
#' @param localities vector of locality_ids to be cropped; if NULL then all localities are cropped (default NULL)
#' @param end_included if TRUE then end datetime is included (default TRUE), see details
#' @return cropped data in the same myClim format as input.
#' @export
#' @examples
#' cropped_data <- mc_prep_crop(mc_data_example_clean, end=as.POSIXct("2020-02-01", tz="UTC"))
mc_prep_crop <- function(data, start=NULL, end=NULL, localities=NULL, end_included=TRUE) {
    if(!is.null(start) && any(format(start, format="%Z") != "UTC")) {
        warning(stringr::str_glue("start datetime is not in UTC"))
    }
    if(!is.null(end) && any(format(end, format="%Z") != "UTC")) {
        warning(stringr::str_glue("end datetime is not in UTC"))
    }
    if(!.prep_crop_is_datetime_correct(start, localities) ||
        !.prep_crop_is_datetime_correct(end, localities)) {
        stop(.prep_const_MESSAGE_CROP_DATETIME_LENGTH)
    }
    all_table <- tibble::tibble(locality_id=names(data$localities))
    if(!is.null(localities)) {
        table <- tibble::tibble(locality_id=localities)
        table$start_datetime <- if(is.null(start)) lubridate::NA_POSIXct_ else start
        table$end_datetime <- if(is.null(end)) lubridate::NA_POSIXct_ else end
        all_table <- dplyr::left_join(all_table, table, by="locality_id")
    }
    else {
        all_table$start_datetime <- if(is.null(start)) lubridate::NA_POSIXct_ else start
        all_table$end_datetime <- if(is.null(end)) lubridate::NA_POSIXct_ else end
    }

    sensors_item_function <- function(item, start_datetime, end_datetime) {
        .prep_crop_data(item, start_datetime, end_datetime, end_included)
    }

    raw_locality_function <- function(locality_id, start_datetime, end_datetime) {
        locality <- data$localities[[locality_id]]
        if(!is.na(start_datetime) || !is.na(end_datetime)) {
            locality$loggers <- purrr::pmap(list(item=locality$loggers,
                                                 start_datetime=start_datetime,
                                                 end_datetime=end_datetime),
                                            sensors_item_function)
        }
        return(locality)
    }

    agg_locality_function <- function(locality_id, start_datetime, end_datetime) {
        locality <- data$localities[[locality_id]]
        if(!is.na(start_datetime) || !is.na(end_datetime)) {
            locality <- sensors_item_function(locality, start_datetime, end_datetime)
        }
        return(locality)
    }

    if(.common_is_agg_format(data)) {
        data$localities <- purrr::pmap(all_table, agg_locality_function)
    } else {
        data$localities <- purrr::pmap(all_table, raw_locality_function)
    }
    names(data$localities) <- all_table$locality_id
    return(data)
}

.prep_crop_is_datetime_correct <- function(datetime, localities) {
    return(is.null(datetime) || length(datetime) == 1 ||
        (!is.null(localities) && length(datetime) == length(localities)))
}

.prep_crop_data <- function(item, start, end, end_included) {
    table <- .common_sensor_values_as_tibble(item)
    if(!is.na(start)) {
        table <- dplyr::filter(table, .data$datetime >= start)
    }
    last_datetime <- lubridate::NA_POSIXct_
    if(!is.na(end)) {
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

    if(is.na(start)) {
        start <- min(item$datetime)
    }
    if(is.na(end)) {
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

#' Load sensor calibration parameters to correct microclimatic records
#'
#' @description
#' This function loads calibration parameters from data.frame *logger_calib_table*
#' and stores them in the myClim object metadata. This function
#' does not calibrate data. For calibration itself run [myClim::mc_prep_calib()]
#'
#' @details
#' This function allows user to provide correction coefficients `cor_factor` and `cor_slope` for linear sensor calibration.
#' Calibrated data have by default the form of linear function terms:
#'
#' `calibrated value = original value * (cor_slope + 1) + cor_factor`
#'
#' In case of one-point calibration, `cor_factor` can be estimated as:
#' `cor_factor = reference value - sensor value`
#' and `cor_slope` should be set to 0.
#' This function loads sensor-specific
#' calibration coefficients from *calib_table* and stores them into myClim Raw-format
#' object metadata. The *calib_table* is data.frame with 5 columns:
#'
#'  * serial_number = serial number of the logger
#'  * sensor_id = name of sensor, e.g. "TMS_T1"
#'  * datetime = the date of the calibration in POSIXct type
#'  * cor_factor = the correction factor
#'  * cor_slope = the slope of calibration curve (in case of one-point calibration, use cor_slope = 0)
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
    if(!lubridate::is.POSIXct(calib_table$datetime)) {
        stop(.prep_const_MESSAGE_DATETIME_WRONG_TYPE)
    }
    calib_table <- dplyr::group_nest(dplyr::group_by(calib_table, .data$serial_number))

    sensor_function <- function(sensor, logger_calib_table) {
        sensor_calib_table <- dplyr::filter(logger_calib_table, .data$sensor_id == sensor$metadata@sensor_id)
        if(nrow(sensor_calib_table) == 0) {
            return(sensor)
        }
        if(sensor$metadata@calibrated) {
            stop(.prep_const_MESSAGE_ALREADY_CALIBRATED)
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
#' This function is not designed for moisture_raw calibration
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
        if(.model_is_physical_moisture_raw(sensor$metadata)) {
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
#' @template param_myClim_object_cleaned
#' @template param_localities
#' @template param_sensors
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

#' Detection of out-of-soil measurements from TMS logger
#'
#' @description
#' This function creates new virtual sensor labelling anomalies in TMS logger caused by displacement out of from soil.
#'
#' @details
#' TMS loggers, when correctly installed in the soil, exhibit certain temperature and soil moisture signal characteristics.
#' Temperature varies the most at the soil interface, and temperature fluctuations in the soil are minimized.
#' The moisture signal from a sensor that has lost direct contact with the soil is reduced.
#' The following criteria are used for detecting faulty measurements: the ratio of the standard deviations of the soil
#' sensor to the above-ground sensor within 24h moving window is greater than the defined threshold (default 0.76085),
#' and simultaneously, the soil moisture minimum within 24h mowing window is less than 721.5.
#' Optionally, the prediction results can be smoothed using a floating window to average-out unlikely short periods detected by the algorithm.
#' Selection and parametrization of criteria was done using a recursive partitioning (rpart::rpart)
#' on the training set of 7.8M readings in 154 TMS timeseries from different environmental settings (temperate forests, tropical rainforest, cold desert, alpine and subnival zone,
#' and invalid measurements from loggers stored in the office or displaced from the soil).
#' Sensitivity of the method (true positive rate) on was 95.1% and specificity (true negative rate) was 99.4% using function default parameters.
#' Smoothing with 10 day floating window increased sensitivity to 96.8% while retaining specifity at the same level of 99.4%.
#' Decreasing 'smooth_threshold' below 0.5 will extend periods flagged as faulty measurement.
#'
#' @template param_myClim_object_cleaned
#' @template param_localities
#' @param soil_sensor character, soil temperature sensor (default `mc_const_SENSOR_TMS_T1`)
#' @param air_sensor character, air temperature sensor (default `mc_const_SENSOR_TMS_T2`)
#' @param moist_sensor character, soil moisture sensor (default `mc_const_SENSOR_TMS_moist`)
#' @param output_sensor character, name of virtual sensor to store ouptup values (default "off_soil")
#' @param smooth logical, smooth out isolated faulty/correct records using floating window (default FALSE)
#' @param smooth_window integer, smooth floating window width (in days) (default 10)
#' @param smooth_threshold numeric, floating window threshold for detection of faulty records. (default 0.5)
#' @param sd_threshold numeric, threshold value for the criteria on the ratio of standard deviation of the soil sensor
#' to the above-ground sensor temperatures (default 0.76085)
#' @param minmoist_threshold numeric, threshold value for criteria on the minimum soil moisture (default 721.5)
#' @return numeric vector (0 = correct measurement, 1 = faulty measurement) stored as virtual sensor in myClim object
#' @export
#' @examples
#' data <- mc_read_files(system.file("extdata", "data_93142760_201904.csv", package = "myClim"),
#'                       "TOMST")
#' data <- mc_prep_TMSoffsoil(data)
#' mc_plot_line(data, sensors = c("off_soil","TMS_T1", "TMS_T2","TMS_T3"))
mc_prep_TMSoffsoil <- function(data,
                               localities=NULL,
                               soil_sensor = mc_const_SENSOR_TMS_T1,
                               air_sensor = mc_const_SENSOR_TMS_T2,
                               moist_sensor = mc_const_SENSOR_TMS_moist,
                               output_sensor = "off_soil",
                               smooth = FALSE,
                               smooth_window = 10,
                               smooth_threshold = 0.5,
                               sd_threshold = 0.76085,
                               minmoist_threshold = 721.5){
    is_agg <- .common_is_agg_format(data)
    if(!is_agg) {
        .prep_check_datetime_step_unprocessed(data, stop)
    } else {
        .calc_check_maximal_day_step(data)
    }

    logger_function <- function (logger) {
        if(.calc_check_maximal_day_step_in_logger_get_skip(logger))
        {
            return(logger)
        }
        .prep_item_add_tmsoffsoil_sensor(logger, logger$clean_info@step, soil_sensor, air_sensor, moist_sensor,
                                         output_sensor, smooth, smooth_window, smooth_threshold, sd_threshold,
                                         minmoist_threshold)
    }

    locality_function <- function (locality) {
        if(!(is.null(localities) || locality$metadata@locality_id %in% localities)) {
            return(locality)
        }
        if(is_agg) {
            return(.prep_item_add_tmsoffsoil_sensor(locality, data$metadata@step, soil_sensor, air_sensor, moist_sensor,
                                                    output_sensor, smooth, smooth_window, smooth_threshold,
                                                    sd_threshold, minmoist_threshold))
        }

        locality$loggers <- purrr::map(locality$loggers, logger_function)
        return(locality)
    }

    data$localities <- purrr::map(data$localities, locality_function)
    return(data)
}

.prep_item_add_tmsoffsoil_sensor <- function(item, step, soil_sensor, air_sensor, moist_sensor, output_sensor,
                                             smooth, smooth_window, smooth_threshold, sd_threshold, minmoist_threshold){
    skip <- .prep_TMSofsoil_check_sensors_get_skip(item, soil_sensor, air_sensor, moist_sensor, output_sensor)
    if(skip) {
        return(item)
    }
    count_values_per_day <- 3600 * 24 / step
    t1_sd <- .prep_apply_function_to_window(item$sensors[[soil_sensor]]$values, count_values_per_day + 1, sd, fillNA = TRUE)
    t2_sd <- .prep_apply_function_to_window(item$sensors[[air_sensor]]$values, count_values_per_day + 1, sd, fillNA = TRUE)
    sdt12 <- t1_sd / t2_sd
    moist <- item$sensors[[moist_sensor]]$values
    minmoist  <- .prep_apply_function_to_window(moist, count_values_per_day + 1, min, na.rm = TRUE)
    result_values <- ifelse(sdt12 < sd_threshold, 0, ifelse(minmoist >= minmoist_threshold, 0, 1))
    if(smooth) {
        result_values <- .prep_smoothing_rolling_mean(result_values, smooth_window * count_values_per_day + 1,
                                                      threshold = smooth_threshold, na.rm = TRUE)
    }
    item$sensors[[output_sensor]] <- .common_get_new_sensor(mc_const_SENSOR_logical, output_sensor,
                                                            values=result_values)
    return(item)
}

# Fast rolling window
#
# @details Aplly custom function to rolling window.
#
# @param values numeric vector
# @param window_width integer, rolling window width
# @param FUN custom function to be applied in rolling windows
# @param na.rm na.rm argument passed to custom function
# @param fillNA logical, fill_na = TRUE fills vector edges with NA, fill_na fills vector edges with first/last value
.prep_apply_function_to_window <- function(values, window_width, FUN, na.rm = TRUE, fillNA = FALSE){
    begin <- rep(ifelse(fillNA, NA, dplyr::first(values)), window_width)
    end <- rep(ifelse(fillNA, NA, dplyr::last(values)), window_width )
    frollapply_values <- c(begin, values,  end)
    result <- data.table::frollapply(frollapply_values, n=window_width, FUN=FUN, na.rm=na.rm, align="center", fill=NA)
    return(as.numeric(result[(window_width + 1):(length(values) + window_width)]))
}

# Smoothing 0/1 timeseries using fast rolling mean
#
# @param values numeric vector
# @param window_width integer, rolling window width
# @param threshold integer, threshold for classification of smoothed values back to 0/1
# @param na.rm logical
.prep_smoothing_rolling_mean <- function(values, window_width, threshold=0.5, na.rm=TRUE){
    frollmean_values <- c(rep(dplyr::first(values), window_width), values, rep(dplyr::last(values), window_width))
    result <- data.table::frollmean(frollmean_values, n =  window_width, align = "center", fill = NA, na.rm = na.rm)
    return(as.numeric(result[(window_width+1):(length(values)+window_width)] >= threshold))
}

.prep_TMSofsoil_check_sensors_get_skip <- function(item, soil_sensor, air_sensor, moist_sensor, output_sensor){
    if(!.calc_check_sensor_in_item(item, soil_sensor)){
        return(TRUE)
    }
    if(!.model_is_physical_T_C(item$sensors[[soil_sensor]]$metadata)){
        .calc_wrong_physical_warning_function(soil_sensor, .model_const_PHYSICAL_T_C)
    }
    if(!.calc_check_sensor_in_item(item, air_sensor)){
        return(TRUE)
    }
    if(!.model_is_physical_T_C(item$sensors[[air_sensor]]$metadata)){
        .calc_wrong_physical_warning_function(air_sensor, .model_const_PHYSICAL_T_C)
    }
    if(!.calc_check_sensor_in_item(item, moist_sensor)){
        return(TRUE)
    }
    if(!.model_is_physical(item$sensors[[moist_sensor]]$metadata, .model_const_PHYSICAL_moisture_raw)){
        .calc_wrong_physical_warning_function(moist_sensor, .model_const_PHYSICAL_moisture_raw)
    }
    .calc_warn_if_overwriting(item, output_sensor)
    return(FALSE)
}
