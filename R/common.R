.common_const_MESSAGE_UNKNOWN_SENSOR_ID <- "Sensor_id {sensor_id} is unknown."
.common_const_MESSAGE_LONG_PERIOD_LOCAL_TIME <- "It isn't possible change time zone for period longer than day. Apply tz_offset in mc_agg function (parameter use_utc)."
.common_const_MESSAGE_NO_RAW <- "Format of input data isn't correct. Use non-aggregated data."

.common_convert_factors_in_dataframe <- function(dataframe) {
    factor_columns <- sapply(dataframe, is.factor)
    if(!any(factor_columns)) {
        return(dataframe)
    }
    dataframe[factor_columns] <- lapply(dataframe[factor_columns], as.character)
    return(dataframe)
}

.common_get_sensor_info <- function(sensor_metadata) {
    myClim::mc_data_sensors[[sensor_metadata@sensor_id]]
}

.common_as_utc_posixct <- function(datetime) {
    as.POSIXct(datetime, origin="1970-01-01", tz="UTC")
}

.common_get_loggers <- function(data) {
    unname(do.call(c, lapply(data$localities, function(x) x$loggers)))
}

.common_sensor_values_as_tibble <- function(item) {
    data <- c(list(datetime=item$datetime), purrr::map(item$sensors, function(.x) .x$values))
    tibble::as_tibble(data)
}

.common_is_agg_format <- function(data) {
    data$metadata@format_type == .model_const_FORMAT_AGG
}

.common_is_raw_format <- function(data) {
    data$metadata@format_type == .model_const_FORMAT_RAW
}

.common_stop_if_not_raw_format <- function(data) {
    if(!.common_is_raw_format(data)) {
        stop(.common_const_MESSAGE_NO_RAW)
    }
}

.common_get_new_sensor <- function(sensor_id, sensor_name, values=NULL, height=NA_character_,
                                   calibrated=FALSE, calibration=data.frame(), states=data.frame()){
    if(!(sensor_id %in% names(myClim::mc_data_sensors))) {
        warning(stringr::str_glue(.common_const_MESSAGE_UNKNOWN_SENSOR_ID))
    }
    metadata <- new("mc_SensorMetadata")
    metadata@sensor_id <- sensor_id
    metadata@name <- sensor_name
    metadata@height <- height
    metadata@calibrated <- calibrated
    item <- list(metadata = metadata,
                 values = values,
                 calibration = calibration,
                 states = states)
    item
}

.common_get_id_of_item_with_sensors <-function(item) {
    if("locality_id" %in% slotNames(item$metadata)) {
        return(item$metadata@locality_id)
    }
    item$metadata@serial_number
}

.common_get_cleaned_data_range <- function(data, add_step_to_end=FALSE) {
    is_agg <- .common_is_agg_format(data)

    if(is_agg) {
        items <- data$localities
        steps <- NA_real_
    } else {
        items <- purrr::flatten(purrr::map(data$localities, ~ .x$loggers))
        steps <- purrr::map_dbl(items, ~ .x$clean_info@step)
    }

    datetime_items_function <- function(item, step) {
        start <- dplyr::first(item$datetime)
        end <- dplyr::last(item$datetime)
        if(!add_step_to_end) {
            return(list(start=start, end=end))
        }
        if(!is_agg) {
            end <- end + lubridate::seconds(step)
        } else if(!is.na(data$metadata@step)) {
            end <- end + lubridate::seconds(data$metadata@step)
        } else {
            end <- end + lubridate::period(data$metadata@period)
        }
        return(list(start=start, end=end))
    }
    table <- purrr::map2_dfr(items, steps, datetime_items_function)
    lubridate::interval(min(table$start, na.rm=TRUE), max(table$end, na.rm=TRUE))
}

.common_get_logger_shift <- function(logger) {
    as.integer(logger$datetime[[1]]) %% as.integer(logger$clean_info@step)
}

.common_crop_states_table <- function(states_table, intervals) {
    states_table$interval <- lubridate::interval(states_table$start, states_table$end)
    interval_function <- function(interval) {
        result <- states_table
        result$new_interval <- lubridate::intersect(interval, result$interval)
        result <- dplyr::filter(result, !is.na(.data$new_interval))
        return(list(tag=result$tag,
                    start=lubridate::int_start(result$new_interval),
                    end=lubridate::int_end(result$new_interval),
                    value=result$value))
    }

    return(as.data.frame(purrr::map_dfr(intervals, interval_function)))
}

.common_get_time_series_intervals <- function(datetime, filter) {
    rle_output <- rle(filter)
    cumsum_lengths <- cumsum(rle_output$lengths)
    start_indexes <- (c(0, cumsum_lengths[-length(cumsum_lengths)]) + 1)[rle_output$values]
    end_indexes <- cumsum_lengths[rle_output$values]
    lubridate::interval(datetime[start_indexes], datetime[end_indexes])
}

.common_get_count_items <- function(data) {
    count_env <- new.env()
    count_env$localities <- length(data$localities)
    count_env$loggers <- 0
    count_env$sensors <- 0

    sensors_item_function <- function(item) {
        count_env$sensors <- count_env$sensors + length(item$sensors)
    }

    raw_locality_function <- function(locality) {
        count_env$loggers <- count_env$loggers + length(locality$loggers)
        purrr::walk(locality$loggers, sensors_item_function)
    }

    if(.common_is_agg_format(data)) {
        purrr::walk(data$localities, sensors_item_function)
    } else {
        purrr::walk(data$localities, raw_locality_function)
    }
    return(count_env)
}

.common_check_agg_use_utc <- function(use_utc, period) {
    if(use_utc) {
        return(use_utc)
    }
    if(period %in% .agg_const_INTERVAL_PERIODS) {
        warning(.common_const_MESSAGE_LONG_PERIOD_LOCAL_TIME)
        return(TRUE)
    }
    period_object <- lubridate::period(period)
    if(period_object[[1]]@year == 0 && period_object[[1]]@month == 0 && period_object[[1]]@day == 0) {
        return(use_utc)
    }
    warning(.common_const_MESSAGE_LONG_PERIOD_LOCAL_TIME)
    return(TRUE)
}

.common_duplicated_abreast <- function(x) {
    compare_function <- function(item, i) {
        if(i == 1) {
            return(FALSE)
        }
        if(is.na(item))
        {
            return(is.na(x[i - 1]))
        }
        if(is.na(x[i - 1]))
        {
            return(FALSE)
        }
        return(item == x[i - 1])
    }

    result <- purrr::imap_dbl(x, compare_function)
    return(result)
}

.common_get_period_from_agg_data <- function(data) {
    if(data$metadata@period %in% .agg_const_INTERVAL_PERIODS) {
        return(lubridate::as.period(data$metadata@intervals))
    }
    return(data$metadata@period)
}

