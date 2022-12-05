.common_const_MESSAGE_UNKNOWN_SENSOR_ID <- "Sensor_id {sensor_id} is unknown."

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
        stop("Format of input data isn't correct for preparing. Use non-aggregated data.")
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
    state_function <- function(tag, start, end, value) {
        state_interval <- lubridate::interval(start, end)
        new_state_intervals <- purrr::discard(lubridate::intersect(state_interval, intervals), ~ is.na(.x))
        return(list(tag=tag,
                    start=lubridate::int_start(new_state_intervals),
                    end=lubridate::int_end(new_state_intervals),
                    value=value))
    }

    as.data.frame(purrr::pmap_dfr(states_table, state_function))
}

.common_get_time_series_intervals <- function(datetime, filter) {
    rle_output <- rle(filter)
    cumsum_lengths <- cumsum(rle_output$lengths)
    start_indexes <- (c(0, cumsum_lengths[-length(cumsum_lengths)]) + 1)[rle_output$values]
    end_indexes <- cumsum_lengths[rle_output$values]
    lubridate::interval(datetime[start_indexes], datetime[end_indexes])
}
