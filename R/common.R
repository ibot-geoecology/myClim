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
    unname(do.call(c, lapply(data, function(x) x$loggers)))
}

.common_sensor_values_as_tibble <- function(item) {
    data <- c(list(datetime=item$datetime), purrr::map(item$sensors, function(.x) .x$values))
    tibble::as_tibble(data)
}

.common_is_calc_format <- function(data) {
    length(data) == 2 && all(names(data) == c("metadata", "localities"))
}

.common_stop_if_not_calc_format <- function(data) {
    if(!.common_is_calc_format(data)) {
        stop("Format of data isn't right for calculation. It is required convert the data with function mc_agg.")
    }
}

.common_is_prep_format <- function(data) {
    !.common_is_calc_format(data)
}

.common_stop_if_not_prep_format <- function(data) {
    if(!.common_is_prep_format(data)) {
        stop("Format of data isn't right for preparing. Use data before converting with function mc_agg.")
    }
}

.common_get_new_sensor <- function(sensor_id, sensor_name, values=NULL, calibrated=FALSE){
    metadata <- new("mc_SensorMetadata")
    metadata@sensor_id <- sensor_id
    metadata@name <- sensor_name
    metadata@calibrated <- calibrated
    calibration <- .common_get_calibration_to_new_sensor(sensor_id)
    item <- list(metadata = metadata,
                 values = values,
                 calibration = calibration,
                 states = list())
    item
}

.common_get_calibration_to_new_sensor <- function(sensor_id) {
    if(is.na(sensor_id)) {
        return(NULL)
    }
    sensor_info <- mc_data_sensors[[sensor_id]]
    if(is.na(sensor_info@physical)) {
        return(NULL)
    }
    physical <- mc_data_physical[[sensor_info@physical]]
    if(is.na(physical@calibration_class)) {
        return(NULL)
    }
    new(physical@calibration_class)
}

.common_get_localities <- function(data) {
    if(.common_is_calc_format(data)) {
        return(data$localities)
    }
    data
}

.common_get_id_of_item_with_sensors <-function(item) {
    if("locality_id" %in% slotNames(item$metadata)) {
        return(item$metadata@locality_id)
    }
    item$metadata@serial_number
}

.common_set_localities <- function(data, localities) {
    if(.common_is_calc_format(data)) {
        data$localities <- localities
        return(data)
    }
    localities
}

.common_get_cleaned_data_range <- function(data) {
    datetime_items_function <- function(item) {
        list(start=dplyr::first(item$datetime),
             end=dplyr::last(item$datetime))
    }
    if(.common_is_calc_format(data)) {
        items <- data$localities
    } else {
        items <- purrr::flatten(purrr::map(data, ~ .x$loggers))
    }
    table <- purrr::map_dfr(items, datetime_items_function)
    lubridate::interval(min(table$start), max(table$end))
}

