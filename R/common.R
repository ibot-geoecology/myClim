.common_convert_factors_in_dataframe <- function(dataframe) {
    factor_columns <- sapply(dataframe, is.factor)
    if(!any(factor_columns)) {
        return(dataframe)
    }
    dataframe[factor_columns] <- lapply(dataframe[factor_columns], as.character)
    return(dataframe)
}

.common_get_sensor_info <- function(sensor_metadata) {
    microclim::mc_data_sensors[[sensor_metadata@sensor_id]]
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
        stop("Format of data isn't right for calculation.")
    }
}

.common_is_prep_format <- function(data) {
    !.common_is_calc_format(data)
}

.common_stop_if_not_prep_format <- function(data) {
    if(!.common_is_prep_format(data)) {
        stop("Format of data isn't right for preparing.")
    }
}

.common_get_new_sensor <- function(sensor_name, sensor_id=NA_character_, values=NULL){
    metadata <- new("mc_SensorMetadata")
    metadata@sensor_id <- sensor_id
    metadata@name <- sensor_name
    item <- list(metadata = metadata,
                 values = values,
                 states = list())
    item
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

