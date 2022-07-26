.reshape_const_MESSAGE_UNCLEANED <- "Logger {serial_number} isn't cleaned. I can't detect the last time_to."

#' Wideformat of sensor values
#'
#' This function converts myClim object to data.frame with values of sensor in wide format.
#' @details First columnt of the output data.frame is datetime followed by the
#' columns for every sensor. Name of the column is in format:
#' * localityid_serialnumber_sensorname for Prep-format
#' * localityid_sensorname for Calc-format
#' @param data myClim object in Prep-format or Calc-format (see [myClim-package])
#' @param localities names of localities; if NULL then all (default NULL)
#' @param sensors names of sensors; if NULL then all (default NULL) see `names(mc_data_sensors)`
#' @return data.frame with columns:
#' * datetime 
#' * locality1_sensor1
#' * ...
#' * ...
#' * localityn_sensorn
#' 
#' @export
#' @examples
#' example_tms_wideformat <- mc_reshape_wide(example_tomst_data1, c("A6W79", "A2E32"), c("TMS_T1", "TMS_T2"))
mc_reshape_wide <- function(data, localities=NULL, sensors=NULL) {
    data <- mc_filter(data, localities, sensors)
    datetimes <- .reshape_get_all_datetimes(data)
    tables <- c(tibble::tibble(datetimes), .reshape_get_sensor_tables(data))
    tables[[1]] <- tibble::as_tibble(tables[[1]])
    colnames(tables[[1]]) <- "datetime"
    as.data.frame(purrr::reduce(tables, function(.x, .y) dplyr::left_join(.x, .y, by="datetime")))
}

.reshape_get_all_datetimes <- function(data){
    is_calc_format <- myClim:::.common_is_calc_format(data)
    locality_function <- function(locality) {
        if(is_calc_format) {
            return(locality$datetime)
        }
        datetimes <- purrr::map(locality$loggers, function(x) x$datetime)
        purrr::reduce(datetimes, union)
    }
    locality_datetimes <- purrr::map(myClim:::.common_get_localities(data), locality_function)
    datetimes <- purrr::reduce(locality_datetimes, union)
    datetimes <- sort(datetimes)
    myClim:::.common_as_utc_posixct(datetimes)
}

.reshape_get_sensor_tables <- function(data) {
    sensors_function <- function(item, name_prefix) {
        table <- myClim:::.common_sensor_values_as_tibble(item)
        colnames(table)[-1] <- purrr::map_chr(colnames(table)[-1], function(x) stringr::str_glue("{name_prefix}_{x}"))
        table
    }

    if(myClim:::.common_is_calc_format(data)) {
        return(purrr::map2(data$localities, names(data$localities), sensors_function))
    }

    prep_locality_function <- function(locality) {
        prefixes <- purrr::map_chr(locality$loggers, function(x) stringr::str_glue("{locality$metadata@locality_id}_{x$metadata@serial_number}"))
        purrr::map2(locality$loggers, prefixes, sensors_function)
    }
    result <- purrr::map(data, prep_locality_function)
    purrr::flatten(result)
}

#' Long format of sensor values
#'
#' This function converts myClim object to long data.frame with sensor records.
#'
#' @param data myClim object in Prep-format or Calc-format (see [myClim-package])
#' @param localities locality_ids; if NULL then all (default NULL)
#' @param sensors names of sensors; if NULL then all (default NULL) see `names(mc_data_sensors)`
#' @return data.frame
#'
#' columns:
#' * locality_id
#' * serial_number
#' * sensor_name
#' * datetime
#' * value
#' @export
#' @examples
#' head(mc_reshape_long(mc_data_example_clean, c("A6W79", "A2E32"), c("TMS_T1", "TMS_T2")), 10)
mc_reshape_long <- function(data, localities=NULL, sensors=NULL) {
    data <- mc_filter(data, localities, sensors)
    is_prep_format <- myClim:::.common_is_prep_format(data)
    period <- NULL
    if (!is_prep_format && !(data$metadata@period %in% myClim:::.agg_const_INTERVAL_PERIODS)) {
        period <- lubridate::period(data$metadata@period)
    }

    sensor_function <- function(locality_id, serial_number, sensor_item, datetime, time_to) {
        count <- length(datetime)
        tibble::tibble(locality_id=rep(locality_id, count),
                       serial_number=rep(serial_number, count),
                       sensor_name=rep(sensor_item$metadata@name, count),
                       height=rep(sensor_item$metadata@height, count),
                       datetime=datetime,
                       time_to=time_to,
                       value=sensor_item$values)
    }

    sensors_item_function <- function(locality_id, item) {
        serial_number <- NA_character_
        if(is_prep_format) {
            serial_number <- item$metadata@serial_number
            if(is.na(item$clean_info@step)) {
                warning(stringr::str_glue(.reshape_const_MESSAGE_UNCLEANED))
            }
            period <- lubridate::minutes(item$clean_info@step)
        }
        if(!is.null(period)) {
            time_to <- c(item$datetime[-1], dplyr::last(item$datetime) + period)
        } else {
            first_index <- match(dplyr::first(item$datetime), data$metadata@intervals_start)
            intervals_end <- data$metadata@intervals_end[seq(first_index, first_index + length(item$datetime) - 1)]
            time_to <- intervals_end + lubridate::seconds(1)
        }
        tables <- purrr::pmap_dfr(list(locality_id=locality_id, serial_number=serial_number,
                                       sensor_item=item$sensors, datetime=list(item$datetime),
                                       time_to=list(time_to)),
                                  sensor_function)
    }

    prep_locality_function <- function(locality) {
        purrr::map2_dfr(locality$metadata@locality_id, locality$loggers, sensors_item_function)
    }

    if(is_prep_format) {
        result <- purrr::map_dfr(data, prep_locality_function)
    } else {
        result <- purrr::map2_dfr(names(data$localities), data$localities, sensors_item_function)
    }
    as.data.frame(result)
}


