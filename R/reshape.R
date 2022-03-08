#' Wideformat of sensor values
#'
#' This function create data.frame with values of sensor in wide format.
#'
#' @param data in format for preparing or calculation
#' @param localities names of localities; if NULL then all (default NULL)
#' @param sensors names of sensors; if NULL then all (default NULL)
#' @return data.frame with datetime column and columns for every sensor; Name of column is in format
#' {locality_id}_{serial_number}_{sensor_name} for preparing format and
#' {locality_id}_{sensor_name} for calculation format.
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

#' Longformat of sensor values
#'
#' This function create data.frame with values of sensor
#'
#' @param data in format for preparing or calculation
#' @param localities locality_ids; if NULL then all (default NULL)
#' @param sensors names of sensors; if NULL then all (default NULL)
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

    sensor_function <- function(locality_id, serial_number, datetime, sensor_item) {
        count <- length(datetime)
        tibble::tibble(locality_id=rep(locality_id, count),
                       serial_number=rep(serial_number, count),
                       sensor_name=rep(sensor_item$metadata@name, count),
                       datetime=datetime,
                       value=sensor_item$values)
    }

    sensors_item_function <- function(locality_id, item) {
        serial_number <- NA_character_
        if(is_prep_format) {
            serial_number <- item$metadata@serial_number
        }
        tables <- purrr::pmap_dfr(list(locality_id=locality_id, serial_number=serial_number,
                                       datetime=list(item$datetime), sensor=item$sensors),
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
    result
}


