.filter_const_MESSAGE_FILTERED_ALL <- "All data are removed by filter."
.filter_const_MESSAGE_LOGGER_TYP_AGG <- "Logger types can be used only for raw format."

#' Filter data from myClim object
#' @description 
#' This function filter data by localities and sensors.
#' 
#' @details 
#' In default settings it returns the object containing input sensors / localities.
#' When you provide vector of localities e.g. `localities=c("A6W79", "A2E32")` 
#' selected localities are filtered with all sensors on those localities.
#' The same as When you provide vector of sensors `sensors=c("TMS_T1", "TMS_T2")`, 
#' selected sensors are filtered through all localities.
#' When you combine localities and sensors, then filtering return 
#' selected sensors on selected localities. 
#' 
#' When `reverse = TRUE` and using only localities parameter then 
#' the listed localities are removed. Similarly `reverse = TRUE` with proving only 
#' sensors parameter, then the listed sensors are removed from all localities. When 
#' using `reverse = TRUE` and combining sensors and localities parameters then
#' selected sensors are removed from selected localities.   
#' 
#' 
#' @template param_myClim_object
#' @param localities locality_ids for filtering data; if NULL then do nothing
#' @param sensors sensor_names for filtering data; if NULL then do nothing see `names(mc_data_sensors)`
#' @param reverse if TRUE then input localities and/or sensors are excluded (default FALSE)
#' @param stop_if_empty if TRUE then error for empty output (default TRUE)
#' @return filtered myClim object
#' @export
#' @examples 
#' ## keep only "A6W79", "A2E32" localities with all their sensors
#' filtered_data <- mc_filter(mc_data_example_raw, localities=c("A6W79", "A2E32"))
#' 
#' ## remove "A6W79", "A2E32" localities and keep all others
#' filtered_data <- mc_filter(mc_data_example_raw, localities=c("A6W79", "A2E32"), reverse=TRUE)
#' 
#' ## keep only "TMS_T1", and "TMS_T2" sensors on all localities
#' filtered_data <- mc_filter(mc_data_example_raw, sensors=c("TMS_T1", "TMS_T2"))
#' 
#' ## remove "TMS_T1", and "TMS_T2" sensors from all localities
#' filtered_data <- mc_filter(mc_data_example_raw, sensors=c("TMS_T1", "TMS_T2"),reverse=TRUE)
#' 
#' ## keep only "TMS_T1", and "TMS_T2" sensors on "A6W79", "A2E32" localities
#' filtered_data <- mc_filter(mc_data_example_raw, localities=c("A6W79", "A2E32"),
#'                            sensors=c("TMS_T1", "TMS_T2"))
#' 
#' ## remove "TMS_T1", and "TMS_T2" sensors from "A6W79", "A2E32" localities
#' ## and keep all other sensors and localities
#' filtered_data <- mc_filter(mc_data_example_raw, localities=c("A6W79", "A2E32"),
#'                            sensors=c("TMS_T1", "TMS_T2"), reverse=TRUE)

mc_filter <- function(data, localities=NULL, sensors=NULL, reverse=FALSE, stop_if_empty=TRUE, logger_types=NULL) {
    is_agg_format <- .common_is_agg_format(data)
    if(is_agg_format && !is.null(logger_types)){
        stop(.filter_const_MESSAGE_LOGGER_TYP_AGG)
    }

    sensors_item_function <- function(item, value) {
        if(length(value) == 1) {
            value <- if(reverse) !value else value
            if(value) {
                return(item)
            } else {
                return(NULL)
            }
        }
        filter_function <- if(reverse) purrr::discard else purrr::keep
        item$sensors <- filter_function(item$sensors, function(.x) value[[.x$metadata@name]])
        if(length(item$sensors) == 0) {
            return(NULL)
        }
        return(item)
    }

    locality_function <- function(value, locality_id) {
        locality <- data$localities[[locality_id]]
        if(is.logical(value) && length(value) == 1) {
            value <- if(reverse) !value else value
            if(value) {
                return(locality)
            } else {
                return(NULL)
            }
        }
        if (!is_agg_format) {
            loggers <- purrr::pmap(list(item = locality$loggers,
                                        value = value), sensors_item_function)
            locality$loggers <- purrr::discard(loggers, is.null)
            if(length(locality$loggers) == 0) {
                return(NULL)
            }
        } else {
            locality <- sensors_item_function(locality, value)
        }
        return(locality)
    }

    if(is.null(localities) && is.null(sensors) && is.null(logger_types) && !reverse) {
        return(data)
    }
    if(is.null(localities) && is.null(sensors) && is.null(logger_types) && reverse) {
        data$localities <- list()
    } else {
        prepared_filter <- .filter_get_prepared_filter(data, localities, sensors, logger_types, is_agg_format)
        out_localities <- purrr::imap(prepared_filter, locality_function)
        data$localities <- purrr::discard(out_localities, is.null)
    }

    if(stop_if_empty && length(data$localities) == 0) {
        stop(.filter_const_MESSAGE_FILTERED_ALL)
    }
    data
}

.filter_get_prepared_filter <- function(data, localities, sensors, logger_types, is_agg_format) {
    sensors_item_function <- function(item) {
        is_in <- is.null(logger_types) || item$metadata@type %in% logger_types
        if(!is_in) {
            return(FALSE)
        }

        if(is.null(sensors)) {
            return(TRUE)
        }

        sensors <- purrr::map_lgl(item$sensors, ~ .x$metadata@name %in% sensors)
        if(all(sensors)) {
            return(TRUE)
        }
        if(all(!sensors)) {
            return(FALSE)
        }
        return(sensors)
    }

    locality_function <- function(locality) {
        is_in <- is.null(localities) || locality$metadata@locality_id %in% localities

        if(!is_in) {
            return(FALSE)
        }

        if (!is_agg_format) {
            loggers <- purrr::map(locality$loggers, sensors_item_function)
            has_all_one_value <- all(purrr::map_lgl(loggers, ~ length(.x) == 0))
            if(has_all_one_value && all(loggers)){
                return(TRUE)
            }
            if(has_all_one_value && all(!loggers)){
                return(FALSE)
            }
            return(loggers)
        } else {
            return(sensors_item_function(locality))
        }
    }

    result <- purrr::map(data$localities, locality_function)
    return(result)
}