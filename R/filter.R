.filter_const_MESSAGE_FILTERED_ALL <- "All data are removed by filter."

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

mc_filter <- function(data, localities=NULL, sensors=NULL, reverse=FALSE, stop_if_empty=TRUE) {
    is_agg_format <- .common_is_agg_format(data)

    sensors_item_function <- function(item) {
        filter_function <- if(reverse) purrr::discard else purrr::keep
        item$sensors <- filter_function(item$sensors, function(.x) .x$metadata@name %in% sensors)
        if(length(item$sensors) == 0) {
            return(NULL)
        }
        return(item)
    }

    locality_function <- function(locality) {
        is_in <- is.null(localities) || locality$metadata@locality_id %in% localities

        if(is.null(sensors)) {
            return(if(is_in == reverse) NULL else locality)
        }

        if(!reverse && !is_in) {
            return(NULL)
        }

        if (!is_agg_format) {
            loggers <- purrr::map(locality$loggers, sensors_item_function)
            locality$loggers <- purrr::discard(loggers, is.null)
            if(length(locality$loggers) == 0) {
                return(NULL)
            }
        } else {
            locality <- sensors_item_function(locality)
        }
        return(locality)
    }

    if(is.null(localities) && is.null(sensors) && !reverse) {
        return(data)
    }
    if(is.null(localities) && is.null(sensors) && reverse) {
        data$localities <- list()
    } else {
        out_localities <- purrr::map(data$localities, locality_function)
        data$localities <- purrr::discard(out_localities, is.null)
    }

    if(stop_if_empty && length(data$localities) == 0) {
        stop(.filter_const_MESSAGE_FILTERED_ALL)
    }
    data
}
