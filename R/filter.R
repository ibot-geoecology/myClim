.filter_const_MESSAGE_FILTERED_ALL <- "All data are removed by filter."
.filter_const_MESSAGE_LOGGER_TYP_AGG <- "Logger types can be used only for raw format."
.filter_const_MESSAGE_REVERSE_COMBINATION <- "The reverse parameter can only be used for a single filter condition. Is not allowed to combine more filter conditions with reverse parameter."

#' Filter data from myClim object
#' @description
#' This function filter data by localities, logger types and sensors.
#'
#' @details
#' In default settings it returns the object containing input localities / logger types / sensors.
#' When you provide vector of localities e.g. `localities=c("A6W79", "A2E32")`
#' selected localities are filtered with all loggers / sensors on those localities.
#' The same as When you provide vector of logger_types `logger_types=c("TMS", "TMS_L45")`
#' selected loggers by type are filtered through all localities 
#' (logger_types criterion is applicable only for raw data format see [myClim-package]) and
#' the sensors parameter `sensors=c("TMS_T1", "TMS_T2")`,
#' selected sensors are filtered through all localities.
#' When you combine localities, logger_types and sensors, then filtering return
#' selected sensors in selected loggers on selected localities.
#'
#' Parameter `reverse = TRUE` returns myClim object without listed localities, or logger types or sensors.
#' Using  `reverse = TRUE` is not allowed for combination of localities and logger types and sensors.
#' It is allowed to use `reverse` only with single filter criterion either locality, logger type or sensor.
#' 
#' * `reverse = TRUE` and `logger_types` are selected then the listed logger types are removed from all localities.
#' * `reverse = TRUE` and `localities` are selected then the listed localities are removed from myClim object. 
#' * `reverse = TRUE` and `sensors` are selected then listed sensors are removed from all loggers / localities.
#'
#' @template param_myClim_object
#' @param localities locality_ids for filtering data; if NULL then do nothing (default NULL)
#' @param logger_types types of logger for filtering data; if NULL then do nothing (default NULL). 
#' The logger_types parameter can by used only for raw data format see [myClim-package].
#' @param sensors sensor_names for filtering data; if NULL then do nothing see `names(mc_data_sensors)` (default NULL)
#' @param reverse if TRUE then input localities and/or sensors are excluded (default FALSE)
#' @param stop_if_empty if TRUE then error for empty output (default TRUE)
#'
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
#' ## Remove "Dendro" loggers on all localities
#' filtered_data <- mc_filter(mc_data_example_raw, logger_types="Dendro", reverse=TRUE)

mc_filter <- function(data, localities=NULL, sensors=NULL, reverse=FALSE, stop_if_empty=TRUE, logger_types=NULL) {
    is_agg_format <- .common_is_agg_format(data)
    if(is_agg_format && !is.null(logger_types)){
        stop(.filter_const_MESSAGE_LOGGER_TYP_AGG)
    }

    if(reverse && sum(c(!is.null(localities), !is.null(sensors), !is.null(logger_types))) > 1) {
        stop(.filter_const_MESSAGE_REVERSE_COMBINATION)
    }

    sensors_item_function <- function(item) {
        is_in <- is.null(logger_types) || item$metadata@type %in% logger_types
        if(is.null(sensors)) {
            return(if(is_in == reverse) NULL else item)
        }
        if(!reverse && !is_in) {
            return(NULL)
        }
        filter_function <- if(reverse) purrr::discard else purrr::keep
        item$sensors <- filter_function(item$sensors, function(.x) .x$metadata@name %in% sensors)
        if(length(item$sensors) == 0) {
            return(NULL)
        }
        return(item)
    }

    locality_function <- function(locality) {
        is_in <- is.null(localities) || locality$metadata@locality_id %in% localities

        if(is.null(sensors) && is.null(logger_types)) {
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

    if(is.null(localities) && is.null(sensors) && is.null(logger_types) && !reverse) {
        return(data)
    }
    if(is.null(localities) && is.null(sensors) && is.null(logger_types) && reverse) {
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
