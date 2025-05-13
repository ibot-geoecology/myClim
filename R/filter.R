.filter_const_MESSAGE_FILTERED_ALL <- "All data are removed by filter."
.filter_const_MESSAGE_LOGGER_TYPE_AGG <- "Logger types can be used only for raw format."
.filter_const_MESSAGE_LOGGERS_AGG <- "Loggers can be used only for raw format."
.filter_const_MESSAGE_LOGGERS_BOTH <- "It is not allowed to combine loggers and logger_types parameter."

#' Filter data from myClim object
#' @description
#' This function filter data by localities, logger types and sensors.
#'
#' @details
#' In default settings it returns the object containing input localities / logger types / loggers / sensors.
#' When you provide vector of localities e.g. `localities=c("A6W79", "A2E32")`
#' selected localities are filtered with all loggers / sensors on those localities.
#' When you provide vector of loggers e.g. `loggers=c("TMS_1", "TMS_2")`
#' selected loggers are filtered through all localities.
#' The same as When you provide vector of logger_types `logger_types=c("TMS", "TMS_L45")`
#' selected loggers by type are filtered through all localities.
#' (loggers or logger_types criterion is applicable only for raw data format see [myClim-package]) and
#' the sensors parameter `sensors=c("TMS_T1", "TMS_T2")`,
#' selected sensors are filtered through all localities.
#' When you combine localities, logger_types/loggers and sensors, then filtering return
#' selected sensors in selected loggers on selected localities.
#'
#' Parameter `reverse = TRUE` returns myClim object complemented to object filtered by parameter `reverse = FALSE`.
#' 
#' * `reverse = TRUE` and `localities` are selected then the listed localities are removed from myClim object.
#' * `reverse = TRUE` and `loggers` are selected then the listed loggers are removed from all localities.
#' * `reverse = TRUE` and `logger_types` are selected then the listed logger types are removed from all localities.
#' * `reverse = TRUE` and `sensors` are selected then listed sensors are removed from all loggers / localities.
#' * `reverse = TRUE` and `localities` and `loggers` are selected then the listed loggers are removed only from listed localities.
#'
#' Only one of parameters loggers or logger_types can be used.
#'
#' @template param_myClim_object
#' @param localities locality_ids for filtering data; if NULL then do nothing (default NULL)
#' @param logger_types types of logger for filtering data; if NULL then do nothing (default NULL).
#' The logger_types parameter can by used only for raw data format see [myClim-package].
#' @param loggers logger_names for filtering data; if NULL then do nothing (default NULL).
#' The loggers parameter can by used only for raw data format see [myClim-package].
#' @param sensors sensor_names for filtering data; if NULL then do nothing see `names(mc_data_sensors)` (default NULL)
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
#' ## Remove "Dendro" loggers on all localities
#' filtered_data <- mc_filter(mc_data_example_raw, logger_types="Dendro", reverse=TRUE)

mc_filter <- function(data, localities=NULL, logger_types=NULL, loggers=NULL,
                      sensors=NULL, reverse=FALSE, stop_if_empty=TRUE) {
    is_agg_format <- .common_is_agg_format(data)
    if(is_agg_format && !is.null(logger_types)){
        stop(.filter_const_MESSAGE_LOGGER_TYPE_AGG)
    }
    if(is_agg_format && !is.null(loggers)){
        stop(.filter_const_MESSAGE_LOGGERS_AGG)
    }
    if(!is.null(logger_types) && !is.null(loggers)){
        stop(.filter_const_MESSAGE_LOGGERS_AGG)
    }

    sensors_table <- .filter_get_sensors_table(data)
    selected <- rep(TRUE, nrow(sensors_table))
    if(!is.null(localities)) {
        selected <- selected & sensors_table$locality_id %in% localities
    }
    if(!is.null(logger_types)) {
        selected <- selected & sensors_table$logger_type %in% logger_types
    }
    if(!is.null(loggers)) {
        selected <- selected & sensors_table$logger_name %in% loggers
    }
    if(!is.null(sensors)) {
        selected <- selected & sensors_table$sensor_name %in% sensors
    }
    if(reverse) {
        selected <- !selected
    }
    sensors_table <- dplyr::filter(sensors_table, selected)
    if(nrow(sensors_table) == 0) {
        stop(.filter_const_MESSAGE_FILTERED_ALL)
    }

    result_env <- new.env()
    result_env$data <- data
    result_env$data$localities <- list()

    row_function <- function(locality_id, logger_name, sensor_name) {
        if(!(locality_id %in% names(result_env$data$localities))) {
            result_env$data$localities[[locality_id]] <- data$localities[[locality_id]]
            if(is_agg_format) {
                result_env$data$localities[[locality_id]]$sensors <- list()
            } else {
                result_env$data$localities[[locality_id]]$loggers <- list()
            }
        }
        if(!is_agg_format && !(logger_name %in% names(result_env$data$localities[[locality_id]]$loggers))) {
            result_env$data$localities[[locality_id]]$loggers[[logger_name]] <- data$localities[[locality_id]]$loggers[[logger_name]]
            result_env$data$localities[[locality_id]]$loggers[[logger_name]]$sensors <- list()
        }
        if(is_agg_format) {
            result_env$data$localities[[locality_id]]$sensors[[sensor_name]] <-
                data$localities[[locality_id]]$sensors[[sensor_name]]
        } else {
            result_env$data$localities[[locality_id]]$loggers[[logger_name]]$sensors[[sensor_name]] <-
                data$localities[[locality_id]]$loggers[[logger_name]]$sensors[[sensor_name]]
        }
    }
    
    purrr::pwalk(dplyr::select(sensors_table, "locality_id", "logger_name", "sensor_name"), row_function)

    return(result_env$data)
}

.filter_get_sensors_table <- function(data) {
    is_raw_format <- .common_is_raw_format(data)

    sensors_item_function <- function(locality_id, item) {
        sensor_names <- names(item$sensors)
        logger_name <- NA_character_
        logger_type <- NA_character_
        if(is_raw_format) {
            logger_name <- item$metadata@name
            logger_type <- item$metadata@type
        }
        count <- length(sensor_names)
        result <- tibble::tibble(locality_id=rep(locality_id, count),
                                 logger_name=rep(logger_name, count),
                                 logger_type=rep(logger_type, count),
                                 sensor_name=sensor_names)
        return(result)
    }

    raw_locality_function <- function(locality) {
        params <- list(locality_id=rep(locality$metadata@locality_id, length(locality$loggers)),
                       item=locality$loggers)
        return(purrr::pmap_dfr(params, sensors_item_function))
    }

    if(is_raw_format) {
        result <- purrr::map_dfr(data$localities, raw_locality_function)
    } else {
        params <- list(locality_id=names(data$localities),
                       item=data$localities)
        result <- purrr::pmap_dfr(params, sensors_item_function)
    }
    return(result)
}