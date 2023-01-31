.filter_const_MESSAGE_FILTERED_ALL <- "All data are removed by filter."

#' Filter data from myClim object
#' @description 
#' This function filter data by localities and sensors.
#' 
#' @details 
#' In default settings it returns the object containing input sensors / localities.
#' It is possible to filter selected sensors from all localities, selected 
#' localities with all sensors or combine both. 
#' 
#' When `reverse = TRUE` then 
#' resulting myClim object contains all localities and sensors as input except
#' those you listed in parameters `localities, sensors`
#' 
#' @template param_myClim_object
#' @param localities locality_ids for filtering data; if NULL then do nothing
#' @param sensors sensor_ids for filtering data; if NULL then do nothing see `names(mc_data_sensors)`
#' @param reverse if TRUE then input localities and/or sensors are excluded (default FALSE)
#' @param stop_if_empty if TRUE then error for empty output (default TRUE)
#' @return filtered myClim object
#' @export
#' @examples 
#' filtered_data <- mc_filter(mc_data_example_raw, localities=c("A6W79", "A2E32"),
#'                            sensors=c("TMS_T1", "TMS_T2"))
#' mc_info(filtered_data)
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
