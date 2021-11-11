#' Snow detection
#'
#' Function detect snow based on detrended time series
#'
#' @param data all data in standard format
#' @param sensor name of temperature sensor
#' @param localities names of localities; if empty then all
#' @param dr delta range
#' @param tmax maximal temperature
#' @return data.frame with datetime column and logical columns named by serial_number of loggers
#' @export
#' @examples
#' mc_eco_snow(example_tomst_data1, "TMS_T3")
mc_eco_snow <- function(data, sensor, localities=c(), dr=2, tmax=0.5) {
    data <- microclim:::.common_get_filtered_data(data, localities, sensor)
    loggers <- unname(do.call(c, lapply(data, function(x) x$loggers)))
    snow_tables <- purrr::map(loggers, function(x) .get_eco_snow_from_logger(x, dr, tmax))
    result <- purrr::reduce(snow_tables, function(x, y) dplyr::full_join(x, y, by="datetime"))
    as.data.frame(result)
}

.get_eco_snow_from_logger <- function(logger, dr, tmax) {
    microclim:::.clean_warn_if_datetime_step_unprocessed(logger)
    result = tibble::tibble(datetime=logger$datetime)
    if(length(logger$sensors) == 0){
        result[[logger$metadata@serial_number]] <- NA
        return(result)
    }
    day_max_temp <- runner::runner(logger$sensors[[1]]$values, k=3600*24, idx=logger$datetime, f=function(x) if(length(x) == 0) NA else max(x), na_pad=TRUE)
    day_range_temp <- runner::runner(logger$sensors[[1]]$values, k=3600*24, idx=logger$datetime, f=function(x) if(length(x) == 0) NA else max(x) - min(x), na_pad=TRUE)
    result[[logger$metadata@serial_number]] <- (day_range_temp < dr) & (day_max_temp < tmax)
    return(result)
}

#' Snow detection summary
#'
#' Function return summary info about snow detection
#'
#' @param snow_data data from function mc_eco_snow
#' @param period count days for continuous cover of snow (default 3)
#' @return data.frame with columns serial_number, snow_days, first_day, last_day, first_day_period, last_day_period
#' @export
#' @examples
#' data <- mc_eco_snow(example_tomst_data1, "TMS_T3")
#' mc_eco_snow_agg(data)
mc_eco_snow_agg <- function(snow_data, period = 3) {
    result <- data.frame(serial_number=character(),
                         snow_days=numeric(),
                         first_day=as.Date(x = integer(0), origin = "1970-01-01"),
                         last_day=as.Date(x = integer(0), origin = "1970-01-01"),
                         first_day_period=as.Date(x = integer(0), origin = "1970-01-01"),
                         last_day_period=as.Date(x = integer(0), origin = "1970-01-01"))
    for(serial_number in colnames(snow_data)[-1]) {
        result[nrow(result) + 1, ] <- .eco_get_snow_agg_row(snow_data, serial_number, period)
    }
    result
}

.eco_get_snow_agg_row <- function(snow_data, serial_number, period) {
    snow_data <- snow_data[!is.na(snow_data[[serial_number]]), ]
    if(nrow(snow_data) == 0) {
        return(list(serial_number=serial_number, snow_days=0,
             first_day=NA, last_day=NA,
             first_day_period=NA, last_day_period=NA))
    }
    snow_days_table <- aggregate(snow_data[[serial_number]], by=list(day=cut(snow_data$datetime, breaks = "days")), FUN=max)
    snow_days_table$day <- as.Date(snow_days_table$day)
    snow_days <- sum(snow_days_table$x)
    if(snow_days == 0) {
        last_day <- NA
        first_day <- NA
    }
    else {
        last_day <- as.Date(snow_days_table$day[max(which(snow_days_table$x == 1))])
        first_day <- as.Date(snow_days_table$day[min(which(snow_days_table$x == 1))])
    }
    snow_by_period <- runner::runner(snow_days_table$x, k=period, idx=as.Date(snow_days_table$day), f=function(x) if(length(x) == 0) NA else min(x), na_pad=TRUE)
    snow_by_period_index <- which(snow_by_period == 1)
    if(length(snow_by_period_index) == 0) {
        last_day_period <- NA
        first_day_period <- NA
    }
    else{
        last_day_period <- snow_days_table$day[max(snow_by_period_index)]
        first_day_period <- snow_days_table$day[min(snow_by_period_index) - period + 1]
    }
    list(serial_number=serial_number, snow_days=snow_days,
         first_day=first_day, last_day=last_day,
         first_day_period=first_day_period, last_day_period=last_day_period)
}
