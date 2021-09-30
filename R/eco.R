#' Snow detection
#'
#' Function detect snow based on detrended time series
#'
#' @param data all data in standard format
#' @param sensor name of temperature sensor
#' @param localities names of localities; if empty then all
#' @param dr delta range
#' @param tmax maximal temperature
#' @param interval_length length of interval in minutes (default 15)
#' @return data.frame with datetime column and logical columns named by serial_number of loggers
#' @export
#' @examples
#' mc_eco_snow(example_tms_data1, "T3")
mc_eco_snow <- function(data, sensor, localities=c(), dr=2, tmax=0.5, interval_length=15) {
    data <- microclim:::.common_get_filtered_data(data, localities, sensor)
    loggers <- unname(do.call(c, lapply(data, function(x) x$loggers)))
    snow_tables <- lapply(loggers, function(x) .get_eco_snow_from_logger(x, dr, tmax, interval_length))
    Reduce(function(x, y) merge(x, y, by="datetime", all=TRUE), snow_tables)
}

.get_eco_snow_from_logger <- function(logger, dr, tmax, interval_length=15) {
    data_table <- microclim:::.reshape_wideformat_interval_logger(logger, interval_length)
    day_max_temp <- runner::runner(data_table[[2]], k=3600*24, idx=data_table$datetime, f=max)
    day_range_temp <- runner::runner(data_table[[2]], k=3600*24, idx=data_table$datetime, f=function(x) {max(x) - min(x)})
    result = data.frame(datetime=data_table$datetime)
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
#' data <- mc_eco_snow(example_tms_data1, "T3")
#' mc_eco_snow_agg(data)
mc_eco_snow_agg <- function(snow_data, period = 3) {
    result <- data.frame(serial_number=character(), snow_days=numeric(),
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
    snow_days_table <- aggregate(snow_data[[serial_number]], by=list(day=cut(snow_data$datetime, breaks = "days")), FUN=max)
    snow_days_table$day <- as.Date(snow_days_table$day)
    snow_days <- sum(snow_days_table$x)
    last_day <- as.Date(snow_days_table$day[max(which(snow_days_table$x == 1))])
    first_day <- as.Date(snow_days_table$day[min(which(snow_days_table$x == 1))])
    snow_by_period <- runner::runner(snow_days_table$x, k=period, idx=as.Date(snow_days_table$day), f=min)
    snow_by_period_index <- which(snow_by_period == 1)
    if(length(snow_by_period_index) == 0) {
        last_day_period <- NA
        first_day_period <- NA
    }
    else{
        last_day_period <- snow_days_table$day[max(snow_by_period_index)]
        first_day_period <- snow_days_table$day[min(snow_by_period_index)]
    }
    list(serial_number=serial_number, snow_days=snow_days,
         first_day=first_day, last_day=last_day,
         first_day_period=first_day_period, last_day_period=last_day_period)
}
