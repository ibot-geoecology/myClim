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
#' @export
#' @examples
#' mc_eco_snow(example_tms_data1, "T3")
mc_eco_snow <- function(data, sensor, localities=c(), dr=2, tmax=0.5, interval_length=15) {
    data <- microclim:::.common_get_filtered_data(data, localities, sensor)
    loggers <- unname(do.call(c, lapply(data, function(x) x$loggers)))
    snow_tables <- lapply(loggers, function(x) .get_eco_snow_from_logger(x, dr, tmax, interval_length))
    Reduce(function(x, y) merge(x, y, by="date", all=TRUE), snow_tables)
}

.get_eco_snow_from_logger <- function(logger, dr, tmax, interval_length=15) {
    data_table <- microclim:::.reshape_wideformat_interval_logger(logger, interval_length)
    records_in_day <-  (24*60*60)/(as.numeric(data_table$datetime[[2]]) - as.numeric(data_table$datetime[[1]]))

    temp_sma <- TTR::SMA(data_table[[2]], n = records_in_day)
    temp_delta <- data_table[[2]] - temp_sma

    max_values <- aggregate(data_table[[2]], by=list(day=cut(data_table$datetime, breaks="days")), FUN=max)[,2]
    min_values <- aggregate(data_table[[2]], by=list(day=cut(data_table$datetime, breaks="days")), FUN=min)[,2]
    range <- max_values - min_values

    max_delta <- aggregate(temp_delta, by=list(day=cut(data_table$datetime, breaks="days")), FUN = max)[,2]
    min_delta <- aggregate(temp_delta, by=list(day=cut(data_table$datetime, breaks="days")), FUN = min)[,2]
    delta_range <- max_delta - min_delta
    result = data.frame(date=as.Date(unique(cut(data_table$datetime, breaks="days"))))
    result[[logger$metadata@serial_number]] <- (delta_range < dr) & (max_values < tmax)
    return(result)
}
