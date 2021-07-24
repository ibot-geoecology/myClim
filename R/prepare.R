#' TMS3 logger raw data reading
#'
#' This function read raw data of TMS3 logger and return data in standard format.
#'
#' @param filename path to source file
#' @param serial_number serial number of logger
#' @param id_locality id of locality
#' @return data in standard format
#' @export
prepare.read_TMS3_logger <- function(filename, serial_number, id_locality) {
    prepare.read_logger(filename, serial_number, id_locality, data.source_data_formats$TMS3, "UTC")
}

#' Logger raw data reading
#'
#' This function read raw data of logger and return data in standard format.
#'
#' @param filename path to source file
#' @param serial_number serial number of logger
#' @param id_locality id of locality
#' @param data_format definition of format source file
#' @param tz time zone of dates in source data
#' @return data in standard format
#' @export
prepare.read_logger <- function(filename, serial_number, id_locality, data_format, tz = "UTC") {
    skip <- if(data_format@has_header) 1 else 0
    data_table <- read.table(filename,
                             sep = data_format@separator,
                             skip = skip,
                             stringsAsFactors = FALSE,
                             na.strings = data_format@na_strings)
    data_format <- model.load_info_from_data(data_format, data_table)
    data_table$date_UTC <- as.POSIXct(strptime(data_table[[data_format@date_column]], data_format@date_format, tz))
    metadata <- new("model.LoggerMetadata",
                    serial_number = serial_number,
                    id_locality = id_locality)
    list(metadata = metadata,
         sensors_data = .get_sensors_data_list(data_table, data_format))
}

.get_sensors_data_list <- function(data_table, data_format){
    result <- list()
    for(sensor_name in names(data_format@columns))
    {
        sensor_data <- data.frame(data_table$date_UTC, data_table[data_format@columns[[sensor_name]]])
        colnames(sensor_data) <- c("date", "value")
        item <- new("model.SensorData",
                    sensor = sensor_name,
                    data = sensor_data)
        result[[sensor_name]] <- item
    }
    result
}
