#' Data files reading by CSV
#'
#' This function read raw data from loggers by table saved in CSV file
#'
#' @param csv_with_files_table data.frame
#' @return data in standard format
#' @export
prepare.read_files_by_csv <- function(csv_with_files_table) {
    files_table <- read.table(csv_with_files_table,
                              header = TRUE,
                              sep = ",",
                              stringsAsFactors = FALSE)
    prepare.read_files(files_table)
}

#' Data files reading
#'
#' This function read raw data from loggers by data.frame with files description
#'
#' @param files_table data.frame
#' @return data in standard format
#' @export
prepare.read_files <- function(files_table) {
    if(nrow(files_table) == 0)
    {
        return(list())
    }
    result = list()
    for(i in 1:nrow(files_table))
    {
        row <- files_table[i, ]
        result[[row$locality_id]] <- .add_logger_to_locality(result[[row$locality_id]], row)
    }
    result
}

.add_logger_to_locality <- function(current_locality, row) {
    if(is.null(current_locality))
    {
        metadata <- new("model.LocalityMetadata",
                        id = row$locality_id)
        current_locality <- list(metadata = metadata, loggers=list())
    }
    new_index <- length(current_locality$loggers) + 1
    current_locality$loggers[[new_index]] <- .functions_read_logger[[row$logger]](row$path, row$serial_number)
    current_locality
}

.read_TMS_logger <- function(filename, serial_number=NULL) {
    .read_logger(filename, data.source_data_formats$TMS, serial_number, "UTC")
}

.read_logger <- function(filename, data_format, serial_number=NULL, tz = "UTC") {
    if(is.null(serial_number) | is.na(serial_number)){
        serial_number <- model.get_serial_number_from_filename(data_format, filename)
    }
    skip <- if(data_format@has_header) 1 else 0
    data_table <- read.table(filename,
                             sep = data_format@separator,
                             skip = skip,
                             stringsAsFactors = FALSE,
                             na.strings = data_format@na_strings)
    data_format <- model.load_info_from_data(data_format, data_table)
    datetime <- as.POSIXct(strptime(data_table[[data_format@date_column]], data_format@date_format, tz))
    metadata <- new("model.LoggerMetadata",
                    serial_number = serial_number)
    list(metadata = metadata,
         datetime = datetime,
         sensors_data = .get_sensors_data_list(data_table, data_format))
}

.get_sensors_data_list <- function(data_table, data_format){
    result <- list()
    for(sensor_name in names(data_format@columns))
    {
        values <- data_table[[data_format@columns[[sensor_name]]]]
        item <- new("model.SensorData",
                    sensor = sensor_name,
                    values = values)
        result[[sensor_name]] <- item
    }
    result
}

.functions_read_logger <- list(
    TMS = .read_TMS_logger)
