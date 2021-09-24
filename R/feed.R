#' Reading TMS files from directory
#'
#' This function read TMS data files from directory. Locality is set None.
#'
#' @param directory character
#' @param recursive logical - recursive search in subdirectories
#' @return data in standard format
#' @export
#' @examples
#' example_tms_data <- microclim::mc_feed_TMS_directory("examples/data/TMS/")
mc_feed_TMS_directory <- function(directory, recursive=TRUE) {
    mc_feed_directory(directory, "TMS", recursive = recursive)
}

#' Reading TMS files
#'
#' This function read data files of TMS type. Locality is set None.
#'
#' @param files vector of character - files with data
#' @return data in standard format
#' @export
#' @examples
#' example_tms_data <- microclim::mc_feed_TMS_files(c("examples/data/TMS/data_91184101_0.csv", "examples/data/TMS/data_94184102_0.csv"))
mc_feed_TMS_files <- function(files) {
    mc_feed_files(files, "TMS")
}

#' Reading files from directory
#'
#' This function read csv data files from directory of one logger type.
#' If csv file is not in correct format, is skipped. Locality is set None.
#'
#' @param directory character
#' @param logger_type character - type of logger (TMS)
#' @param recursive logical - recursive search in subdirectories
#' @return data in standard format
#' @export
#' @examples
#' example_tms_data <- microclim::mc_feed_directory("examples/data/TMS/", "TMS")
mc_feed_directory <- function(directory, logger_type, recursive=TRUE) {
    files <-list.files(directory, pattern=".+\\.[cC][sS][vV]", recursive=recursive, full.names=TRUE)
    mc_feed_files(files, logger_type)
}

#' Reading files
#'
#' This function read data files of one logger type. Locality is set None.
#'
#' @param files vector of character - files with data
#' @param logger_type character - type of logger (TMS)
#' @return data in standard format
#' @export
#' @examples
#' example_tms_data <- microclim::mc_feed_files(c("examples/data/TMS/data_91184101_0.csv", "examples/data/TMS/data_94184102_0.csv"), "TMS")
mc_feed_files <- function(files, logger_type) {
    files_table <- data.frame(path=files, locality_id=mc_const_NONE_LOCALITY_ID, logger=logger_type, serial_number=NA_character_)
    mc_feed_from_df(files_table)
}

#' Data files reading by CSV
#'
#' This function read raw data from loggers by table saved in CSV file
#'
#' @param csv_with_files_table data.frame
#' @return data in standard format
#' @export
#' @examples
#' example_tms_data <- microclim::mc_feed_from_csv("examples/data/TMS/files_table.csv")
mc_feed_from_csv <- function(csv_with_files_table) {
    files_table <- read.table(csv_with_files_table,
                              header = TRUE,
                              sep = ",",
                              stringsAsFactors = FALSE)
    mc_feed_from_df(files_table)
}

#' Data files reading
#'
#' This function read raw data from loggers by data.frame with files description.
#' Columns of data.frame:
#' * path - path to file
#' * locality_id
#' * logger
#' * serial_number - can be NA, than try detect
#'
#' @param files_table data.frame which describe data files
#' @return data in standard format
#' @export
mc_feed_from_df <- function(files_table) {
    files_table <- microclim:::.common_convert_factors_in_dataframe(files_table)
    if(nrow(files_table) == 0)
    {
        return(list())
    }
    result <- list()
    for(i in 1:nrow(files_table))
    {
        row <- files_table[i, ]
        result[[row$locality_id]] <- .feed_add_logger_to_locality(result[[row$locality_id]], row)
    }
    result
}

.feed_add_logger_to_locality <- function(current_locality, row) {
    if(is.null(current_locality))
    {
        current_locality <- .feed_get_new_locality(row$locality_id)
    }
    new_index <- length(current_locality$loggers) + 1
    logger <- .feed_functions_read_logger[[row$logger]](row$path, row$serial_number)
    if(is.null(logger)) {
        warning(sprintf("File %s dosn't have right format. File is skipped.", row$path))
    }
    else {
        current_locality$loggers[[new_index]] <- logger
    }
    current_locality
}

.feed_get_new_locality <- function(locality_id = NULL) {
    if (is.null(locality_id))
    {
        locality_id <- microclim::mc_const_NONE_LOCALITY_ID
    }
    metadata <- mc_LocalityMetadata(id = locality_id)
    list(metadata = metadata, loggers=list())
}

.feed_read_TMS_logger <- function(filename, serial_number=NULL) {
    .feed_read_logger(filename, microclim::mc_data_formats$TMS, "TMS", serial_number, "UTC")
}

.feed_read_logger <- function(filename, data_format, logger_type, serial_number=NULL, tz = "UTC") {
    if(is.null(serial_number) | is.na(serial_number)){
        serial_number <- microclim:::.model.get_serial_number_from_filename(data_format, filename)
    }
    if(!microclim:::.model.is_file_in_right_format(data_format, filename)) {
        return(NULL)
    }
    skip <- if(data_format@has_header) 1 else 0
    data_table <- read.table(filename,
                             sep = data_format@separator,
                             skip = skip,
                             stringsAsFactors = FALSE,
                             na.strings = data_format@na_strings)
    data_format <- microclim:::.model.load_data_format_params_from_data(data_format, data_table)
    datetime <- as.POSIXct(strptime(data_table[[data_format@date_column]], data_format@date_format, tz))
    metadata <- mc_LoggerMetadata(
                    serial_number = serial_number,
                    type = logger_type)
    list(metadata = metadata,
         datetime = datetime,
         sensors = .feed_get_sensors(data_table, data_format))
}

.feed_get_sensors <- function(data_table, data_format){
    result <- list()
    for(sensor_name in names(data_format@columns))
    {
        result[[sensor_name]] <- .feed_get_sensor(data_table, data_format, sensor_name)
    }
    result
}

.feed_get_sensor <- function(data_table, data_format, sensor_name){
    values <- data_table[[data_format@columns[[sensor_name]]]]
    metadata <- mc_SensorMetadata(sensor = sensor_name)
    item <- list(metadata = metadata,
                 values = values,
                 states = list())
    item
}

.feed_functions_read_logger <- list(
    TMS = .feed_read_TMS_logger)
