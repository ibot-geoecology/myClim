#' Reading TOMST files from directory
#'
#' This function read TOMST data files from directory. Locality is set None.
#'
#' @param directory character
#' @param recursive logical - recursive search in subdirectories
#' @return data in standard format
#' @export
#' @examples
#' example_tomst_data <- microclim::mc_feed_TOMST_directory("examples/data/TOMST/")
mc_feed_TOMST_directory <- function(directory, recursive=TRUE) {
    mc_feed_directory(directory, "TOMST", recursive = recursive)
}

#' Reading TOMST files
#'
#' This function read data files of TOMST type. Locality is set None.
#'
#' @param files vector of character - files with data
#' @return data in standard format
#' @export
#' @examples
#' example_tomst_data <- microclim::mc_feed_TOMST_files(c("examples/data/TOMST/data_91184101_0.csv", "examples/data/TOMST/data_94184102_0.csv"))
mc_feed_TOMST_files <- function(files) {
    mc_feed_files(files, "TOMST")
}

#' Reading files from directory
#'
#' This function read csv data files from directory of one logger type.
#' If csv file is not in correct format, is skipped. Locality is set None.
#'
#' @param directory character
#' @param dataformat_name character - data format of logger (TOMST)
#' @param recursive logical - recursive search in subdirectories
#' @return data in standard format
#' @export
#' @examples
#' example_tomst_data <- microclim::mc_feed_directory("examples/data/TOMST/", "TOMST")
mc_feed_directory <- function(directory, dataformat_name, recursive=TRUE) {
    files <-list.files(directory, pattern=".+\\.[cC][sS][vV]$", recursive=recursive, full.names=TRUE)
    mc_feed_files(files, dataformat_name)
}

#' Reading files
#'
#' This function read data files of one logger type. Locality is set None.
#'
#' @param files vector of character - files with data
#' @param dataformat_name character - data format of logger (TOMST)
#' @return data in standard format
#' @export
#' @examples
#' example_tomst_data <- microclim::mc_feed_files(c("examples/data/TOMST/data_91184101_0.csv", "examples/data/TOMST/data_94184102_0.csv"), "TOMST")
mc_feed_files <- function(files, dataformat_name) {
    files_table <- data.frame(path=files, locality_id=mc_const_NONE_LOCALITY_ID, data_format=dataformat_name, serial_number=NA_character_)
    mc_feed_from_df(files_table)
}

#' Data files reading by CSV
#'
#' This function read raw data from loggers by table saved in CSV file
#'
#' @param csv_files_table data.frame
#' @param csv_localities_table data.frame
#' @return data in standard format
#' @export
#' @examples
#' example_tomst_data <- microclim::mc_feed_from_csv("examples/data/TOMST/files_table.csv")
mc_feed_from_csv <- function(csv_files_table, csv_localities_table=NULL) {
    files_table <- read.table(csv_files_table,
                              header = TRUE,
                              sep = ",",
                              stringsAsFactors = FALSE)
    localities_table <- NULL
    if(!is.null(csv_localities_table)){
        localities_table <- read.table(csv_localities_table,
                                       header = TRUE,
                                       sep = ",",
                                       stringsAsFactors = FALSE)
    }
    mc_feed_from_df(files_table, localities_table)
}

#' Data files reading
#'
#' This function read raw data from loggers by data.frame with files description.
#'
#' @param files_table data.frame which describe data files
#' Columns:
#' * path - path to file
#' * locality_id
#' * data_format
#' * serial_number - can be NA, than try detect
#' @param localities_table data.frame which describe localities
#' Columns:
#' * locality_id
#' * altitude
#' * lon_wgs84
#' * lat_wgs84
#' * tz_offset
#' @return data in standard format
#' @export
mc_feed_from_df <- function(files_table, localities_table=NULL) {
    files_table <- microclim:::.common_convert_factors_in_dataframe(files_table)
    if(nrow(files_table) == 0)
    {
        return(list())
    }
    result <- list()
    if(!is.null(localities_table))
    {
        result <- .feed_init_localities_from_table(localities_table)
    }
    for(i in 1:nrow(files_table))
    {
        row <- files_table[i, ]
        result[[row$locality_id]] <- .feed_add_logger_to_locality(result[[row$locality_id]], row)
    }
    result
}

.feed_init_localities_from_table <- function(localities_table) {
    result <- purrr::pmap(localities_table, .feed_get_new_locality)
    names(result) <- localities_table$locality_id
    result
}

.feed_add_logger_to_locality <- function(current_locality, row) {
    if(is.null(current_locality))
    {
        current_locality <- .feed_get_new_locality(row$locality_id)
    }
    new_index <- length(current_locality$loggers) + 1
    logger <- .feed_read_logger(row$path, mc_data_formats[[row$data_format]], row$serial_number)
    if(is.null(logger)) {
        warning(sprintf("File %s dosn't have right format. File is skipped.", row$path))
    }
    else {
        current_locality$loggers[[new_index]] <- logger
    }
    current_locality
}

.feed_get_new_locality <- function(locality_id=NULL, altitude=NA_real_, lon_wgs84=NA_real_, lat_wgs84=NA_real_, tz_offset=NA_integer_) {
    if (is.null(locality_id))
    {
        locality_id <- microclim::mc_const_NONE_LOCALITY_ID
    }
    metadata <- mc_LocalityMetadata(locality_id=locality_id,
                                    altitude=altitude,
                                    lon_wgs84=lon_wgs84,
                                    lat_wgs84=lat_wgs84,
                                    tz_offset=tz_offset)
    list(metadata = metadata, loggers=list())
}

.feed_read_logger <- function(filename, data_format, serial_number=NULL) {
    if(is.null(serial_number) | is.na(serial_number)){
        serial_number <- microclim:::.model_get_serial_number_from_filename(data_format, filename)
    }
    if(!microclim:::.model_is_file_in_right_format(data_format, filename)) {
        return(NULL)
    }
    skip <- if(data_format@has_header) 1 else 0
    data_table <- read.table(filename,
                             sep = data_format@separator,
                             skip = skip,
                             stringsAsFactors = FALSE,
                             na.strings = data_format@na_strings)
    data_format <- microclim:::.model_load_data_format_params_from_data(data_format, data_table)
    datetime <- as.POSIXct(strptime(data_table[[data_format@date_column]], data_format@date_format, "UTC"))
    metadata <- mc_LoggerMetadata(
                    serial_number = serial_number,
                    type = data_format@logger_type)
    list(metadata = metadata,
         clean_log = list(),
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
    metadata <- mc_SensorMetadata(sensor_id = sensor_name)
    item <- list(metadata = metadata,
                 values = values,
                 states = list())
    item
}
