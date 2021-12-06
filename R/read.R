#' Reading files from directory
#'
#' This function read csv data files from directory of one logger type.
#' If csv file is not in correct format, is skipped. Locality is set to serial_number of logger.
#'
#' @param directory character
#' @param dataformat_name character - data format of logger (TOMST)
#' @param recursive logical - recursive search in subdirectories
#' @return data in standard format
#' @export
#' @examples
#' example_tomst_data <- microclim::mc_read_directory("examples/data/TOMST/", "TOMST")
mc_read_directory <- function(directory, dataformat_name, recursive=TRUE) {
    files <-list.files(directory, pattern=".+\\.[cC][sS][vV]$", recursive=recursive, full.names=TRUE)
    mc_read_files(files, dataformat_name)
}

#' Reading files
#'
#' This function read data files of one logger type. Locality is set to serial_number of logger.
#'
#' @param files vector of character - files with data
#' @param dataformat_name character - data format of logger (TOMST)
#' @return data in standard format
#' @export
#' @examples
#' example_tomst_data <- microclim::mc_read_files(c("examples/data/TOMST/data_91184101_0.csv", "examples/data/TOMST/data_94184102_0.csv"), "TOMST")
mc_read_files <- function(files, dataformat_name) {
    files_table <- data.frame(path=files, locality_id=NA_character_, data_format=dataformat_name, serial_number=NA_character_)
    mc_read_from_df(files_table)
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
#' example_tomst_data <- microclim::mc_read_from_csv("examples/data/TOMST/files_table.csv")
mc_read_from_csv <- function(csv_files_table, csv_localities_table=NULL) {
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
    mc_read_from_df(files_table, localities_table)
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
mc_read_from_df <- function(files_table, localities_table=NULL) {
    files_table <- microclim:::.common_convert_factors_in_dataframe(files_table)
    if(nrow(files_table) == 0)
    {
        return(list())
    }
    localities <- list()
    if(!is.null(localities_table))
    {
        localities <- .read_init_localities_from_table(localities_table)
    }
    files_table <- dplyr::filter(files_table, .read_is_data_format_ok(path, data_format))
    files_table$serial_number <- .read_get_edited_serial_numbers(files_table)
    files_table$locality_id <- .read_get_edited_locality_ids(files_table)
    .read_get_output_data(files_table, localities)
}

.read_init_localities_from_table <- function(localities_table) {
    result <- purrr::pmap(localities_table, .read_get_new_locality)
    names(result) <- localities_table$locality_id
    result
}

.read_is_data_format_ok <- function(paths, data_formats) {
    item_function <- function(path, data_format) {
        if(!(data_format %in% names(mc_data_formats))){
            warning(stringr::str_glue("It is unknown format {data_format} for {path}. File is skipped."))
            return(FALSE)
        }
        if(!microclim:::.model_is_file_in_right_format(mc_data_formats[[data_format]], path)) {
            warning(stringr::str_glue("File {path} dosn't have right format. File is skipped."))
            return(FALSE)
        }
        TRUE
    }
    purrr::map2_lgl(paths, data_formats, item_function)
}

.read_get_edited_serial_numbers <- function(files_table) {
    row_function <- function(path, locality_id, data_format, serial_number) {
        if(!is.na(serial_number)) {
            return(serial_number)
        }
        microclim:::.model_get_serial_number_from_filename(mc_data_formats[[data_format]], path)
    }

    purrr::pmap_chr(files_table, row_function)
}

.read_get_edited_locality_ids <- function(files_table) {
    row_function <- function(locality_id, serial_number) {
        if(!is.na(locality_id)) {
            return(locality_id)
        }
        serial_number
    }

    purrr::map2_chr(files_table$locality_id, files_table$serial_number, row_function)
}

.read_get_output_data <- function(files_table, localities) {
    groupped_files <- dplyr::group_by(files_table, locality_id)
    row_function <- function(path, data_format, serial_number) {
        .read_logger(path, mc_data_formats[[data_format]], serial_number)
    }
    locality_function <- function(.x, .y) {
        if(.y$locality_id %in% names(localities)) {
            locality <- localities[[.y$locality_id]]
        } else {
            locality <- .read_get_new_locality(.y$locality_id)
        }
        locality$loggers <- purrr::pmap(.x, row_function)
        locality
    }
    result <- dplyr::group_map(groupped_files, locality_function)
    names(result) <- purrr::map_chr(result, function(.x) .x$metadata@locality_id)
    result
}

.read_get_new_locality <- function(locality_id, altitude=NA_real_, lon_wgs84=NA_real_, lat_wgs84=NA_real_, tz_offset=NA_integer_) {
    tz_type <- if(is.na(tz_offset)) microclim::mc_const_TZ_UTC else microclim::mc_const_TZ_USER_DEFINED
    metadata <- mc_LocalityMetadata(locality_id=locality_id,
                                    altitude=altitude,
                                    lon_wgs84=lon_wgs84,
                                    lat_wgs84=lat_wgs84,
                                    tz_offset=tz_offset,
                                    tz_type=tz_type)
    list(metadata = metadata, loggers=list())
}

.read_logger <- function(filename, data_format, serial_number) {
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
         sensors = .read_get_sensors(data_table, data_format))
}

.read_get_sensors <- function(data_table, data_format){
    result <- list()
    for(sensor_name in names(data_format@columns))
    {
        result[[sensor_name]] <- .read_get_sensor(data_table, data_format, sensor_name)
    }
    result
}

.read_get_sensor <- function(data_table, data_format, sensor_name){
    values <- data_table[[data_format@columns[[sensor_name]]]]
    metadata <- mc_SensorMetadata(sensor_id = sensor_name)
    item <- list(metadata = metadata,
                 values = values,
                 states = list())
    item
}
