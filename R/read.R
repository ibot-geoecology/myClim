.read_const_MESSAGE_COMBINE_FILES_AND_DIRECTORIES <- "It isn't possible combine files and directories"

#' Reading files
#'
#' This function read data files of one logger type. If file is not in correct format, is skipped.
#' Locality is set to serial_number of logger.
#'
#' @param paths vector of paths to files or directories
#'
#' If paths are directories, then function search logger files.
#' It isn't possible combine files and directories.
#' @param dataformat_name character - data format of logger (TOMST, TOMST_join)
#' @param recursive logical - recursive search in subdirectories (default TRUE)
#' @return data in standard format
#' @export
#' @examples
#' example_tomst_data <- myClim::mc_read_files(c("examples/data/TOMST/data_91184101_0.csv", "examples/data/TOMST/data_94184102_0.csv"), "TOMST")
mc_read_files <- function(paths, dataformat_name, recursive=TRUE) {
    if(all(dir.exists(paths))) {
        files <- .read_get_csv_files_from_directory(paths, recursive)
    } else if(any(dir.exists(paths))) {
        stop(.read_const_MESSAGE_COMBINE_FILES_AND_DIRECTORIES)
    } else {
        files <- paths
    }
    files_table <- data.frame(path=files, locality_id=NA_character_, data_format=dataformat_name, serial_number=NA_character_)
    mc_read_data(files_table)
}

.read_get_csv_files_from_directory <- function(paths, recursive) {
    files_function <- function (directory) {
        list.files(directory, pattern=".+\\.[cC][sS][vV]$", recursive=recursive, full.names=TRUE)
    }
    purrr::flatten_chr(purrr::map(paths, files_function))
}

#' Data files reading
#'
#' This function read raw data from loggers by data.frame or csv file with files description.
#'
#' @param files_table csv file path or data.frame which describe data files
#'
#' Csv file required header row. Column separator is ",".
#'
#' Columns:
#' * path - path to file
#' * locality_id
#' * data_format
#' * serial_number - can be NA, than try detect
#' @param localities_table csv file path or data.frame which describe localities
#'
#' Csv file required header row. Column separator is ",".
#'
#' Columns:
#' * locality_id
#' * altitude
#' * lon_wgs84
#' * lat_wgs84
#' * tz_offset
#' @return data in standard format
#' @export
mc_read_data <- function(files_table, localities_table=NULL) {
    if(is.character(files_table)) {
        files_table <- .read_get_table_from_csv(files_table)
    }
    files_table <- myClim:::.common_convert_factors_in_dataframe(files_table)
    if(nrow(files_table) == 0)
    {
        return(list())
    }
    localities <- list()
    if(!is.null(localities_table))
    {
        if(is.character(localities_table)) {
            localities_table <- .read_get_table_from_csv(localities_table)
        }
        localities <- .read_init_localities_from_table(localities_table)
    }
    files_table <- dplyr::filter(files_table, .read_is_data_format_ok(path, data_format))
    files_table$serial_number <- .read_get_edited_serial_numbers(files_table)
    files_table$locality_id <- .read_get_edited_locality_ids(files_table)
    .read_get_output_data(files_table, localities)
}

.read_get_table_from_csv <- function(csv_path) {
    files_table <- read.table(csv_path,
                              header = TRUE,
                              sep = ",",
                              stringsAsFactors = FALSE)
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
        if(!myClim:::.model_is_file_in_right_format(mc_data_formats[[data_format]], path)) {
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
        serial_number <- myClim:::.model_get_serial_number_from_filename(mc_data_formats[[data_format]], path)
        if(is.na(serial_number))
        {
            stop(stringr::str_glue("It isn't possible detect serial_number for {path}."))
        }
        serial_number
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
    tz_type <- if(is.na(tz_offset)) myClim::mc_const_TZ_UTC else myClim::mc_const_TZ_USER_DEFINED
    metadata <- new("mc_LocalityMetadata")
    metadata@locality_id <- locality_id
    metadata@altitude <- altitude
    metadata@lon_wgs84 <- lon_wgs84
    metadata@lat_wgs84 <- lat_wgs84
    metadata@tz_offset <- tz_offset
    metadata@tz_type <- tz_type
    list(metadata = metadata, loggers=list())
}

.read_logger <- function(filename, data_format, serial_number) {
    skip <- if(data_format@has_header) 1 else 0
    data_table <- read.table(filename,
                             sep = data_format@separator,
                             skip = skip,
                             stringsAsFactors = FALSE,
                             na.strings = data_format@na_strings)
    data_format <- myClim:::.model_load_data_format_params_from_data(data_format, data_table)
    data_table <- .read_fix_decimal_separator_if_need(filename, data_format, data_table)
    datetime <- as.POSIXct(strptime(data_table[[data_format@date_column]], data_format@date_format, "UTC"))
    if(any(is.na(datetime))) {
        stop(stringr::str_glue("It isn't possible read datetimes from {filename}."))
    }
    metadata <- new("mc_LoggerMetadata")
    metadata@serial_number <- serial_number
    metadata@type <- data_format@logger_type
    list(metadata = metadata,
         clean_info = new("mc_LoggerCleanInfo"),
         datetime = datetime,
         sensors = .read_get_sensors(data_table, data_format))
}

.read_fix_decimal_separator_if_need <- function(filename, data_format, data_table) {
    values_function <- function(column_index) {
        if(!(column_index %in% data_format@columns) || is.numeric(data_table[[column_index]]) ||
             all(is.na(data_table[[column_index]]))) {
            return(data_table[[column_index]])
        }
        if(is.character(data_table[[column_index]])) {
            result <- sub(",", ".", data_table[[column_index]])
            return(as.numeric(result))
        }
        stop(stringr::str_glue("It isn't possible load sensor data from {column_index}. column in file {filename}."))
    }
    as.data.frame(purrr::map(seq(ncol(data_table)), values_function))
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
    myClim:::.common_get_new_sensor(sensor_name, sensor_name, values)
}