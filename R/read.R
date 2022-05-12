.read_const_MESSAGE_COMBINE_FILES_AND_DIRECTORIES <- "It isn't possible combine files and directories"
.read_const_MESSAGE_SOURCE_EMPTY_SOURCE_DATA_TABLE <- "Source data table is empty."
.read_const_MESSAGE_DATETIME_TYPE <- "Datetime must be in POSIXct format and UTC timezone."
.read_const_MESSAGE_ANY_FILE <- "There isn't any source file."
.read_const_MESSAGE_WRONG_DATETIME <- "It isn't possible read datetimes from {filename}. File is skipped."
.read_const_MESSAGE_ANY_LOCALITY <- "There isn't any valid locality."

#' Reading files or directories
#'
#' This function read one or more csv files or directories of identical, 
#' pre-defined logger type (format) see [mc_DataFormat] and [mc_data_formats]. 
#' This function does not allow you to provide additional locality or sensor metadata. 
#' Metadata can be load through [myClim::mc_read_data()] or 
#' can be provided later with function [myClim::mc_prep_meta()]
#' 
#' @details 
#' If file is not in expected format, then file is skipped and warning printed in console. 
#' CSV files (loggers raw data) are in resulting myClim object placed to separate localities with empty metadata.    
#' Localities are named after serial_number of logger.
#'
#' @seealso [myClim::mc_DataFormat]
#'
#' @param paths vector of paths to files or directories
#' @param dataformat_name character - data format of logger one of (TOMST, TOMST_join) see `names(mc_data_formats)`
#' @param recursive logical - recursive search in sub-directories (default TRUE)
#' @param date_format format of date in your hobo files e.g. "%d.%m.%y %H:%M:%S" (default NA). Required for HOBO files. 
#' For TMS files ignored, there is fix date format. see [mc_data_formats]
#' @param tz_offset - timezone offset in minutes; It is required fill only for non-UTC data (custom settings in HOBO). Not used in TMS (default NA)
#' @return myClim object in Prep-format see [myClim-package]
#' @export
#' @examples
#' \dontrun{
#' tomst_data <- mc_read_files(c("examples/data/TOMST/data_91184101_0.csv", "examples/data/TOMST/data_94184102_0.csv"), "TOMST")
#' }
mc_read_files <- function(paths, dataformat_name, recursive=TRUE, date_format=NA_character_, tz_offset=NA_integer_) {
    if(all(dir.exists(paths))) {
        files <- .read_get_csv_files_from_directory(paths, recursive)
    } else if(any(dir.exists(paths))) {
        stop(.read_const_MESSAGE_COMBINE_FILES_AND_DIRECTORIES)
    } else {
        files <- paths
    }
    files_table <- data.frame(path=files, locality_id=NA_character_, data_format=dataformat_name,
                              serial_number=NA_character_, date_format=date_format, tz_offset=tz_offset)
    mc_read_data(files_table)
}

.read_get_csv_files_from_directory <- function(paths, recursive) {
    files_function <- function (directory) {
        list.files(directory, pattern=".+\\.(csv|CSV|txt|TXT)$", recursive=recursive, full.names=TRUE)
    }
    purrr::flatten_chr(purrr::map(paths, files_function))
}

#' Reading files with locality metadata
#'
#' This function has two tables parameters. i.) `files_table` with paths pointing to raw
#' csv logger files, specification of data format (logger type) and locality name. This table is required.
#' ii) `localities_table` with locality id and metadata e.g. longitude, latitude, altitude...
#' 
#' @details 
#' The input tables could be R data.frames or csv files. When loading `files_table` and `localities_table` from external CSV it 
#' must have header, column separator must be comma ",".
#' @seealso [myClim::mc_DataFormat]
#' @param files_table path to csv file or data.frame object with 3 required columns and few optional:
#' required columns:
#' * path - path to file
#' * locality_id
#' * data_format see [mc_data_formats], `names(mc_data_formats)`
#'
#' optional columns:
#' * serial_number - can be NA, than try detect
#' * date_format - for reading HOBO format of date in strptime function (e.g. "%d.%m.%y %H:%M:%S"); Ignored for TOMST data format
#' * tz_offset - If source datetimes aren't in UTC, then is possible define offset from UTC in minutes.
#' Value in this column have the highest priority. If NA then auto detection of timezone in files. If timezone can't be detected, then UTC is supposed.
#' Timezone offset in HOBO format can be defined in header. In this case function try detect offset automatically. Ignored for TOMST data format
#'
#' @param localities_table path to csv file or data.frame. Localities table is optional (default NULL).
#' object contains 5 columns:
#' * locality_id
#' * altitude
#' * lon_wgs84
#' * lat_wgs84
#' * tz_offset
#' @return myClim object in Prep-format see [myClim-package]
#' @export
#' @examples
#' \dontrun{
#' tomst_data <- mc_read_data("examples/data/TOMST/files_table.csv", "examples/data/TOMST/localities_table.csv")
#' }
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

    data_formats <- .read_get_data_formats(files_table)
    condition <- purrr::map_lgl(data_formats, ~ !is.null(.x))
    files_table <- files_table[condition, ]
    data_formats <- data_formats[condition]
    if(nrow(files_table) == 0) {
        stop(.read_const_MESSAGE_ANY_FILE)
    }
    files_table$serial_number <- .read_get_edited_serial_numbers(files_table, data_formats)
    files_table$locality_id <- .read_get_edited_locality_ids(files_table)
    .read_get_output_data(files_table, localities, data_formats)
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

.read_get_data_formats <- function(files_table) {
    file_function <- function (path, data_format, date_format, tz_offset) {
        if(!(data_format %in% names(mc_data_formats))){
            warning(stringr::str_glue("It is unknown format {data_format} for {path}. File is skipped."))
            return(NULL)
        }
        data_format_object <- mc_data_formats[[data_format]]
        if(is.na(data_format_object@date_format) && !is.na(date_format)) {
            data_format_object@date_format <- date_format
        }
        if(!is.na(tz_offset)) {
            data_format_object@tz_offset <- tz_offset
        }
        data_format_object <- myClim:::.model_load_data_format_params_from_file(data_format_object, path)
        if(is.null(data_format_object)) {
            warning(stringr::str_glue("File {path} dosn't have right format. File is skipped."))
            return(NULL)
        }
        return(data_format_object)
    }

    date_format <- NA_character_
    if("date_format" %in% colnames(files_table)) {
        date_format <- files_table$date_format
    }
    tz_offset <- NA_integer_
    if("tz_offset" %in% colnames(files_table)) {
        tz_offset <- files_table$tz_offset
    }
    purrr::pmap(list(path=files_table$path,
                     data_format=files_table$data_format,
                     date_format=date_format,
                     tz_offset=tz_offset), file_function)
}

.read_get_edited_serial_numbers <- function(files_table, data_formats) {
    row_function <- function(path, locality_id, data_format, serial_number) {
        if(!is.na(serial_number)) {
            return(serial_number)
        }
        serial_number <- myClim:::.model_get_serial_number_from_file(data_format, path)
        if(is.na(serial_number))
        {
            stop(stringr::str_glue("It isn't possible detect serial_number for {path}."))
        }
        serial_number
    }

    serial_numbers <- NA_character_
    if("serial_number" %in% colnames(files_table)) {
        serial_numbers <- files_table$serial_number
    }

    parameters <- list(path = files_table$path,
                       locality_id = files_table$locality_id,
                       data_format = data_formats,
                       serial_number = serial_numbers)

    purrr::pmap_chr(parameters, row_function)
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

.read_get_output_data <- function(files_table, localities, data_formats) {
    files_table$index <- 1:nrow(files_table)
    groupped_files <- dplyr::group_by(files_table, locality_id)
    row_function <- function(path, data_format, serial_number) {
        .read_logger(path, data_format, serial_number)
    }
    locality_function <- function(.x, .y) {
        if(.y$locality_id %in% names(localities)) {
            locality <- localities[[.y$locality_id]]
        } else {
            locality <- .read_get_new_locality(.y$locality_id)
        }
        parameters <- list(path = .x$path,
                           data_format = data_formats[.x$index],
                           serial_number = .x$serial_number)
        locality$loggers <- purrr::pmap(parameters, row_function)
        locality$loggers <- purrr::discard(locality$loggers, ~ is.null(.x))
        locality
    }
    result <- dplyr::group_map(groupped_files, locality_function)
    result <- purrr::discard(result, ~ length(.x$loggers) == 0)
    if(length(result) == 0) {
        stop(.read_const_MESSAGE_ANY_LOCALITY)
    }
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
    data_table <- .read_get_data_from_file(filename, data_format)
    data_table <- .read_fix_decimal_separator_if_need(filename, data_format, data_table)
    datetime <- as.POSIXct(strptime(data_table[[data_format@date_column]], data_format@date_format, "UTC"))
    if(any(is.na(datetime))) {
        warning(stringr::str_glue(.read_const_MESSAGE_WRONG_DATETIME))
        return(NULL)
    }
    if(data_format@tz_offset != 0) {
        datetime <- datetime - data_format@tz_offset * 60
    }
    sensors <- .read_get_sensors_from_data_format(data_table, data_format)
    .read_get_new_logger(datetime, sensors, serial_number, data_format@logger_type)
}

.read_get_data_from_file <- function(filename, data_format, nrows=-1) {
    read.table(filename,
               sep = data_format@separator,
               skip = data_format@skip,
               stringsAsFactors = FALSE,
               na.strings = data_format@na_strings,
               fill = TRUE,
               nrows = nrows,
               comment.char = "",
               encoding = "UTF-8")
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

.read_get_new_logger <- function(datetime, sensors, serial_number=NA_character_, logger_type=NA_character_) {
    metadata <- new("mc_LoggerMetadata")
    metadata@serial_number <- serial_number
    metadata@type <- logger_type
    list(metadata = metadata,
         clean_info = new("mc_LoggerCleanInfo"),
         datetime = datetime,
         sensors = sensors)
}

.read_get_sensors_from_data_format <- function(data_table, data_format){
    result <- list()
    for(sensor_name in names(data_format@columns))
    {
        result[[sensor_name]] <- .read_get_sensor_from_data_format(data_table, data_format, sensor_name)
    }
    result
}

.read_get_sensor_from_data_format <- function(data_table, data_format, sensor_name){
    values <- data_table[[data_format@columns[[sensor_name]]]]
    myClim:::.common_get_new_sensor(sensor_name, sensor_name, values)
}

#' Reading data from wide data.frame
#'
#' This is universal function designed to read time series and values 
#' from wide data.frame to myClim object.
#' 
#' 
#' @details The first column of input data.frame must be datetime column in POSIXct time format UTC timezone.
#' Following columns represents localities. Column names are the localities. 
#' All values in wide data.frame represents the same sensor, e.g. air temperature. If you wish to 
#' read multiple sensors use [myClim::mc_read_long] or use [myClim::mc_read_wide] multiple times separately
#' for each sensor and that merge myClim objects with [myClim::mc_prep_merge]
#'
#' @param data_table data.frame with first column of POSIXct time format UTC timezone, 
#' followed by columns with microclimatic records. See details.  
#'
#' Columns:
#' * datetime column - POSIXct and UTC timezone is required
#' * Name of locality[1] - values
#' * ...
#' * Name of locality[n] - values
#' @param sensor_id define the sensor type, one of `names(mc_data_sensors)` (default `real`)
#' @param sensor_name custom name of sensor; if NULL (default) than `sensor_name == sensor_id`
#' @return myClim object in Prep-format
#' @export
#' @seealso [myClim::mc_read_long]
mc_read_wide <- function(data_table, sensor_id=myClim:::.model_const_SENSOR_real, sensor_name=NULL) {
    if(ncol(data_table) <= 1) {
       stop(.read_const_MESSAGE_SOURCE_EMPTY_SOURCE_DATA_TABLE)
    }
    .read_check_datetime(data_table[[1]])
    if(is.null(sensor_name)) {
        sensor_name <- sensor_id
    }
    result <- purrr::map(colnames(data_table)[-1], .read_get_new_locality)
    names(result) <- purrr::map_chr(result, ~ .x$metadata@locality_id)
    locality_function <- function(locality) {
        sensors <- list()
        sensors[[sensor_name]] <- myClim:::.common_get_new_sensor(sensor_id, sensor_name, data_table[[locality$metadata@locality_id]])
        locality$loggers[[1]] <- .read_get_new_logger(data_table[[1]], sensors)
        locality
    }
    purrr::map(result, locality_function)
}

.read_check_datetime <- function(datetime) {
    if(!lubridate::is.POSIXct(datetime) || attr(datetime,"tzone") != "UTC"){
        stop(.read_const_MESSAGE_DATETIME_TYPE)
    }
}

#' Reading data from long data.frame
#'
#' This is universal function designed to read time series and values 
#' from long data.frame to myClim object. 
#'
#' @details Similar like [myClim::mc_read_wide] but is capable to read multiple sensors. 
#'
#' @param data_table long data.frame with Columns:
#' * locality_id
#' * sensor_name  - see `names(mc_data_sensors)`
#' * datetime - POSIXct and UTC timezone is required
#' * value
#' @param sensor_ids list with relations between sensor_names and sensor_ids (default list());
#' sensor_id is key from `names(mc_data_sensors)`. If sensor_name is the same as sensor_id,
#' it does not have to be in the list.
#'
#' `sensor_ids <- list(sensor_name1=sensor_id1, sensor_name2=sensor_id2)`
#' @return myClim object in Prep-format
#' @export
#' @seealso [myClim::mc_read_wide]
mc_read_long <- function(data_table, sensor_ids=list()) {
    .read_check_datetime(data_table$datetime)

    data_table <- dplyr::group_by(data_table, locality_id)
    localities <- dplyr::group_map(data_table, .read_long_locality, sensor_ids=sensor_ids)
    names(localities) <- purrr::map_chr(localities, ~ .x$metadata@locality_id)
    localities
}

.read_long_locality <- function(locality_table, locality_id, sensor_ids) {
    locality_id <- locality_id$locality_id[[1]]
    sensor_names <- unique(locality_table$sensor_name)
    sensor_table_function <- function(name) {
        data <- dplyr::filter(locality_table, sensor_name == name)
        result <- dplyr::select(data, datetime, value)
        names(result)[2] <- name
        result
    }

    datetime <- sort(unique(locality_table$datetime))
    tables <- c(list(tibble::tibble(datetime=datetime)), purrr::map(sensor_names, sensor_table_function))
    table_values <- purrr::reduce(tables, function(.x, .y) dplyr::left_join(.x, .y, by="datetime"))
    result <- .read_get_new_locality(locality_id)

    sensor_function <- function(sensor_name) {
        sensor_id <- sensor_name
        if(sensor_name %in% names(sensor_ids)) {
            sensor_id <- sensor_ids[[sensor_name]]
        }
        myClim:::.common_get_new_sensor(sensor_id, sensor_name, table_values[[sensor_name]])
    }

    sensors <- purrr::map(sensor_names, sensor_function)
    names(sensors) <- purrr::map_chr(sensors, ~ .x$metadata@name)
    result$loggers[[1]] <- .read_get_new_logger(table_values$datetime, sensors)
    result
}
