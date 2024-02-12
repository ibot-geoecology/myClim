.read_const_MESSAGE_COMBINE_FILES_AND_DIRECTORIES <- "It isn't possible to combine files and directories"
.read_const_MESSAGE_SOURCE_EMPTY_SOURCE_DATA_TABLE <- "Source data table is empty."
.read_const_MESSAGE_DATETIME_TYPE <- "Datetime must be in POSIXct format UTC timezone."
.read_const_MESSAGE_ANY_FILE <- "There aren't any source file."
.read_const_MESSAGE_WRONG_DATETIME <- "It isn't possible to read datetimes from {filename}. File is skipped."
.read_const_MESSAGE_ANY_LOCALITY <- "There aren't any valid localities."
.read_const_MESSAGE_TUBEDB_PLOT_REGION_NULL <- "Plot or region must be set."
.read_const_MESSAGE_UNSUPPOERTED_FORMAT <- "{data_format} is not a supported data format. File is skipped."
.read_const_MESSAGE_UNAPLICABLE_FORMAT <- "{data_format} is not applicable format to {path}. File is skipped."
.read_const_MESSAGE_USER_DATA_FORMAT_KEY <- "The key in user_data_format must not be the same as the key in mc_data_formats."
.read_const_MESSAGE_VROOM_WARNING <- "Parsing issues in file {filename}"
.read_const_MESSAGE_FILE_SKIP <- "File {.x} does not exist - skipping."

.read_state <- new.env()

#' Reading files or directories
#'
#' This function read one or more CSV/TXT files or directories of identical, 
#' pre-defined logger type (format) see [mc_DataFormat] and [mc_data_formats]. 
#' This function does not support loading locality or sensor metadata while reading.
#' Metadata can be loaded through [myClim::mc_read_data()] or
#' can be provided later with function [myClim::mc_prep_meta_locality()]
#' 
#' @details 
#' If file is not in expected format, then file is skipped and warning printed in console. 
#' CSV/TXT files (loggers raw data) are in resulting myClim object placed to separate
#' localities with empty metadata. Localities are named after serial_number of logger.
#' Pre-defined logger types are ("Dendro","HOBO","Thermo","TMS","TMS_L45")
#' By default data are cleaned with function [myClim::mc_prep_clean()]. 
#' See function description. It detects holes in time-series, 
#' duplicated records or records in wrong order.
#'
#' @seealso [myClim::mc_DataFormat], [myClim::mc_prep_clean()]
#'
#' @param paths vector of paths to files or directories
#' @param dataformat_name data format of logger; one of `names(mc_data_formats)`
#' @param recursive recursive search in sub-directories (default TRUE)
#' @param date_format format of date in your hobo files e.g. "%d.%m.%y %H:%M:%S" (default NA).
#' Required for HOBO files. For TMS files ignored, there is known, stable date format. see [mc_data_formats]
#' @param logger_type type of logger (default NA), can be one of
#' pre-defined see [myClim::mc_read_data()] or any custom string
#' @param tz_offset timezone offset in minutes; It is required only for non-UTC data
#' (custom settings in HOBO). Not used in TMS (default NA)
#' @param step time step of microclimatic time-series in seconds. When provided, then is used in
#' [mc_prep_clean] instead of automatic step detection.
#' If not provided (NA), is automatically detected in [mc_prep_clean]. (default NA)
#' @template params_read_common
#' @return myClim object in Raw-format see [myClim-package]
#' @export
#' @examples
#' files <- c(system.file("extdata", "data_91184101_0.csv", package = "myClim"),
#'            system.file("extdata", "data_94184102_0.csv", package = "myClim"))
#' tomst_data <- mc_read_files(files, "TOMST")
#' # user_data_formats
#' files <- system.file("extdata", "TMS94184102.csv", package = "myClim")
#' user_data_formats <- list(my_logger=new("mc_DataFormat"))
#' user_data_formats$my_logger@date_column <- 2
#' user_data_formats$my_logger@date_format <- "%Y-%m-%d %H:%M:%S"
#' user_data_formats$my_logger@tz_offset <- 0
#' user_data_formats$my_logger@columns[[mc_const_SENSOR_T_C]] <- c(3, 4, 5)
#' user_data_formats$my_logger@columns[[mc_const_SENSOR_real]] <- 6
#' my_data <- mc_read_files(files, "my_logger", silent=TRUE, user_data_formats=user_data_formats)
mc_read_files <- function(paths, dataformat_name, logger_type=NA_character_, recursive=TRUE, date_format=NA_character_,
                          tz_offset=NA_integer_, step=NA_integer_, clean=TRUE, silent=FALSE, user_data_formats=NULL) {
    if(all(dir.exists(paths))) {
        files <- .read_get_csv_files_from_directory(paths, recursive)
    } else if(any(dir.exists(paths))) {
        stop(.read_const_MESSAGE_COMBINE_FILES_AND_DIRECTORIES)
    } else {
        files <- paths
    }
    files_table <- data.frame(path=files, locality_id=NA_character_, data_format=dataformat_name,
                              serial_number=NA_character_, logger_type=logger_type,
                              tz_offset=tz_offset, step=step)
    files_table$date_format <- rep(list(date_format), nrow(files_table))
    mc_read_data(files_table, clean=clean, silent=silent, user_data_formats=user_data_formats)
}

.read_get_csv_files_from_directory <- function(paths, recursive) {
    files_function <- function (directory) {
        list.files(directory, pattern=".+\\.(csv|CSV|txt|TXT)$", recursive=recursive, full.names=TRUE)
    }
    purrr::flatten_chr(purrr::map(paths, files_function))
}

#' Reading files with locality metadata
#' @description
#' This function has two tables as the parameters.
#'
#' (i) `files_table` with *paths* pointing to raw
#' csv logger files, specification of *data format* (logger type) and *locality name*.
#'
#' (ii) `localities_table` with *locality id* and metadata e.g. longitude, latitude, elevation...
#' 
#' @details 
#' The input tables could be R data.frames or csv files. When loading `files_table`
#' and `localities_table` from external CSV they must have header, column separator must be comma ",".
#' By default data are cleaned with function [myClim::mc_prep_clean()]. See function description. It detects
#' holes in time-series, duplicated records or records in wrong order.
#' @seealso [myClim::mc_DataFormat]
#' @param files_table path to csv file or data.frame object see example](https://github.com/ibot-geoecology/myClim/blob/main/examples/data/TOMST/files_table.csv) 
#' with 3 required columns and few optional:
#' required columns:
#' * path - path to files
#' * locality_id - unique locality id
#' * data_format see [mc_data_formats], `names(mc_data_formats)`
#'
#' optional columns:
#' * serial_number - logger serial number. If is NA, than myClim tries to detect serial number from file name (for TOMST) or header (for HOBO)
#' * logger_type - type of logger. This defines individual sensors attributes (measurement heights and physical units) of the logger. Important when combining the data from multiple loggers on the locality.
#' If not provided, myClim tries to detect loger_type from the source data file structure (applicable for HOBO, Dendro, Thermo and TMS), but automatic detection of TMS_L45 is not possible.
#' Pre-defined logger types are: ("Dendro", "HOBO", "Thermo", "TMS", "TMS_L45")
#' Default heights of sensor based on logger types are defined in table [mc_data_heights]
#' * date_format A character vector specifying the custom date format(s) for the [lubridate::parse_date_time()] function
#' (e.g., "%d.%m.%Y %H:%M:%S"). Multiple formats can be defined. The first matching format will be selected for parsing.
#' * tz_offset - If source datetimes aren't in UTC, then is possible define offset from UTC in minutes.
#' Value in this column have the highest priority. If NA then auto detection of timezone in files.
#' If timezone can't be detected, then UTC is supposed.
#' Timezone offset in HOBO format can be defined in header. In this case function try detect offset automatically.
#' Ignored for Tomst TMS data format (they are always in UTC)
#' * step - Time step of microclimatic time-series in seconds. When provided, then used in [mc_prep_clean]
#' instead of automatic step detection.
#'
#' @param localities_table path to csv file ("c:/user/localities.table.csv") or R data.frame [
#' see example](https://github.com/ibot-geoecology/myClim/blob/main/examples/data/TOMST/localities_table.csv). 
#' Localities table is optional (default NULL).
#' The `locality_id` is the only required column. Other columns are optional. Column names corresponding 
#' with the myclim pre-defined locality metadata (elevation, lon_wgs84, lat_wgs84, tz_offset) 
#' are associted withthose pre-defined metadata slots, other columns are  written into 
#' `metadata@user_data` [myClim-package].
#' 
#'required columns:
#' * locality_id - unique locality id
#' 
#' optional columns:
#' * elevation - elevation (in m)
#' * lon_wgs84 - longitude (in decimal degrees)
#' * lat_wgs84 - latitude (in decimal degrees)
#' * tz_offset - locality time zone offset from UTC, applicable for converting time-series from UTC to local time.
#' * ... - any other columns are imported to `metadata@user_data`
#' @template params_read_common
#' @return myClim object in Raw-format see [myClim-package]
#' @export
#' @examples
#' files_csv <- system.file("extdata", "files_table.csv", package = "myClim")
#' localities_csv <- system.file("extdata", "localities_table.csv", package = "myClim")
#' tomst_data <- mc_read_data(files_csv, localities_csv)
mc_read_data <- function(files_table, localities_table=NULL, clean=TRUE, silent=FALSE, user_data_formats=NULL) {
    if(is.character(files_table)) {
        source_csv_file <- files_table
        files_table <- .read_get_table_from_csv(files_table)
        files_table <- .read_edit_data_file_paths(files_table, source_csv_file)
    }
    files_table <- .common_convert_factors_in_dataframe(files_table)
    files_table <- .read_check_data_file_paths(files_table)
    .read_state$check_bar <- NULL
    .read_state$read_bar <- NULL
    if(!silent) {
        .read_state$check_bar <- progress::progress_bar$new(format = "check [:bar] :current/:total files",
                                                              total=nrow(files_table))
    }
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
        localities_table <- .common_convert_factors_in_dataframe(localities_table)
        localities <- .read_init_localities_from_table(localities_table)
    }
    .read_check_data_formats(user_data_formats)
    data_formats <- .read_get_data_formats(files_table, user_data_formats)
    condition <- purrr::map_lgl(data_formats, ~ !is.null(.x))
    files_table <- files_table[condition, ]
    data_formats <- data_formats[condition]
    if(nrow(files_table) == 0) {
        stop(.read_const_MESSAGE_ANY_FILE)
    }
    if(!silent) {
        .read_state$read_bar <- progress::progress_bar$new(format = "read [:bar] :current/:total files",
                                                           total=nrow(files_table))
    }
    files_table$serial_number <- .read_get_edited_serial_numbers(files_table, data_formats)
    files_table$locality_id <- .read_get_edited_locality_ids(files_table)
    result <- .read_get_output_data(files_table, localities, data_formats)
    if(clean) {
        result <- mc_prep_clean(result, silent=silent)
    }
    result
}

.read_get_table_from_csv <- function(csv_path) {
    files_table <- read.table(csv_path,
                              header = TRUE,
                              sep = ",",
                              stringsAsFactors = FALSE)
}

.read_edit_data_file_paths <- function(files_table, source_file) {
    path_function <- function(path) {
        if(file.exists(path)) {
            return(path)
        }
        new_path <- file.path(dirname(source_file), path)
        if(file.exists(new_path)) {
            return(new_path)
        }
        return(path)
    }
    files_table$path <- purrr::map(files_table$path, path_function)
    return(files_table)
}

.read_check_data_file_paths <- function(files_table) {
    file_exists <- file.exists(as.character(files_table$path))
    if(all(file_exists)) {
        return(files_table)
    }
    purrr::walk(files_table$path[!file_exists], ~ warning(stringr::str_glue(.read_const_MESSAGE_FILE_SKIP)))
    return(files_table[file_exists, ])
}

.read_init_localities_from_table <- function(localities_table) {
    result <- purrr::pmap(localities_table, .read_get_new_locality)
    names(result) <- localities_table$locality_id
    result
}

.read_check_data_formats <- function(user_data_formats) {
    if(is.null(user_data_formats))
        return()
    same_keys <- intersect(names(myClim::mc_data_formats), names(user_data_formats))
    if(length(same_keys) > 0)
        stop(.read_const_MESSAGE_USER_DATA_FORMAT_KEY)
}

.read_get_data_formats <- function(files_table, user_data_formats) {
    file_function <- function (path, data_format, logger_type, date_format, tz_offset) {
        if(data_format %in% names(myClim::mc_data_formats)) {
            data_format_object <- myClim::mc_data_formats[[data_format]]
        }
        else if(data_format %in% names(user_data_formats)) {
            data_format_object <- user_data_formats[[data_format]]
        }
        else {
            warning(stringr::str_glue(.read_const_MESSAGE_UNSUPPOERTED_FORMAT))
            if(!is.null(.read_state$check_bar)) .read_state$check_bar$tick()
            return(NULL)
        }
        if(any(!is.na(date_format))) {
            data_format_object@date_format <- date_format
        }
        if(!is.na(tz_offset)) {
            data_format_object@tz_offset <- tz_offset
        }
        if(!(is.na(logger_type) || logger_type == "")) {
            data_format_object@logger_type <- logger_type
        }
        data_format_object <- .model_load_data_format_params_from_file(data_format_object, path)
        if(is.null(data_format_object)) {
            warning(stringr::str_glue(.read_const_MESSAGE_UNAPLICABLE_FORMAT))
            if(!is.null(.read_state$check_bar)) .read_state$check_bar$tick()
            return(NULL)
        }
        if(!is.null(.read_state$check_bar)) .read_state$check_bar$tick()
        return(data_format_object)
    }

    logger_type <- NA_character_
    if("logger_type" %in% colnames(files_table)) {
        logger_type <- files_table$logger_type
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
                     logger_type=logger_type,
                     date_format=date_format,
                     tz_offset=tz_offset), file_function)
}

.read_get_edited_serial_numbers <- function(files_table, data_formats) {
    row_function <- function(path, locality_id, data_format, serial_number) {
        if(!(is.na(serial_number) || serial_number == "")) {
            return(serial_number)
        }
        serial_number <- .model_get_serial_number_from_file(data_format, path)
        if(is.na(serial_number))
        {
            stop(stringr::str_glue("It isn't possible to automatically detect serial_number from {path}."))
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
    files_table$index <- seq_len(nrow(files_table))
    groupped_files <- dplyr::group_by(files_table, .data$locality_id)
    locality_function <- function(.x, .y) {
        if(.y$locality_id %in% names(localities)) {
            locality <- localities[[.y$locality_id]]
        } else {
            locality <- .read_get_new_locality(.y$locality_id)
        }
        step <- if("step" %in% colnames(.x)) .x$step else NA_integer_
        parameters <- list(filename = .x$path,
                           data_format = data_formats[.x$index],
                           serial_number = .x$serial_number,
                           step = step)
        locality$loggers <- purrr::pmap(parameters, .read_logger)
        locality$loggers <- purrr::discard(locality$loggers, ~ is.null(.x))
        locality
    }
    result_localities <- dplyr::group_map(groupped_files, locality_function)
    result_localities <- purrr::discard(result_localities, ~ length(.x$loggers) == 0)
    if(length(result_localities) == 0) {
        stop(.read_const_MESSAGE_ANY_LOCALITY)
    }
    names(result_localities) <- purrr::map_chr(result_localities, function(.x) .x$metadata@locality_id)
    .read_get_data_raw_from_localities(result_localities)
}

.read_get_new_locality <- function(locality_id, elevation=NA_real_, lon_wgs84=NA_real_, lat_wgs84=NA_real_, tz_offset=NA_integer_, ...) {
    tz_type <- if(is.na(tz_offset)) .model_const_TZ_UTC else .model_const_TZ_USER_DEFINED
    metadata <- new("mc_LocalityMetadata")
    metadata@locality_id <- locality_id
    metadata@elevation <- elevation
    metadata@lon_wgs84 <- lon_wgs84
    metadata@lat_wgs84 <- lat_wgs84
    metadata@tz_offset <- tz_offset
    metadata@tz_type <- tz_type
    metadata@user_data <- list(...)
    list(metadata = metadata, loggers=list())
}

.read_get_data_raw_from_localities <- function(localities) {
    metadata <- new("mc_MainMetadata")
    myClimList(metadata, localities)
}

.read_logger <- function(filename, data_format, serial_number, step) {
    data_table <- .read_get_data_from_file(filename, data_format)
    .model_check_format(data_format)
    data_table <- .model_edit_data(data_format, data_table)
    data_table <- .read_fix_decimal_separator_if_need(filename, data_format, data_table)
    datetime <- data_table[[data_format@date_column]]
    if(!lubridate::is.POSIXct(datetime)) {
        datetime <- lubridate::parse_date_time(datetime, data_format@date_format, tz="UTC")
    }
    if(any(is.na(datetime))) {
        warning(stringr::str_glue(.read_const_MESSAGE_WRONG_DATETIME))
        if(!is.null(.read_state$read_bar)) .read_state$read_bar$tick()
        return(NULL)
    }
    if(data_format@tz_offset != 0) {
        datetime <- datetime - data_format@tz_offset * 60
    }
    states <- .read_create_source_states(filename, datetime)
    sensors <- .read_get_sensors_from_data_format(data_table, data_format, datetime, states)
    result <- .read_get_new_logger(datetime, sensors, serial_number, data_format@logger_type, step)
    if(!is.null(.read_state$read_bar)) .read_state$read_bar$tick()
    return(result)
}

.read_get_data_from_file <- function(filename, data_format, nrows=Inf) {
    result <- vroom::vroom(filename,
                           col_names = FALSE,
                           col_types = data_format@col_types,
                           col_select = if(is.na(data_format@col_types)) vroom::everything() else 1:stringr::str_length(data_format@col_types),
                           delim = data_format@separator,
                           skip = data_format@skip,
                           na = data_format@na_strings,
                           n_max = nrows,
                           show_col_types = FALSE,
                           progress = FALSE)
    problems <- data.frame()
    if("spec_tbl_df" %in% class(result)){
        problems <- vroom::problems(result)
    }
    if(nrow(problems) > 0) {
        warning(stringr::str_glue(.read_const_MESSAGE_VROOM_WARNING))
    }
    return(result)
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
        stop(stringr::str_glue("It isn't possible to load sensor data from {column_index} column in file {filename}."))
    }
    original_names <- colnames(data_table)
    result <- as.data.frame(purrr::map(seq(ncol(data_table)), values_function))
    colnames(result) <- original_names
    result
}

.read_get_new_logger <- function(datetime, sensors, serial_number=NA_character_, logger_type=NA_character_, step=NA_integer_) {
    metadata <- new("mc_LoggerMetadata")
    metadata@serial_number <- serial_number
    metadata@type <- logger_type
    metadata@step <- step
    list(metadata = metadata,
         clean_info = new("mc_LoggerCleanInfo"),
         datetime = datetime,
         sensors = sensors)
}

.read_get_sensors_from_data_format <- function(data_table, data_format, datetime, states){
    heights_dataframe <- dplyr::filter(myClim::mc_data_heights, .data$logger_type == data_format@logger_type)
    sensor_def_function <- function(column, sensor_id){
        height <- NA_character_
        suffix <- NA_character_
        sensor_filter <- heights_dataframe$sensor_name == sensor_id
        if(any(sensor_filter)) {
            height <- heights_dataframe$height[sensor_filter]
            suffix <- heights_dataframe$suffix[sensor_filter]
        }
        sensor_name <- if(is.na(suffix)) sensor_id else paste0(sensor_id, suffix)
        count <- length(column)
        result <- tibble::tibble(column=column,
                                 sensor_id=rep(sensor_id, count),
                                 sensor_name=rep(sensor_name, count),
                                 height=rep(height, count))
        if(count > 1) {
            result$sensor_name <- paste0(result$sensor_name, 1:count)
        }
        return(result)
    }
    sensor_table <- purrr::imap_dfr(data_format@columns, sensor_def_function)
    sensor_function <- function(column, sensor_id, sensor_name, height) {
        values <- data_table[[column]]
        sensor <- .common_get_new_sensor(sensor_id, sensor_name, values=values, height=height, states=states)
        sensor <- .read_set_errors_in_sensor(sensor, data_format@error_value, datetime)
        return(sensor)
    }
    result <- purrr::pmap(sensor_table, sensor_function)
    names(result) <- purrr::map_chr(result, ~ .x$metadata@name)
    result
}

.read_set_errors_in_sensor <- function(sensor, error_value, datetime) {
    if(is.na(error_value)) {
        return(sensor)
    }
    error_filter <- !is.na(sensor$values) & sensor$values == error_value
    if(!any(error_filter)) {
        return(sensor)
    }
    error_intervals <- .common_get_time_series_intervals(datetime, error_filter)
    error_states <- data.frame(tag = .model_const_SENSOR_STATE_ERROR,
                               start = lubridate::int_start(error_intervals),
                               end = lubridate::int_end(error_intervals),
                               value = as.character(error_value))
    sensor$values[error_filter] <- NA
    if(nrow(sensor$states) == 0) {
        sensor$states <- error_states
    } else {
        sensor$states <- dplyr::bind_rows(sensor$states, error_states)
    }
    return(sensor)
}

.read_create_source_states <- function(path, datetime) {
    abspath <- normalizePath(path)
    start <- dplyr::first(datetime)
    end <- dplyr::last(datetime)
    result <- data.frame(tag=.model_const_SENSOR_STATE_SOURCE,
                         start=start, end=end, value=abspath)
    return(.common_convert_factors_in_dataframe(result))
}

#' Reading data from wide data.frame
#'
#' This is universal function designed to read time-series and values
#' from wide data.frame to myClim object. Useful for data not coming from
#' supported microclimatic loggers. E.g. meteorological station data.
#' 
#' 
#' @details The first column of input data.frame must be datetime column in POSIXct time format UTC timezone.
#' Following columns represents localities. Column names are the localities names.
#' All values in wide data.frame represents the same sensor type, e.g. air temperature. If you wish to
#' read multiple sensors use [myClim::mc_read_long] or use [myClim::mc_read_wide] multiple times separately
#' for each sensor type and that merge myClim objects with [myClim::mc_prep_merge]
#' By default data are cleaned with function [myClim::mc_prep_clean()]. See function description. It detects
#' holes in time-series, duplicated records or records in wrong order.
#'
#' @param data_table data.frame with first column of POSIXct time format UTC timezone, 
#' followed by columns with (micro)climatic records. See details.
#'
#' Columns:
#' * datetime column - POSIXct in UTC timezone is required
#' * Name of locality\[1\] - values
#' * ...
#' * Name of locality\[n\] - values
#' @param sensor_id define the sensor type, one of `names(mc_data_sensors)` (default `real`)
#' @param sensor_name custom name of sensor; if NULL (default) than `sensor_name == sensor_id`
#' @param clean if TRUE, then [mc_prep_clean] is called automatically while reading (default TRUE)
#' @param silent if TRUE, then any information is printed in console (default FALSE)
#' @return myClim object in Raw-format
#' @export
#' @seealso [myClim::mc_read_long]
mc_read_wide <- function(data_table, sensor_id=mc_const_SENSOR_real, sensor_name=NULL, clean=TRUE, silent=FALSE) {
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
        sensors[[sensor_name]] <- .common_get_new_sensor(sensor_id, sensor_name, data_table[[locality$metadata@locality_id]])
        locality$loggers[[1]] <- .read_get_new_logger(data_table[[1]], sensors)
        locality
    }
    localities <- purrr::map(result, locality_function)
    data <- .read_get_data_raw_from_localities(localities)
    if(clean) {
        data <- mc_prep_clean(data, silent=silent)
    }
    return(data)
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
#' @details 
#' Similar like [myClim::mc_read_wide] but is capable to read multiple sensors
#' from single table. Useful for data not coming from supported microclimatic
#' loggers. E.g. meteorological station data.
#' By default data are cleaned with function [myClim::mc_prep_clean()].
#'
#' @param data_table long data.frame with Columns:
#' * locality_id - character; id of locality
#' * sensor_name - can be any character string, recommended are these: `names(mc_data_sensors)`
#' * datetime - POSIXct in UTC timezone is required
#' * value
#' @param sensor_ids list with relations between sensor_names and sensor_ids (default list());
#' sensor_id is key from `names(mc_data_sensors)`. E.g.,
#' `sensor_ids <- list(precipitation="real", maxAirT="T_C")`
#' If sensor_name is the same as sensor_id does not have to be provided.
#' @param clean if TRUE, then [mc_prep_clean] is called automatically while reading (default TRUE)
#' @param silent if TRUE, then any information is not printed in console (default FALSE)
#' @return myClim object in Raw-format
#' @export
#' @seealso [myClim::mc_read_wide]
mc_read_long <- function(data_table, sensor_ids=list(), clean=TRUE, silent=FALSE) {
    .read_check_datetime(data_table$datetime)

    data_table <- dplyr::group_by(data_table, .data$locality_id)
    localities <- dplyr::group_map(data_table, .read_long_locality, sensor_ids=sensor_ids)
    names(localities) <- purrr::map_chr(localities, ~ .x$metadata@locality_id)
    data <- .read_get_data_raw_from_localities(localities)
    if(clean) {
        data <- mc_prep_clean(data, silent=silent)
    }
    return(data)
}

.read_long_locality <- function(locality_table, locality_id, sensor_ids) {
    locality_id <- locality_id$locality_id[[1]]
    sensor_names <- unique(locality_table$sensor_name)
    sensor_table_function <- function(name) {
        data <- dplyr::filter(locality_table, .data$sensor_name == name)
        result <- dplyr::select(data, "datetime", "value")
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
        .common_get_new_sensor(sensor_id, sensor_name, table_values[[sensor_name]])
    }

    sensors <- purrr::map(sensor_names, sensor_function)
    names(sensors) <- purrr::map_chr(sensors, ~ .x$metadata@name)
    result$loggers[[1]] <- .read_get_new_logger(table_values$datetime, sensors)
    result
}

#' Reading data from TubeDB
#'
#' Function is reading data from TubeDB (https://environmentalinformatics-marburg.github.io/tubedb/) into myClim object.
#' @details 
#' In case you store your microclimatic time-series in TubeDB, you can read data
#' with TubeDB API into myClim object. You need to know database URL, username and password.
#'
#' @param tubedb object for connection to server see [rTubeDB::TubeDB-class]
#' @param region vector of TubeDB region ids - see [rTubeDB::query_regions] (default NULL)
#'
#' Regions are used mainly for loading metadata from TubeDB localities.
#' @param plot vector of localities ids see [rTubeDB::query_region_plots] [rTubeDB::query_timeseries] (default NULL)
#'
#' If plot is NULL, then all localities are loaded from whole region.
#' @param sensor_ids list in format `list(tubedb_sensor_name=myClim_sensor_name)` (default NULL)
#' If sensor names in TubeDB match the default sensor names in myClim, then the value is detected automatically.
#' @param clean if TRUE, then [mc_prep_clean] is called automatically while reading (default TRUE)
#' @param silent if TRUE, then any information is not printed in console (default FALSE)
#' @param aggregation parameter used in function [rTubeDB::query_timeseries] (default raw)
#' @param quality parameter used in function [rTubeDB::query_timeseries] (default no)
#' @param ... other parameters from function [rTubeDB::query_timeseries]
#' @return myClim object in Raw-format
#' @export
#' @examples
#' # Not run: To retrieve data from TubeDB, a running TubeDB server with a user account
#' #          and a secret password is required.
#' \dontrun{
#' tubedb <- TubeDB(url="server", user="user", password="password")
#' data <- mc_read_tubedb(tubedb, region="ckras", plot=c("TP_KAR_19", "TP_KODA_61"))
#' }
mc_read_tubedb <- function(tubedb, region=NULL, plot=NULL,
                           sensor_ids=NULL, clean=TRUE, silent=FALSE,
                           aggregation="raw", quality="no", ...) {
    if(is.null(plot) && is.null(region)) {
        stop(.read_const_MESSAGE_TUBEDB_PLOT_REGION_NULL)
    }
    if(is.null(region))
    {
        region <- .read_get_regions_from_plots(tubedb, plot)
    }
    plot_table <- .read_get_plot_table_from_regions(tubedb, region)
    if(is.null(sensor_ids))
    {
        sensor_ids <- .read_get_tubedb_sensors(tubedb, region)
    }

    if(is.null(plot)) {
        plot <- plot_table$id
    }
    plot_function <- function(plot_item) {
        tubedb_table <- rTubeDB::query_timeseries(tubedb, plot=plot_item, datetimeFormat="character",
                                            sensor=names(sensor_ids),
                                            aggregation=aggregation, quality=quality, ...)
        tubedb_table$datetime <- lubridate::ymd_hm(tubedb_table$datetime)
        result <- tidyr::pivot_longer(tubedb_table, !c(.data$plot, .data$datetime), names_to="sensor_name", values_to="value")
        return(result)
    }
    data_table <- purrr::map_dfr(plot, plot_function)
    data_table <- .read_get_data_table_for_import_from_tubedb(data_table)
    result <- mc_read_long(data_table, sensor_ids, clean, silent)
    if(is.null(plot_table)) {
        return(result)
    }
    result <- .read_load_metadata_from_tubedb_plots(result, plot_table, plot)
    return(result)
}

.read_get_regions_from_plots <- function(tubedb, plot) {
    regions <- rTubeDB::query_regions(tubedb)$id
    region_function <- function (region) {
        region_plots <- rTubeDB::query_region_plots(tubedb, region)$id
        return(any(plot %in% region_plots))
    }
    return(purrr::keep(regions, region_function))
}

.read_get_plot_table_from_regions <- function(tubedb, regions) {
    plot_table_function <- function(region) {
        return(rTubeDB::query_region_plots(tubedb, region))
    }
    return(purrr::map_dfr(regions, plot_table_function))
}

.read_get_data_table_for_import_from_tubedb <- function(data_table) {
    data_table <- dplyr::relocate(data_table, .data$plot, .data$sensor_name, .data$datetime, .data$value)
    colnames(data_table) <- c("locality_id", "sensor_name", "datetime", "value")
    data_table$datetime <- lubridate::force_tz(data_table$datetime, "UTC")
    return(data_table)
}

.read_load_metadata_from_tubedb_plots <- function(data, plot_table, plot) {
    plot_table <- dplyr::filter(plot_table, .data$id %in% plot)
    plot_table <- dplyr::select(plot_table, "id", "latitude", "longitude", "elevation")
    colnames(plot_table) <- c("locality_id", "lat_wgs84", "lon_wgs84", "elevation")
    plot_table$lat_wgs84 <- as(plot_table$lat_wgs84, "numeric")
    plot_table$lon_wgs84 <- as(plot_table$lon_wgs84, "numeric")
    plot_table$elevation <- as(plot_table$elevation, "numeric")
    result <- mc_prep_meta_locality(data, plot_table)
    return(result)
}

.read_get_tubedb_sensors <- function(tubedb, regions) {
    region_function <- function(region) {
        sensors_table <- rTubeDB::query_region_sensors(tubedb, regionID = region)
    }
    sensors_table <- purrr::map_dfr(regions, region_function)
    sensors_table <- dplyr::filter(sensors_table, !.data$derived)
    sensors_table <- unique(sensors_table)
    sensors_with_suffix <- dplyr::filter(myClim::mc_data_heights, !is.na(.data$suffix))
    sensors_with_suffix$full_name <- paste0(sensors_with_suffix$sensor_name, sensors_with_suffix$suffix)
    sensor_id_function <- function(sensor_id) {
        if(sensor_id %in% names(myClim::mc_data_sensors)) {
            return(sensor_id)
        }
        condition <- sensors_with_suffix$full_name == sensor_id
        if(any(condition)) {
            return(sensors_with_suffix$sensor_name[condition])
        }
        return(sensor_id)
    }
    result <- purrr::map(sensors_table$id, sensor_id_function)
    names(result) <- sensors_table$id
    return(result)
}
