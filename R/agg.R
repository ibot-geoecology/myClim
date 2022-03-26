.agg_const_MESSAGE_LOCALITY_WITHOUT_DATA <- "Locality {locality$metadata@locality_id} is without valid data. It is removed."

#' Aggregate data by function
#'
#' Function has two basic uses: 
#' * aggregate (upscale) time step of microclimatic records with specified function (e. g. 15 min records to daily means); 
#' * convert myClim object from Prep-format to Calc-format see [myClim-package] without records modification, this behavior appears wen fun=NULL, period=NULL.
#' 
#' @details 
#' Any output of mc_agg is in Calc-format. That means the structure of myClim object is flattened. Hierarchical level of logger is removed (Locality<-Logger<-Sensor<-Record), and all microclimatic records within the sensors are joined directly to the level of locality (Locality<-Sensor<-Record). This is called Calc-format and is only acceptable format for `mc_calc` functions family. See [myClim-package]. 
#' 
#' In case `mc_agg()` is used only for conversion from Prep-format to Calc-format (fun=NULL, period=NULL) then microclimatic records are not modified. 
#' 
#' When fun and period is specified, microclimatic records are aggregated based on function into new period. Aggregated time step is marked by a first time step of selected period i.e. day = c(2022-12-29 00:00, 2022-12-30 00:00...); week = c(2022-12-19 00:00, 2022-12-28 00:00...); month = c(2022-11-01 00:00, 2022-12-01 00:00...); year = c(2021-01-01 00:00, 2022-01-01 00:00...). 
#' When first or last period is incomplete in original data, the incomplete part is deleted, and a warning is shown (e.g. when original data starting on 2021-11-28 00:00 and period = ”month” then incomplete November is deleted and aggregation starts in December). 
#' 
#' Empty sensors with no records are excluded. `mc_agg()` return NA for empty vector except from count which returns 0. 
#' When aggregation functions are provided as vector or list e.g. c(mean,min,maxx), than they are applied to all sensors of input myClim object. When named list (names are the sensor ids) of functions is provided then `mc_agg()` apply specific functions to the specific sensors based on the named list. 

#' Aggregation function creates new sensors on localities putting aggregation 
#' function in its name (TMS_T1 -> TMS_T1_max), despite sensor names contains aggregation 
#' function, sensor_id stays the same as before aggregation in sensor metadata (e.g. TMS_T1).
#' Sensors created with functions `min`, `max`, `mean`, `percentile`, `sum`, `range` 
#' keeps identical sensor_id and value_type as original input sensors. Sensors created 
#' with functions `count` has sensor_id `count` and value_type `integer`, 
#' function  `coverage` has sensor_id `coverage` and value_type `real`
#'
#' @param data cleaned myClim object in Prep-format: output of [myClim::mc_prep_clean()] or Calc-format as it is allowed to aggregate data multiple times.
#' @param fun aggregation function; one of ("min", "max", "mean", "percentile", "sum", "range", "count", "coverage") See details.
#' Can be single function name, character vector of function names or named list of vector function names.
#' Named list of functions allows apply specific functions for different sensors e.g. `list(TMS_T1=c("max", "min"), TMS_T2="mean", TMS_T3_GDD="sum")`
#' if NULL records are not aggregated, but converted to Calc-format. See details.
#'
#' @param period Time period for aggregation - same as breaks in cut.POSIXt, e.g. ("hour", "day", "month"); if NULL then no aggregation
#'
#' There is special period "all" returning single value for each sensor based on function applied across all records within the sensor.
#'
#' Start day of week is Monday.
#' @param use_utc default TRUE, if set FALSE forced to use UTC time, instead possibly available time offset
#' (in locality metadata: tz_offset) local or solar time see (e.g. [myClim::mc_prep_solar_tz()], [myClim::mc_prep_meta()]);
#' Non-UTC time can by used only for period `day` and longer. 
#' @param percentiles vector of percentile numbers; numbers are from range 0-100; each specified percentile number generate new sensor, see details
#' @param na.rm parameter for aggregation function; Not used for count and coverage.
#' @return Returns new myClim object in Calc-format see [myClim-package] ready for `mc_calc` functions family. When fun=NULL, period=NULL
#' records are not modified but only converted to Calc-format. When fun and period provided then time step is aggregated based on function.
#' @export
#' @examples
#' hour_data <- mc_agg(mc_data_example_clean, c("min", "max", "percentile"), "hour", percentiles = 50, na.rm=TRUE)
#' day_data <- mc_agg(mc_data_example_clean, list(TMS_T1=c("max", "min"), TMS_T2="mean"), "day", na.rm=FALSE)
mc_agg <- function(data, fun=NULL, period=NULL, use_utc=TRUE, percentiles=NULL, na.rm=TRUE) {
    options(lubridate.week.start = 1)
    use_interval <- NULL
    if(!is.null(period) && period == "all") {
        use_interval <- myClim:::.common_get_cleaned_data_range(data)
    }
    period_object <- .agg_get_period_object(use_interval, period)
    .agg_check_fun_period(fun, period_object, use_utc)
    if(!use_utc) {
        myClim:::.prep_warn_if_unset_tz_offset(data)
    }
    original_step_text <- .agg_check_steps_and_get_original_text(data, fun, period_object)
    is_prep <- myClim:::.common_is_prep_format(data)
    locality_function <- function (locality) {
        tz_offset <- if(use_utc) 0 else locality$metadata@tz_offset
        if(is_prep) {
            return(.agg_aggregate_prep_locality(locality, fun, period, use_interval, percentiles, na.rm, tz_offset))
        } else {
            return(.agg_aggregate_item(locality, fun, period, use_interval, percentiles, na.rm, tz_offset, original_step_text))
        }
    }
    if(is_prep) {
        localities <- data
    } else {
        localities <- data$localities
    }
    new_localities <- purrr::map(localities, locality_function)
    new_localities <- purrr::keep(new_localities, function (x) !is.null(x))
    if(length(new_localities) == 0) {
        stop("Data are empty.")
    }

    if(is.null(period)) {
        number_of_seconds <- as.numeric(lubridate::as.period(original_step_text))
        step <- as.integer(number_of_seconds / 60)
        step_text <- stringr::str_glue("{step} min")
    } else if(period == "all") {
        number_of_seconds <- as.numeric(use_interval)
        step <- as.integer(number_of_seconds / 60)
        step_text <- stringr::str_glue("{step} min")
    } else {
        step_text <- period
        step <- .agg_get_step_from_period_object(period_object)
    }
    metadata <- new("mc_MainMetadata")
    metadata@step_text <- step_text
    metadata@step <- step
    list(metadata=metadata, localities=new_localities)
}

.agg_get_period_object <- function(use_interval, period) {
    if(is.null(period)) {
        return(NULL)
    }
    if(!is.null(use_interval)) {
        return(lubridate::as.period(use_interval))
    }
    lubridate::period(period)
}

.agg_check_fun_period <- function(fun, period_object, use_utc) {
    if(is.null(period_object) && is.null(fun)) {
        return()
    }
    if(is.null(fun) || is.null(period_object)) {
        stop("Parameters fun and period must be both NULL or must be both set.")
    }
    if(as.numeric(period_object) == 0) {
        stop("Period cannot be 0.")
    }
    if(!use_utc && period_object@year == 0 && period_object@month == 0 && period_object@day == 0) {
        stop("Non-UTC time zone can be used only for period day and bigger.")
    }
}

.agg_check_steps_and_get_original_text <- function(data, fun, period_object) {
    if(myClim:::.common_is_calc_format(data)) {
        return(data$metadata@step_text)
    }
    locality_function <- function(locality) {
        purrr::map_int(locality$loggers, function(.x) as.integer(.x$clean_info@step))
    }
    steps <- as.numeric(purrr::flatten(purrr::map(data, locality_function)))
    if(any(is.na(steps))) {
        stop("All steps must be set. Cleaning is required.")
    }
    if(!is.null(fun) && !is.null(period_object)) {
        return(NULL)
    }
    if(length(steps) > 1 && var(steps) != 0) {
        stop("All steps in loggers must be same.")
    }
    stringr::str_glue("{dplyr::first(steps)} min")
}

.agg_aggregate_prep_locality <- function(locality, fun, period, use_interval, percentiles, na.rm, tz_offset)
{
    logger_function <- function (logger) {
        original_step_text <- stringr::str_glue("{logger$clean_info@step} min")
        logger <- .agg_aggregate_item(logger, fun, period, use_interval, percentiles, na.rm, tz_offset, original_step_text)
        if(is.null(logger)) {
            return(logger)
        }
        logger$clean_info@step <- NA_integer_
        logger
    }
    if(!is.null(fun)){
        locality$loggers <- purrr::map(locality$loggers, logger_function)
        locality$loggers <- purrr::keep(locality$loggers, function (x) !is.null(x))
        if(period == "all") {
            step_text <- stringr::str_glue("{as.numeric(use_interval) / 60} min")
        } else {
            step_text <- period
        }
    } else {
        step_text <- stringr::str_glue("{locality$loggers[[1]]$clean_info@step} min")
    }
    .agg_get_flat_locality(locality, step_text, use_interval)
}

.agg_get_step_from_period_object <- function(period_object) {
    if(period_object@year > 0 || period_object@month > 0) {
        return(NA_integer_)
    }
    as.integer(as.numeric(period_object) / 60)
}

.agg_aggregate_item <- function(item, fun, period, use_interval, percentiles, na.rm, tz_offset, original_step_text)
{
    if(is.null(fun) || length(item$datetime) == 0) {
        return(item)
    }
    output_period <- lubridate::as.period(if(is.null(use_interval)) period else use_interval)
    if(output_period < lubridate::as.period(original_step_text)) {
        stop("It isn't possible aggregate from longer period to shorter one.")
    }
    item$datetime <- myClim:::.calc_get_datetimes_with_offset(item$datetime, tz_offset)
    item <- .agg_crop_data_to_whole_periods(item, period, use_interval, original_step_text)
    if(is.null(item)) {
        return(item)
    }
    if(is.null(use_interval)) {
        by_aggregate <- list(step=cut(item$datetime, breaks=period, start.on.monday = TRUE))
    } else {
        item <- .agg_extend_item_to_interval(item, use_interval, original_step_text)
        count <- length(item$datetime)
        by_aggregate <- list(step=as.factor(rep(lubridate::int_start(use_interval), count)))
    }
    datetime_function <- function(x) {
        if(!is.null(use_interval)) {
            return(lubridate::int_start(use_interval))
        }
        min(x)
    }
    item$datetime <- aggregate(item$datetime, by_aggregate, datetime_function)$x
    sensor_function <- function(sensor) {
        functions <- .agg_get_functions(sensor, fun, percentiles, na.rm)
        .agg_agregate_sensor(sensor, functions, by_aggregate)
    }
    item$sensors <- purrr::flatten(purrr::map(item$sensors, sensor_function))
    item
}

.agg_get_flat_locality <- function(locality, step_text, use_interval) {
    loggers <- purrr::keep(locality$loggers, function(x) length(x$datetime) > 0)
    if(length(loggers) == 0) {
        warning(stringr::str_glue(.agg_const_MESSAGE_LOCALITY_WITHOUT_DATA))
        return(NULL)
    } else if(length(loggers) == 1) {
        datetime <- loggers[[1]]$datetime
        sensors <- loggers[[1]]$sensors
    } else {
        datetime <- .agg_get_locality_datetime(loggers, step_text, use_interval)
        sensors <- .agg_get_merged_sensors(datetime, loggers)
    }
    if(length(sensors) == 0) {
        warning(stringr::str_glue(.agg_const_MESSAGE_LOCALITY_WITHOUT_DATA))
        return(NULL)
    }

    list(metadata = locality$metadata,
         datetime = datetime,
         sensors = sensors)
}

.agg_get_locality_datetime <- function(loggers, step_text, use_interval) {
    if(is.null(use_interval)) {
        return(.agg_get_datetimes_from_sensor_items(loggers, step_text))
    }
    lubridate::int_start(use_interval)
}

.agg_get_datetimes_from_sensor_items <- function(items, step_text) {
    min_datetime_function <- function(.x) {
        if(length(.x$datetime) == 0) return(NA_integer_)
        as.integer(.x$datetime[[1]])}
    min_datetime <- myClim:::.common_as_utc_posixct(min(purrr::map_int(items, min_datetime_function), na.rm=TRUE))
    max_datetime_function <- function(.x) {
        if(length(.x$datetime) == 0) return(NA_integer_)
        as.integer(tail(.x$datetime, n=1))}
    max_datetime <- myClim:::.common_as_utc_posixct(max(purrr::map_int(items, max_datetime_function), na.rm=TRUE))
    seq(min_datetime, max_datetime, by=step_text)
}

.agg_get_merged_sensors <- function(datetime, sensor_items) {
    sensor_items <- .agg_get_items_with_renamed_sensors(sensor_items)
    tables <- c(list(tibble::tibble(datetime=datetime)), purrr::map(sensor_items, myClim:::.common_sensor_values_as_tibble))
    table_values <- purrr::reduce(tables, function(.x, .y) dplyr::left_join(.x, .y, by="datetime"))

    sensor_function <- function (sensor) {
        sensor$values <- table_values[[sensor$metadata@name]]
        sensor
    }

    item_function <- function (item) {
        purrr::map(item$sensors, sensor_function)
    }

    purrr::flatten(purrr::map(sensor_items, item_function))
}

.agg_get_items_with_renamed_sensors <- function(sensor_items) {
    existed_names <- new.env()

    rename_sensor_name_function <- function(sensor) {
        original_sensor_name <- sensor$metadata@name
        sensor_name <- original_sensor_name
        number <- 1
        while(!is.null(existed_names[[sensor_name]])) {
            sensor_name <- stringr::str_glue("{original_sensor_name}_{number}")
            number <- number + 1
        }
        if(sensor_name != original_sensor_name) {
            warning(stringr::str_glue("sensor {original_sensor_name} is renamed to {sensor_name}"))
            sensor$metadata@name <- sensor_name
        }
        existed_names[[sensor_name]] <- TRUE
        sensor
    }

    rename_sensors_in_item_function <- function(item) {
        item$sensors <- purrr::map(item$sensors, rename_sensor_name_function)
        names(item$sensors) <- purrr::map_chr(item$sensors, ~ .x$metadata@name)
        item
    }

    purrr::map(sensor_items, rename_sensors_in_item_function)
}

.agg_crop_data_to_whole_periods <- function(item, period, use_interval, original_step_text) {
    start <- dplyr::first(item$datetime)
    end <- dplyr::last(item$datetime)
    if(is.null(use_interval)) {
        cropping_info <- .agg_get_cropping_info_by_period_and_original_step(start, end, period, original_step_text)
        cropping <- cropping_info$cropping
        start <- cropping_info$start
        end <- cropping_info$end
    } else {
        cropping <- (start < lubridate::int_start(use_interval) || end > lubridate::int_end(use_interval))
        start <- lubridate::int_start(use_interval)
        end <- lubridate::int_end(use_interval)
    }

    if(cropping) {
        item_id <- myClim:::.common_get_id_of_item_with_sensors(item)
        if(start >= end) {
            warning(stringr::str_glue("{item_id} is without valid data. It is removed."))
            return(NULL)
        }
        item_id <- myClim:::.common_get_id_of_item_with_sensors(item)
        warning(stringr::str_glue("{item_id} is cropped to range ({start}, {end})."))
        item <- myClim:::.prep_crop_data(item, start, end, end_included=FALSE)
    }
    item
}

.agg_get_cropping_info_by_period_and_original_step <- function(start, end, period, original_step_text) {
    cropping <- FALSE
    original_step_period <- lubridate::as.period(original_step_text)
    first_period <- lubridate::floor_date(start, period)
    first_period_previous <- lubridate::floor_date(start - original_step_period, period)
    if(first_period == first_period_previous) {
        cropping <- TRUE
        start <- lubridate::ceiling_date(start, period)
    }
    last_period <- lubridate::floor_date(end, period)
    last_period_next <- lubridate::floor_date(end + original_step_period, period)
    if(last_period == last_period_next) {
        cropping <- TRUE
        end <- last_period
    } else {
        end <- last_period_next
    }
    list(start=start, end=end, cropping=cropping)
}

.agg_extend_item_to_interval <- function(item, use_interval, original_step_text) {
    if(use_interval == lubridate::interval(dplyr::first(item$datetime), dplyr::last(item$datetime))) {
        return(item)
    }
    start <- lubridate::ceiling_date(lubridate::int_start(use_interval), original_step_text)
    end <- lubridate::floor_date(lubridate::int_end(use_interval), original_step_text)
    datetime <- tryCatch({
            seq(start, end, by=original_step_text)
        },
        error = function(e){
            seconds <- as.numeric(lubridate::as.period(original_step_text))
            seq(start, end, by=stringr::str_glue("{seconds} sec"))
        })
    datetime_table <- tibble::as_tibble(datetime)
    colnames(datetime_table) <- "datetime"
    item_table <- myClim:::.common_sensor_values_as_tibble(item)
    result_table <- dplyr::left_join(datetime_table, item_table, by="datetime")
    item$datetime <- result_table$datetime
    sensor_function <- function(sensor){
        sensor$values <- result_table[[sensor$metadata@name]]
        sensor
    }
    item$sensors <- purrr::map(item$sensors, sensor_function)
    item
}

.agg_get_functions <- function(sensor, fun, percentiles, na.rm) {
    if(class(fun) == "character") {
        functions_to_convert <- fun
    } else if (sensor$metadata@name %in% names(fun)) {
        functions_to_convert <- fun[[sensor$metadata@name]]
    } else {
        return(NULL)
    }
    value_type <- mc_data_sensors[[sensor$metadata@sensor_id]]@value_type
    purrr::flatten(purrr::map(functions_to_convert, function(x) .agg_convert_function(x, percentiles, na.rm, value_type)))
}

.agg_convert_function <- function(function_text, percentiles, na.rm, value_type) {
    if(function_text == "min") {
        return(list(min=function(x) {
            x <- .agg_function_prepare_data(x, na.rm)
            if(length(x) == 0) return(NA)
            .agg_function_convert_result(min(x), value_type)
        }))
    } else if(function_text == "max") {
        return(list(max=function(x){
            x <- .agg_function_prepare_data(x, na.rm)
            if(length(x) == 0) return(NA)
            .agg_function_convert_result(max(x), value_type)
        }))
    } else if(function_text == "mean") {
        return(list(mean=function(x) {
            x <- .agg_function_prepare_data(x, na.rm)
            if(length(x) == 0) return(NA)
            .agg_function_convert_result(mean(x), value_type)
        }))
    } else if(function_text == "percentile") {
        return(.agg_convert_percentile_functions(percentiles, na.rm, value_type))
    } else if(function_text == "sum") {
        return(list(sum=function(x) {
            x <- .agg_function_prepare_data(x, na.rm)
            if(length(x) == 0) return(NA)
            .agg_function_convert_result(sum(x), value_type)
        }))
    } else if(function_text == "range") {
        return(list(range=function(x) {
            x <- .agg_function_prepare_data(x, na.rm)
            if(length(x) == 0) return(NA)
            .agg_function_convert_result(max(x) - min(x), value_type)
        }))
    } else if(function_text == "count") {
        return(list(count=function(x) length(x[!is.na(x)])))
    } else if(function_text == "coverage") {
        return(list(coverage=function(x) {
            if(length(x) == 0) return(NA)
            length(x[!is.na(x)]) / length(x)
        }))
    }
    NULL
}

.agg_function_prepare_data <- function(values, na.rm) {
    if(na.rm){
        return(values[!is.na(values)])
    }
    values
}

.agg_function_convert_result <- function(values, value_type) {
    if(value_type == "logical"){
        return(as.logical(round(values)))
    }
    if(value_type == "integer"){
        return(round(values))
    }
    values
}

.agg_convert_percentile_functions <- function(percentiles, na.rm, value_type) {
    percentile_function <- function(percentile) {
        quantile <- percentile / 100
        function(x) {
            if(!na.rm && any(is.na(x))) {
                return(NA)
            }
            .agg_function_convert_result(unname(quantile(x, quantile, na.rm=na.rm)), value_type)
        }
    }
    result <- purrr::map(percentiles, percentile_function)
    names(result) <- purrr::map_chr(percentiles, function(x) stringr::str_glue("percentile{x}"))
    result
}

.agg_agregate_sensor <- function(sensor, functions, by_aggregate) {
    sensor_function <- function(.x, .y) {
        new_sensor <- sensor
        if(.y %in% c("count", "coverage")) {
            new_sensor$metadata@sensor_id <- .y
        }
        new_sensor$metadata@name <- stringr::str_glue("{new_sensor$metadata@name}_{.y}")
        new_sensor$values <- aggregate(new_sensor$values, by_aggregate, .x)$x
        new_sensor
    }
    result <- purrr::imap(functions, sensor_function)
    names(result) <- purrr::map_chr(result, function(x) x$metadata@name)
    result
}