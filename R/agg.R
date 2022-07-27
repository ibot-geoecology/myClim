.agg_const_PERIOD_ALL <- "all"
.agg_const_PERIOD_CUSTOM <- "custom"
.agg_const_INTERVAL_PERIODS <- c(.agg_const_PERIOD_ALL, .agg_const_PERIOD_CUSTOM)

.agg_const_FUNCTION_MIN <- "min"
.agg_const_FUNCTION_MAX <- "max"
.agg_const_FUNCTION_MEAN <- "mean"
.agg_const_FUNCTION_PERCENTILE <- "percentile"
.agg_const_FUNCTION_SUM <- "sum"
.agg_const_FUNCTION_RANGE <- "range"
.agg_const_FUNCTION_COUNT <- "count"
.agg_const_FUNCTION_COVERAGE <- "coverage"

.agg_const_MESSAGE_LOCALITY_WITHOUT_DATA <- "Locality {locality$metadata@locality_id} is without valid data. It is removed."
.agg_const_MESSAGE_CUSTOM_START_NULL <- "Parameter custom_start must be set."
.agg_const_MESSAGE_CUSTOM_WRONG_FORMAT <- "Prameter {parameter_name} is in wrong format. Required format is 'mm-dd' or 'mm-dd H:MM'."
.agg_const_MESSAGE_EMPTY_DATA <- "Data are empty."
.agg_const_MESSAGE_WRONG_PREVIOUS_PERIOD <- "It is not possible aggregate all or custom data."
.agg_const_MESSAGE_WRONG_SHIFT <- "Shift of time-series in {locality$metadata@locality_id} locality is different."
.agg_const_MESSAGE_MISSING_HEIGHT <- "Height is missing in sensosr {object@name}."
.agg_const_MESSAGE_WRONG_CUSTOM_FUNCTION <- "Type of values in sensor {new_sensor$metadata@name} is wrong."

#' Aggregate data by function
#'
#' Function has two basic uses: 
#' * aggregate (upscale) time step of microclimatic records with specified function (e. g. 15 min records to daily means); 
#' * convert myClim object from Prep-format to Calc-format see [myClim-package] without records modification, this behavior appears wen fun=NULL, period=NULL.
#' 
#' @details 
#' Any output of mc_agg is in Calc-format. That means the structure of myClim object is flattened.
#' Hierarchical level of logger is removed (Locality<-Logger<-Sensor<-Record), and all microclimatic records within
#' the sensors are joined directly to the level of locality (Locality<-Sensor<-Record).
#' This is called Calc-format and is only acceptable format for `mc_calc` functions family. See [myClim-package].
#' 
#' In case `mc_agg()` is used only for conversion from Prep-format to Calc-format (fun=NULL, period=NULL) then microclimatic
#' records are not modified. Equal step in all sensors is required for conversion from Prep-format to Calc-format.
#' 
#' When fun and period is specified, microclimatic records are aggregated based on function into new period.
#' Aggregated time step is marked by a first time step of selected period i.e. day = c(2022-12-29 00:00, 2022-12-30 00:00...);
#' week = c(2022-12-19 00:00, 2022-12-28 00:00...); month = c(2022-11-01 00:00, 2022-12-01 00:00...);
#' year = c(2021-01-01 00:00, 2022-01-01 00:00...).
#' When first or last period is incomplete in original data, the incomplete part is deleted, and a warning is shown
#' (e.g. when original data starting on 2021-11-28 00:00 and period = ”month” then incomplete November is deleted
#' and aggregation starts in December). 
#' 
#' The behavior is a bit diferent for special period `"all"`. Incomplete datetime series aren't
#' deleted, but result of aggregation depends on `na.rm` parameter. If `na.rm=T` then returns value even in case you have only few records out of 365 days. 
#' CAUTION! `na.rm=T` is default. If `na.rm=F` returns NA when missing data occures.      
#' 
#' Empty sensors with no records are excluded. `mc_agg()` return NA for empty vector except from count which returns 0. 
#' When aggregation functions are provided as vector or list e.g. c(mean, min, max), than they are applied to all sensors
#' of input myClim object. When named list (names are the sensor ids) of functions is provided then `mc_agg()`
#' apply specific functions to the specific sensors based on the named list.

#' Aggregation function creates new sensors on localities putting aggregation 
#' function in its name (TMS_T1 -> TMS_T1_max), despite sensor names contains aggregation 
#' function, sensor_id stays the same as before aggregation in sensor metadata (e.g. TMS_T1).
#' Sensors created with functions `min`, `max`, `mean`, `percentile`, `sum`, `range` 
#' keeps identical sensor_id and value_type as original input sensors. 
#' When function `sum` is applied on `logical` sensor (e.g. snow as TRUE, FALSE) the 
#' output is  `integer` i.e. number of `TRUE` values.
#'
#' Sensors created with functions `count` has sensor_id `count` and value_type `integer`,
#' function  `coverage` has sensor_id `coverage` and value_type `real`
#' 
#' Read carefully details about `sum` and `count` these two may not be intuitive.  
#'
#' @param data cleaned myClim object in Prep-format: output of [myClim::mc_prep_clean()] or Calc-format as it is allowed to aggregate data multiple times.
#' @param fun aggregation function; one of (`"min"`, `"max"`, `"mean"`, `"percentile"`, `"sum"`, `"range"`, `"count"`, `"coverage"`)
#' §and functions defined in `custom_functions` parameter§ See details.
#' Can be single function name, character vector of function names or named list of vector function names.
#' Named list of functions allows apply specific functions for different sensors e.g. `list(TMS_T1=c("max", "min"), TMS_T2="mean", TMS_T3_GDD="sum")`
#' if NULL records are not aggregated, but converted to Calc-format. See details.
#'
#' @param period Time period for aggregation - same as breaks in cut.POSIXt, e.g. (`"hour"`, `"day"`, `"month"`); if NULL then no aggregation
#'
#' There are special periods `"all"` and `"custom"`. Period `"all"` returning single value for each sensor based
#' on function applied across all records within the sensor. E.g. mean and max air temperature from all data from the logger.
#' Period `"custom"` aggregates data in year time window. You can aggregate e.g. water year, vegetation season etc by providing start, end datetime.  
#' See `custom_start` and `custom_end` parameters. The output of special periods `"all"` and `"custom"`is not allowed to be aggregated 
#' again in [myClim::mc_agg()] function. 
#'
#' Start day of week is Monday.
#' @param use_utc default TRUE, if set FALSE forced to use UTC time, instead possibly available time offset
#' (in locality metadata: tz_offset) local or solar time see (e.g. [myClim::mc_prep_solar_tz()], [myClim::mc_prep_meta_locality()]);
#' Non-UTC time can by used only for period `day` and longer. 
#' @param percentiles vector of percentile numbers; numbers are from range 0-100; each specified percentile number generate new sensor, see details
#' @param na.rm parameter for aggregation function; Not used for count and coverage. special importance for period `"all"` and `"custom"` see details
#' @param custom_start date of start only use for `custom` period (defaul NULL); Character in format `"mm-dd"` or `"mm-dd H:MM"`.
#' @param custom_end date of end only use for `custom` period (defaul NULL); If NULL then calculates in year cycle ending on `custom_start` next year. 
#' If parameter is filled in then data out of range `custom_start`-`custom_end` are skipped. E.g. vegetation season, winter season... 
#' Character in format `"mm-dd"` or `"mm-dd H:MM"`. `custom_end` row is not included. I.e.complete daily data from year 2020 ends in 2021-01-01 `custom_end="01-01"`.
#' @param custom_functions §user defined functions in format `list(function_name=function(values){...})`; You can use function_name in `fun` parameter.§
#' @return Returns new myClim object in Calc-format see [myClim-package] ready for `mc_calc` functions family. When fun=NULL, period=NULL
#' records are not modified but only converted to Calc-format. When fun and period provided then time step is aggregated based on function.
#' @export
#' @examples
#' hour_data <- mc_agg(mc_data_example_clean, c("min", "max", "percentile"), "hour", percentiles = 50, na.rm=TRUE)
#' day_data <- mc_agg(mc_data_example_clean, list(TMS_T1=c("max", "min"), TMS_T2="mean"), "day", na.rm=FALSE)
mc_agg <- function(data, fun=NULL, period=NULL, use_utc=TRUE, percentiles=NULL, na.rm=TRUE,
                   custom_start=NULL, custom_end=NULL, custom_functions=NULL) {
    old_lubridate_week_start <- getOption("lubridate.week.start")
    options(lubridate.week.start = 1)
    use_intervals <- .agg_get_use_intervals(data, period, custom_start, custom_end)
    period_object <- .agg_get_period_object(use_intervals, period)
    .agg_check_fun_period(fun, period_object, use_utc)
    use_utc <- .agg_get_use_utc(data, use_utc)
    original_period <- .agg_check_steps_and_get_original_text(data, fun, period_object)
    is_prep <- myClim:::.common_is_prep_format(data)
    locality_function <- function (locality) {
        tz_offset <- if(use_utc) 0 else locality$metadata@tz_offset
        if(is_prep) {
            return(.agg_aggregate_prep_locality(locality, fun, period, use_intervals, percentiles, na.rm, tz_offset, custom_functions))
        } else {
            return(.agg_aggregate_item(locality, fun, period, use_intervals, percentiles, na.rm, tz_offset, original_period, custom_functions))
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
        stop(.agg_const_MESSAGE_EMPTY_DATA)
    }

    intervals_start <- lubridate::NA_POSIXct_
    intervals_end <- lubridate::NA_POSIXct_
    if(is.null(period)) {
        number_of_seconds <- as.numeric(lubridate::as.period(original_period))
        step <- as.integer(number_of_seconds / 60)
        period <- stringr::str_glue("{step} min")
    } else if(!is.null(use_intervals)) {
        step <- .agg_get_step_from_use_intervals(use_intervals)
        intervals_start <- lubridate::int_start(use_intervals)
        intervals_end <- lubridate::int_end(use_intervals)
    } else {
        step <- .agg_get_step_from_period_object(period_object)
    }
    metadata <- new("mc_MainMetadata")
    metadata@period <- period
    metadata@step <- step
    metadata@intervals_start <- intervals_start
    metadata@intervals_end <- intervals_end
    options(lubridate.week.start = old_lubridate_week_start)
    list(metadata=metadata, localities=new_localities)
}

.agg_get_use_intervals <- function(data, period, custom_start, custom_end) {
    result <- NULL
    if(!is.null(period) && period %in% .agg_const_INTERVAL_PERIODS) {
        result <- myClim:::.common_get_cleaned_data_range(data, add_step_to_end = TRUE)
        lubridate::int_end(result) <- lubridate::int_end(result) - lubridate::seconds(1)
    }
    if(!is.null(period) && period == .agg_const_PERIOD_CUSTOM) {
        custom_dates <- .agg_parse_custom_dates(custom_start, custom_end)
        result <- .agg_get_custom_intervals(result, custom_dates)
    }
    result
}

.agg_parse_custom_dates <- function(custom_start, custom_end) {
    if(is.null(custom_start)) {
        stop(.agg_const_MESSAGE_CUSTOM_START_NULL)
    }
    date_format <- "^(\\d{2})-(\\d{2})(?: (\\d{1,2}):(\\d{2}))?$"
    parts <- stringr::str_match(custom_start, date_format)
    if(is.na(parts[1,1])) {
        parameter_name <- "custom_start"
        stop(stringr::str_glue(.agg_const_MESSAGE_CUSTOM_WRONG_FORMAT))
    }
    result <- list(end_month=NA_integer_, end_day=NA_integer_,
                   end_hour=NA_integer_, end_minute=NA_integer_)
    result$start_month <- as.integer(parts[1, 2])
    result$start_day <- as.integer(parts[1, 3])
    result$start_hour <- if(is.na(parts[1, 4])) 0 else as.integer(parts[1, 4])
    result$start_minute <- if(is.na(parts[1, 5])) 0 else as.integer(parts[1, 5])
    if(is.null(custom_end)) {
        return(result)
    }
    parts <- stringr::str_match(custom_end, date_format)
    if(is.na(parts[1,1])) {
        parameter_name <- "custom_end"
        stop(stringr::str_glue(.agg_const_MESSAGE_CUSTOM_WRONG_FORMAT))
    }
    result$end_month <- as.integer(parts[1, 2])
    result$end_day <- as.integer(parts[1, 3])
    result$end_hour <- if(is.na(parts[1, 4])) 0 else as.integer(parts[1, 4])
    result$end_minute <- if(is.na(parts[1, 5])) 0 else as.integer(parts[1, 5])
    result
}

.agg_get_custom_intervals <- function(whole_interval, custom_dates) {
    first_interval <- .agg_get_first_custom_interval(whole_interval, custom_dates)
    last_interval <- .agg_get_last_custom_interval(whole_interval, first_interval)
    count_years <- lubridate::year(lubridate::int_start(last_interval)) - lubridate::year(lubridate::int_start(first_interval))
    intervals <- lubridate::int_shift(first_interval, lubridate::years(seq(0, count_years)))
    lubridate::int_end(intervals) <- lubridate::int_end(intervals) - lubridate::seconds(1)
    intervals
}

.agg_get_first_custom_interval <- function(whole_interval, custom_dates) {
    start <- lubridate::int_start(whole_interval)
    end <- lubridate::int_end(whole_interval)
    start_interval <- lubridate::make_datetime(lubridate::year(start),
                                               custom_dates$start_month,
                                               custom_dates$start_day,
                                               custom_dates$start_hour,
                                               custom_dates$start_minute)
    if(is.na(custom_dates$end_month)) {
        end_interval <- start_interval + lubridate::years(1)
    } else {
        end_interval <- lubridate::make_datetime(lubridate::year(start),
                                                 custom_dates$end_month,
                                                 custom_dates$end_day,
                                                 custom_dates$end_hour,
                                                 custom_dates$end_minute)
    }
    if(end_interval < start_interval) {
        end_interval <- end_interval + lubridate::years(1)
    }
    result <- lubridate::interval(start_interval, end_interval)
    if(lubridate::`%within%`(start, result) && start != end_interval) {
        return(result)
    }
    if(end_interval <= start) {
        result <- lubridate::int_shift(result, lubridate::years(1))
    }
    else if(start_interval > start) {
        previous_interval <- lubridate::int_shift(result, lubridate::years(-1))
        if(lubridate::int_end(previous_interval) > start) {
            result <- previous_interval
        }
    }
    if(lubridate::int_start(result) > end) {
        stop(.agg_const_MESSAGE_EMPTY_DATA)
    }
    result
}

.agg_get_last_custom_interval <- function(whole_interval, first_interval) {
    end <- lubridate::int_end(whole_interval)
    start_first_interval <- lubridate::int_start(first_interval)
    count_years <- lubridate::year(end) - lubridate::year(start_first_interval)
    result <- lubridate::int_shift(first_interval, lubridate::years(count_years))
    if(lubridate::int_start(result) > end) {
        result <- lubridate::int_shift(result, lubridate::years(-1))
    }
    result
}

.agg_get_period_object <- function(use_intervals, period) {
    if(is.null(period)) {
        return(NULL)
    }
    if(!is.null(use_intervals)) {
        return(lubridate::as.period(use_intervals))
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
    if(as.numeric(period_object[[1]]) == 0) {
        stop("Period cannot be 0.")
    }
    if(!use_utc && period_object[[1]]@year == 0 && period_object[[1]]@month == 0 && period_object[[1]]@day == 0) {
        stop("Non-UTC time zone can be used only for period day and bigger.")
    }
}

.agg_get_use_utc <- function(data, use_utc) {
    is_calc <- myClim:::.common_is_calc_format(data)
    if(!use_utc && is_calc && (is.na(data$metadata@step) || data$metadata@step >= 60*24)) {
        use_utc <- TRUE
    }
    if(!use_utc) {
        myClim:::.prep_warn_if_unset_tz_offset(data)
    }
    use_utc
}

.agg_check_steps_and_get_original_text <- function(data, fun, period_object) {
    if(myClim:::.common_is_calc_format(data)) {
        if(data$metadata@period %in% .agg_const_INTERVAL_PERIODS) {
            stop(.agg_const_MESSAGE_WRONG_PREVIOUS_PERIOD)
        }
        return(data$metadata@period)
    }
    step_locality_function <- function(locality) {
        purrr::map_int(locality$loggers, function(.x) as.integer(.x$clean_info@step))
    }
    steps <- as.numeric(purrr::flatten(purrr::map(data, step_locality_function)))
    if(any(is.na(steps))) {
        stop("All steps must be set. Cleaning is required.")
    }
    if(!is.null(fun) && !is.null(period_object)) {
        return(NULL)
    }
    if(length(steps) > 1 && var(steps) != 0) {
        stop("All steps in loggers must be same.")
    }
    shift_locality_function <- function(locality) {
        shifts <- purrr::map_int(locality$loggers, function(.x) as.integer(.x$datetime[[1]]) %% as.integer(.x$clean_info@step * 60))
        if(length(shifts) > 1 && var(shifts) != 0) {
            stop(stringr::str_glue(.agg_const_MESSAGE_WRONG_SHIFT))
        }
    }
    purrr::walk(data, shift_locality_function)
    stringr::str_glue("{dplyr::first(steps)} min")
}

.agg_aggregate_prep_locality <- function(locality, fun, period, use_intervals, percentiles, na.rm, tz_offset, custom_functions)
{
    logger_function <- function (logger) {
        original_period <- stringr::str_glue("{logger$clean_info@step} min")
        logger <- .agg_aggregate_item(logger, fun, period, use_intervals, percentiles, na.rm, tz_offset, original_period, custom_functions)
        if(is.null(logger)) {
            return(logger)
        }
        logger$clean_info@step <- NA_integer_
        logger
    }
    if(!is.null(fun)){
        locality$loggers <- purrr::map(locality$loggers, logger_function)
        locality$loggers <- purrr::keep(locality$loggers, function (x) !is.null(x))
    } else {
        period <- stringr::str_glue("{locality$loggers[[1]]$clean_info@step} min")
    }
    .agg_get_flat_locality(locality, period, use_intervals)
}

.agg_get_step_from_use_intervals <- function(use_intervals) {
    if(length(use_intervals) == 1 || var(as.integer(use_intervals)) == 0) {
        return((as.integer(dplyr::first(use_intervals)) + 1) / 60)
    }
    NA_integer_
}

.agg_get_step_from_period_object <- function(period_object) {
    if(period_object@year > 0 || period_object@month > 0) {
        return(NA_integer_)
    }
    as.integer(as.numeric(period_object) / 60)
}

.agg_aggregate_item <- function(item, fun, period, use_intervals, percentiles, na.rm, tz_offset, original_period,
                                custom_functions)
{
    if(is.null(fun) || length(item$datetime) == 0) {
        return(item)
    }
    output_period <- lubridate::as.period(if(is.null(use_intervals)) period else use_intervals[[1]])
    if(output_period < lubridate::as.period(original_period)) {
        stop("It isn't possible aggregate from longer period to shorter one.")
    }
    item$datetime <- myClim:::.calc_get_datetimes_with_offset(item$datetime, tz_offset)
    item <- .agg_crop_data_to_whole_periods(item, period, use_intervals, original_period)
    if(is.null(item)) {
        return(item)
    }
    if(is.null(use_intervals)) {
        start_datetimes <- lubridate::floor_date(item$datetime, period)
    } else {
        if (period == .agg_const_PERIOD_ALL) {
            item <- .agg_extend_item_to_all_interval(item, use_intervals, original_period)
        }
        interval_function <- function(interval) {
            count <- sum(lubridate::`%within%`(item$datetime, interval))
            rep(lubridate::int_start(interval), count)
        }
        start_datetimes <- myClim:::.common_as_utc_posixct(unlist(purrr::map(use_intervals, interval_function)))
    }
    item$datetime <- unique(start_datetimes)
    by_aggregate <- list(step=as.factor(start_datetimes))
    sensor_function <- function(sensor) {
        functions <- .agg_get_functions(sensor, fun, percentiles, na.rm, custom_functions)
        .agg_agregate_sensor(sensor, functions, by_aggregate, custom_functions)
    }
    item$sensors <- purrr::flatten(purrr::map(item$sensors, sensor_function))
    item
}

.agg_get_flat_locality <- function(locality, period, use_interval) {
    loggers <- purrr::keep(locality$loggers, function(x) length(x$datetime) > 0)
    if(length(loggers) == 0) {
        warning(stringr::str_glue(.agg_const_MESSAGE_LOCALITY_WITHOUT_DATA))
        return(NULL)
    } else if(length(loggers) == 1) {
        datetime <- loggers[[1]]$datetime
        sensors <- loggers[[1]]$sensors
    } else {
        datetime <- .agg_get_locality_datetime(loggers, period, use_interval)
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

.agg_get_locality_datetime <- function(loggers, period, use_interval) {
    if(is.null(use_interval)) {
        return(.agg_get_datetimes_from_sensor_items(loggers, period))
    }
    lubridate::int_start(use_interval)
}

.agg_get_datetimes_from_sensor_items <- function(items, period) {
    min_datetime_function <- function(.x) {
        if(length(.x$datetime) == 0) return(NA_integer_)
        as.integer(.x$datetime[[1]])}
    min_datetime <- myClim:::.common_as_utc_posixct(min(purrr::map_int(items, min_datetime_function), na.rm=TRUE))
    max_datetime_function <- function(.x) {
        if(length(.x$datetime) == 0) return(NA_integer_)
        as.integer(tail(.x$datetime, n=1))}
    max_datetime <- myClim:::.common_as_utc_posixct(max(purrr::map_int(items, max_datetime_function), na.rm=TRUE))
    seq(min_datetime, max_datetime, by=period)
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

.agg_crop_data_to_whole_periods <- function(item, period, use_intervals, original_period) {
    if(!is.null(use_intervals)) {
        item <- .agg_remove_values_outside_intervals(item, use_intervals)
        if(length(item$datetime) == 0)
        {
            return(NULL)
        }
    }

    start <- dplyr::first(item$datetime)
    end <- dplyr::last(item$datetime)

    if(is.null(use_intervals)) {
        cropping_info <- .agg_get_cropping_info_by_period_and_original_step(start, end, period, original_period)
    } else {
        cropping_info <- .agg_get_cropping_info_by_intervals(start, end, period, use_intervals, original_period)
    }
    cropping <- cropping_info$cropping
    start <- cropping_info$start
    end <- cropping_info$end

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

.agg_remove_values_outside_intervals <- function(item, use_intervals) {
    interval_function <- function(interval) {
        lubridate::`%within%`(item$datetime, interval)
    }
    mask <- purrr::reduce(purrr::map(use_intervals, interval_function), `|`)
    item$datetime <- item$datetime[mask]
    sensor_function <- function(sensor) {
        sensor$values <- sensor$values[mask]
        sensor
    }
    item$sensors <- purrr::map(item$sensors, sensor_function)
    item
}

.agg_get_cropping_info_by_period_and_original_step <- function(start, end, period, original_period) {
    cropping <- FALSE
    original_step_period <- lubridate::as.period(original_period)
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

.agg_get_cropping_info_by_intervals <- function(start, end, period, use_intervals, original_period) {
    if(period == .agg_const_PERIOD_ALL) {
        return(list(start=start, end=end, cropping=FALSE))
    }
    cropping <- FALSE
    mask_interval_start <- lubridate::`%within%`(start, use_intervals)
    start_index <- (1:length(use_intervals))[mask_interval_start]
    mask_interval_end <- lubridate::`%within%`(end, use_intervals)
    end_index <- (seq_along(use_intervals))[mask_interval_end]
    original_step_period <- lubridate::as.period(original_period)
    if(lubridate::`%within%`(start - original_step_period, use_intervals[start_index])) {
        cropping <- TRUE
        if(start_index < length(use_intervals)) {
            start <- lubridate::int_start(use_intervals[[start_index + 1]])
        } else {
            start <- lubridate::int_end(use_intervals[[start_index]]) + lubridate::seconds(1)
        }
    }
    if(lubridate::`%within%`(end + original_step_period, use_intervals[end_index])) {
        cropping <- TRUE
        if(end_index > 1) {
            end <- lubridate::int_end(use_intervals[[end_index - 1]]) + lubridate::seconds(1)
        } else {
            end <- lubridate::int_start(use_intervals[[end_index]]) + lubridate::seconds(1)
        }
    }

    list(start=start, end=end, cropping=cropping)
}

.agg_extend_item_to_all_interval <- function(item, use_intervals, original_period) {
    start <- lubridate::ceiling_date(lubridate::int_start(use_intervals), original_period)
    end <- lubridate::floor_date(lubridate::int_end(use_intervals), original_period)
    if(start == dplyr::first(item$datetime) && end == dplyr::last(item$datetime)) {
        return(item)
    }
    datetime <- seq(start, end, by=original_period)
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

.agg_get_functions <- function(sensor, fun, percentiles, na.rm, custom_functions) {
    if(class(fun) == "character") {
        functions_to_convert <- fun
    } else if (sensor$metadata@name %in% names(fun)) {
        functions_to_convert <- fun[[sensor$metadata@name]]
    } else {
        return(NULL)
    }
    value_type <- mc_data_sensors[[sensor$metadata@sensor_id]]@value_type
    purrr::flatten(purrr::map(functions_to_convert, function(x) .agg_convert_function(x, percentiles, na.rm, value_type, custom_functions)))
}

.agg_convert_function <- function(function_text, percentiles, na.rm, value_type, custom_functions) {
    if(function_text == .agg_const_FUNCTION_MIN) {
        return(list(min=function(x) {
            x <- .agg_function_prepare_data(x, na.rm)
            if(length(x) == 0) return(NA)
            .agg_function_convert_result(min(x), value_type)
        }))
    } else if(function_text == .agg_const_FUNCTION_MAX) {
        return(list(max=function(x){
            x <- .agg_function_prepare_data(x, na.rm)
            if(length(x) == 0) return(NA)
            .agg_function_convert_result(max(x), value_type)
        }))
    } else if(function_text == .agg_const_FUNCTION_MEAN) {
        return(list(mean=function(x) {
            x <- .agg_function_prepare_data(x, na.rm)
            if(length(x) == 0) return(NA)
            .agg_function_convert_result(mean(x), value_type)
        }))
    } else if(function_text == .agg_const_FUNCTION_PERCENTILE) {
        return(.agg_convert_percentile_functions(percentiles, na.rm, value_type))
    } else if(function_text == .agg_const_FUNCTION_SUM) {
        return(list(sum=function(x) {
            x <- .agg_function_prepare_data(x, na.rm)
            if(length(x) == 0) return(NA)
            if(value_type == myClim:::.model_const_VALUE_TYPE_LOGICAL) {
                value_type <- myClim:::.model_const_VALUE_TYPE_INTEGER
            }
            .agg_function_convert_result(sum(x), value_type)
        }))
    } else if(function_text == .agg_const_FUNCTION_RANGE) {
        return(list(range=function(x) {
            x <- .agg_function_prepare_data(x, na.rm)
            if(length(x) == 0) return(NA)
            .agg_function_convert_result(max(x) - min(x), value_type)
        }))
    } else if(function_text == .agg_const_FUNCTION_COUNT) {
        return(list(count=function(x) length(x[!is.na(x)])))
    } else if(function_text == .agg_const_FUNCTION_COVERAGE) {
        return(list(coverage=function(x) {
            if(length(x) == 0) return(NA)
            length(x[!is.na(x)]) / length(x)
        }))
    } else if(function_text %in% names(custom_functions)) {
        result <- list()
        result[[function_text]] <- function (x) {
            x <- .agg_function_prepare_data(x, na.rm)
            if(length(x) == 0) return(NA)
            custom_functions[[function_text]] (x)
        }
        return(result)
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
    names(result) <- purrr::map_chr(percentiles, .agg_get_percentile_function_name)
    result
}

.agg_get_percentile_function_name <- function(value) {
    stringr::str_glue("{.agg_const_FUNCTION_PERCENTILE}{value}")
}

.agg_agregate_sensor <- function(sensor, functions, by_aggregate, custom_functions) {
    sensor_function <- function(.x, .y) {
        sensor_info <- mc_data_sensors[[sensor$metadata@sensor_id]]
        new_sensor <- sensor
        if(.y %in% c(.agg_const_FUNCTION_COUNT, .agg_const_FUNCTION_COVERAGE)) {
            new_sensor$metadata@sensor_id <- .y
        }
        if(sensor_info@value_type == myClim:::.model_const_VALUE_TYPE_LOGICAL &&
            .y == .agg_const_FUNCTION_SUM) {
            new_sensor$metadata@sensor_id <- myClim:::.model_const_SENSOR_integer
        }
        new_sensor$metadata@name <- .agg_get_aggregated_sensor_name(new_sensor$metadata@name, .y)
        new_sensor$values <- aggregate(new_sensor$values, by_aggregate, .x)$x
        if(.y %in% names(custom_functions)) {
            if(is.logical(new_sensor$values)) {
                new_sensor$metadata@sensor_id <- myClim:::.model_const_SENSOR_logical
            } else if(is.integer(new_sensor$values)) {
                new_sensor$metadata@sensor_id <- myClim:::.model_const_SENSOR_integer
            } else if(is.numeric(new_sensor$values)) {
                new_sensor$metadata@sensor_id <- myClim:::.model_const_SENSOR_real
            } else {
                stop(stringr::str_glue(.agg_const_MESSAGE_WRONG_CUSTOM_FUNCTION))
            }
        }
        new_sensor
    }
    result <- purrr::imap(functions, sensor_function)
    names(result) <- purrr::map_chr(result, function(x) x$metadata@name)
    result
}

.agg_get_aggregated_sensor_name <- function(name, function_name) {
    stringr::str_glue("{name}_{function_name}")
}

.agg_get_height_name <- function(name, height) {
    if(is.na(height)) {
        warning(stringr::str_glue(.agg_const_MESSAGE_MISSING_HEIGHT))
        return(name)
    }
    height <- stringr::str_replace_all(height, "[-]", "_")
    make.names(height)
}
