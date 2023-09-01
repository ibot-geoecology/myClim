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

.agg_const_MESSAGE_LOCALITY_WITHOUT_DATA <- "Locality {locality$metadata@locality_id} without any valid data. It was removed."
.agg_const_MESSAGE_CUSTOM_START_NULL <- "Parameter custom_start must be set."
.agg_const_MESSAGE_CUSTOM_WRONG_FORMAT <- "Prameter {parameter_name} is in wrong format. Required format is 'mm-dd' or 'mm-dd H:MM'."
.agg_const_MESSAGE_EMPTY_DATA <- "Data are empty."
.agg_const_MESSAGE_WRONG_PREVIOUS_PERIOD <- "It is not possible to aggregate all or custom data."
.agg_const_MESSAGE_WRONG_SHIFT <- "Shift of time-series in {locality$metadata@locality_id} locality is different."
.agg_const_MESSAGE_MISSING_HEIGHT <- "Height is missing in sensosr {object@name}."
.agg_const_MESSAGE_WRONG_CUSTOM_FUNCTION <- "Type of values in sensor {new_sensor$metadata@name} is wrong."
.agg_const_MESSAGE_FUN_AND_PERIOD <- "Parameters 'fun' and 'period' must be both NULL or must be both specified."
.agg_const_MESSAGE_SHORT_PERIOD <- "Period cannot be shorter then 1s."
.agg_const_MESSAGE_NONUTC_SHORT_PERIOD <- "Non-UTC time zone can be used only for period day and longer."

#' Aggregate data by function
#'
#' mc_agg has two basic uses: 
#' * aggregate (upscale) time step of microclimatic records with specified function (e. g. 15 min records to daily mean); 
#' * convert myClim object from Raw-format to Agg-format see [myClim-package] without time-series modification,
#' this behavior appears when `fun=NULL`, `period=NULL`.
#' 
#' @details 
#' Any output of mc_agg is in Agg-format. That means the 
#' hierarchical level of logger is removed (Locality<-Logger<-Sensor<-Record), and all microclimatic records within
#' the sensors are on the level of locality (Locality<-Sensor<-Record). See [myClim-package].
#' 
#' In case `mc_agg()` is used only for conversion from Raw-format to Agg-format (`fun=NULL, period=NULL`) then microclimatic
#' records are not modified. Equal step in all sensors is required for conversion from Raw-format to Agg-format, otherwise
#' period must be specified.
#' 
#' When fun and period are specified, microclimatic records are aggregated based on function into new period.
#' Aggregated time step is named after the first time step of selected period i.e. day = c(2022-12-29 00:00, 2022-12-30 00:00...);
#' week = c(2022-12-19 00:00, 2022-12-28 00:00...); month = c(2022-11-01 00:00, 2022-12-01 00:00...);
#' year = c(2021-01-01 00:00, 2022-01-01 00:00...).
#' When first or last period is incomplete in original data, the incomplete part is extended with NA values to match specified period. 
#' I.e. when you want to aggregate to monthly mean, but your time-series starts on January 15 ending December 20, 
#' myClim will extend the time.series 
#' to start January 1, ending December 31. Then you can decide what to do with the NAs, see parameter `min_coverage`. 
#' 
#' Empty sensors with no records are excluded. `mc_agg()` return NA for empty vector except from `fun=count` which returns 0.
#' When aggregation functions are provided as vector or list e.g. c(mean, min, max), than they are all applied to all the sensors
#' and multiple results are returned from each sensors. When named list (names are the sensor ids) of functions is provided then `mc_agg()`
#' apply specific functions to the specific sensors based on the named list `list(TMS_T1=c("max", "min"), TMS_T2="mean")`.

#' mc_agg returns new sensors on the localities putting aggregation 
#' function in its name (TMS_T1 -> TMS_T1_max), despite sensor names contains aggregation 
#' function, sensor_id stays the same as before aggregation in sensor metadata (e.g. TMS_T1 -> TMS_T1).
#' Sensors created with functions `min`, `max`, `mean`, `percentile`, `sum`, `range` 
#' keeps identical sensor_id and value_type as original input sensors. 
#' When function `sum` is applied on `logical` sensor (e.g. snow as TRUE, FALSE) the 
#' output is  `integer` i.e. number of `TRUE` values.
#'
#' Sensors created with functions `count` has sensor_id `count` and value_type `integer`,
#' function  `coverage` has sensor_id `coverage` and value_type `real`
#' 
#'
#' @param data cleaned myClim object in Raw-format: output of [myClim::mc_prep_clean()] or Agg-format as it is allowed to aggregate data multiple times.
#' @param fun aggregation function; one of (`"min"`, `"max"`, `"mean"`, `"percentile"`, `"sum"`, `"range"`, `"count"`, `"coverage"`)
#' and functions defined in `custom_functions`. See details of custom_functions argument.
#' Can be single function name, character vector of function names or named list of vector function names.
#' Named list of functions allows apply different function(s) to different sensors e.g. `list(TMS_T1=c("max", "min"), TMS_T2="mean", TMS_T3_GDD="sum")`
#' if NULL records are not aggregated, but myClim object is only converted to Agg-format without modifing time-series. See details.
#'
#' @param period Time period for aggregation - same as breaks in cut.POSIXt, e.g. (`"hour"`, `"day"`, `"month"`); if NULL then no aggregation
#'
#' There are special periods `"all"` and `"custom"`. Period `"all"` returning single value for each sensor based
#' on function applied across all records within the sensor.
#' Period `"custom"` aggregates data in yearly cycle. You can aggregate e.g. water year, vegetation season etc. by providing start, end datetime.  
#' See `custom_start` and `custom_end` parameters. The output of special periods `"all"` and `"custom"`are not allowed to be aggregated 
#' again in [myClim::mc_agg()] function, regardless multiple aggregations are allowed in general. 
#'
#' Start day of week is Monday.
#' @param use_utc default TRUE using UTC time, if set FALSE, the time is shifted by offset if available in locality metadata.
#' Shift can be e.g. to solar time [myClim::mc_prep_solar_tz()] or political time with custom offset [myClim::mc_prep_meta_locality()]).
#' Non-UTC time can by used only for aggregation of the data with period shorter than `day` (seconds, minutes, hours) into period `day` and longer. 
#' @param percentiles vector of percentile numbers; numbers are from range 0-100; each specified percentile number generate new virtual sensor, see details
#' @param min_coverage value from range 0-1 (default 1); the threshold specifying how many missing values can you accept within aggregation period. 
#' e.g. when aggregating from 15 min to monthly mean and set `min_coverage=1` then a single NA value within the specific month cause monthly mean = NA.
#' When `min_coverage=0.9` then you will get your monthly mean in case there are no more than 10 % missing values, if there were more than 10% you will get NA. 
#' Ignored for functions `count` and `coverage`
#' @param custom_start date of start, only use for `custom` period (default NULL); Character in format `"mm-dd"` or `"mm-dd H:MM"` recycled in yearly cycle for time-series longer than 1 year. 
#' @param custom_end date of end only use for `custom` period (default NULL); If NULL then calculates in year cycle ending on `custom_start` next year. (useful e.g. for hydrological year) 
#' When custom_end is provided, then data out of range `custom_start`-`custom_end` are ignored.  
#' Character in format `"mm-dd"` or `"mm-dd H:MM"`. `custom_end` row (the last record) is not included. I.e.complete daily data from year 2020 ends in 2021-01-01 `custom_end="01-01"`.
#' @param custom_functions user define one or more functions in format `list(function_name=function(values){...})`; then you will feed function_name(s) 
#' you defined to the `fun` parameter. e.g. `custom_functions = list(positive_count=function(x){length(x[x>0])})`,
#' `fun="positive_count"`,
#' @return Returns new myClim object in Agg-format see [myClim-package] When fun=NULL, period=NULL
#' records are not modified but only converted to Agg-format.
#' When fun and period are provided then time step is aggregated based on function.
#' @export
#' @examples
#' \donttest{hour_data <- mc_agg(mc_data_example_clean, c("min", "max", "percentile"),
#'                               "hour", percentiles = 50, min_coverage=0.5)}
#' \donttest{day_data <- mc_agg(mc_data_example_clean, list(TMS_T1=c("max", "min"), TMS_T2="mean"),
#'                              "day", min_coverage=1)}
#' \donttest{month_data <- mc_agg(mc_data_example_clean, fun=list(TMS_T3="below5"),period = "month",
#'                                custom_functions = list(below5=function(x){length(x[x<(-5)])}))}
mc_agg <- function(data, fun=NULL, period=NULL, use_utc=TRUE, percentiles=NULL, min_coverage=1,
                   custom_start=NULL, custom_end=NULL, custom_functions=NULL) {
    old_lubridate_week_start <- getOption("lubridate.week.start")
    on.exit(options(lubridate.week.start = old_lubridate_week_start))
    options(lubridate.week.start = 1)
    use_intervals <- .agg_get_use_intervals(data, period, custom_start, custom_end)
    period_object <- .agg_get_period_object(use_intervals, period)
    .agg_check_fun_period(fun, period_object, use_utc)
    use_utc <- .agg_get_use_utc(data, use_utc)
    original_period <- .agg_check_steps_and_get_original_text(data, fun, period_object)
    is_raw <- .common_is_raw_format(data)
    locality_function <- function (locality) {
        tz_offset <- if(use_utc) 0 else locality$metadata@tz_offset
        if(is_raw) {
            return(.agg_aggregate_prep_locality(locality, fun, period, use_intervals, percentiles, min_coverage, tz_offset, custom_functions))
        } else {
            return(.agg_aggregate_item(locality, fun, period, use_intervals, percentiles, min_coverage, tz_offset, original_period, custom_functions))
        }
    }
    new_localities <- purrr::map(data$localities, locality_function)
    new_localities <- purrr::keep(new_localities, function (x) !is.null(x))
    if(length(new_localities) == 0) {
        stop(.agg_const_MESSAGE_EMPTY_DATA)
    }

    intervals_start <- lubridate::NA_POSIXct_
    intervals_end <- lubridate::NA_POSIXct_
    if(is.null(period)) {
        number_of_seconds <- as.numeric(lubridate::as.period(original_period))
        step <- as.integer(number_of_seconds)
        period <- .agg_get_period_text_from_step(step)
    } else if(!is.null(use_intervals)) {
        step <- .agg_get_step_from_use_intervals(use_intervals)
        intervals_start <- lubridate::int_start(use_intervals)
        intervals_end <- lubridate::int_end(use_intervals)
    } else {
        step <- .agg_get_step_from_period_object(period_object)
    }
    metadata <- new("mc_MainMetadataAgg")
    metadata@period <- period
    metadata@step <- step
    metadata@intervals_start <- intervals_start
    metadata@intervals_end <- intervals_end
    myClimList(metadata, new_localities)
}

.agg_get_use_intervals <- function(data, period, custom_start, custom_end) {
    result <- NULL
    if(!is.null(period) && period %in% .agg_const_INTERVAL_PERIODS) {
        result <- .common_get_cleaned_data_range(data, add_step_to_end = TRUE)
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
        stop(.agg_const_MESSAGE_FUN_AND_PERIOD)
    }
    if(as.numeric(period_object[[1]]) < 1) {
        stop(.agg_const_MESSAGE_SHORT_PERIOD)
    }
    if(!use_utc && period_object[[1]]@year == 0 && period_object[[1]]@month == 0 && period_object[[1]]@day == 0) {
        stop(.agg_const_MESSAGE_NONUTC_SHORT_PERIOD)
    }
}

.agg_get_use_utc <- function(data, use_utc) {
    is_agg <- .common_is_agg_format(data)
    if(!use_utc && is_agg && (is.na(data$metadata@step) || data$metadata@step >= 60*60*24)) {
        use_utc <- TRUE
    }
    if(!use_utc) {
        .prep_warn_if_unset_tz_offset(data)
    }
    use_utc
}

.agg_check_steps_and_get_original_text <- function(data, fun, period_object) {
    if(.common_is_agg_format(data)) {
        if(data$metadata@period %in% .agg_const_INTERVAL_PERIODS) {
            stop(.agg_const_MESSAGE_WRONG_PREVIOUS_PERIOD)
        }
        return(data$metadata@period)
    }
    step_locality_function <- function(locality) {
        purrr::map_int(locality$loggers, function(.x) as.integer(.x$clean_info@step))
    }
    steps <- as.numeric(purrr::flatten(purrr::map(data$localities, step_locality_function)))
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
        loggers <- purrr::keep(locality$loggers, ~ length(.x$datetime) > 0)
        shifts <- purrr::map_int(loggers, function(.x) as.integer(.x$datetime[[1]]) %% as.integer(.x$clean_info@step))
        if(length(shifts) > 1 && var(shifts) != 0) {
            stop(stringr::str_glue(.agg_const_MESSAGE_WRONG_SHIFT))
        }
    }
    purrr::walk(data$localities, shift_locality_function)
    .agg_get_period_text_from_step(dplyr::first(steps))
}

.agg_get_period_text_from_step <- function(step) {
    if(step %% 60 != 0) {
        return(stringr::str_glue("{step} sec"))
    }
    return(stringr::str_glue("{step %/% 60} min"))
}

.agg_aggregate_prep_locality <- function(locality, fun, period, use_intervals, percentiles, min_coverage, tz_offset, custom_functions)
{
    logger_function <- function (logger) {
        original_period <- .agg_get_period_text_from_step(logger$clean_info@step)
        logger <- .agg_aggregate_item(logger, fun, period, use_intervals, percentiles, min_coverage, tz_offset, original_period, custom_functions)
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
        period <- .agg_get_period_text_from_step(locality$loggers[[1]]$clean_info@step)
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
    as.integer(as.numeric(period_object))
}

.agg_aggregate_item <- function(item, fun, period, use_intervals, percentiles, min_coverage, tz_offset, original_period,
                                custom_functions)
{
    if(is.null(fun) || length(item$datetime) == 0) {
        return(item)
    }
    output_period <- lubridate::as.period(if(is.null(use_intervals)) period else use_intervals[[1]])
    if(output_period < lubridate::as.period(original_period)) {
        stop("It isn't possible aggregate from longer period to shorter one.")
    }
    item$datetime <- .calc_get_datetimes_with_offset(item$datetime, tz_offset)
    if(is.null(use_intervals)) {
        item <- .agg_extend_item_by_period(item, period, original_period)
        start_datetimes <- lubridate::floor_date(item$datetime, period)
    } else {
        item <- .agg_extend_item_use_intervals(item, use_intervals, original_period)
        interval_function <- function(interval) {
            count <- sum(lubridate::`%within%`(item$datetime, interval))
            rep(lubridate::int_start(interval), count)
        }
        start_datetimes <- .common_as_utc_posixct(unlist(purrr::map(use_intervals, interval_function)))
    }
    item$datetime <- unique(start_datetimes)
    by_aggregate <- list(step=as.factor(start_datetimes))
    sensor_function <- function(sensor) {
        functions <- .agg_get_functions(sensor, fun, percentiles, min_coverage, custom_functions)
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
    min_datetime <- .common_as_utc_posixct(min(purrr::map_int(items, min_datetime_function), na.rm=TRUE))
    max_datetime_function <- function(.x) {
        if(length(.x$datetime) == 0) return(NA_integer_)
        as.integer(tail(.x$datetime, n=1))}
    max_datetime <- .common_as_utc_posixct(max(purrr::map_int(items, max_datetime_function), na.rm=TRUE))
    seq(min_datetime, max_datetime, by=period)
}

.agg_get_merged_sensors <- function(datetime, sensor_items) {
    sensor_items <- .agg_get_items_with_renamed_sensors(sensor_items)
    tables <- c(list(tibble::tibble(datetime=datetime)), purrr::map(sensor_items, .common_sensor_values_as_tibble))
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

.agg_extend_item_use_intervals <- function(item, use_intervals, original_period) {
    intervals_with_data <- use_intervals[lubridate::int_start(use_intervals) <= dplyr::last(item$datetime)]
    intervals_with_data <- intervals_with_data[lubridate::int_end(intervals_with_data) >= dplyr::first(item$datetime)]
    first_interval <- dplyr::first(intervals_with_data)
    last_interval <- dplyr::last(intervals_with_data)
    start <- lubridate::int_start(first_interval)
    end <- lubridate::int_end(last_interval)
    item <- .agg_extend_item(item, start, end, original_period)
    item <- .agg_remove_values_outside_intervals(item, intervals_with_data)
    if(length(item$datetime) == 0)
    {
        return(NULL)
    }
    return(item)
}

.agg_extend_item_by_period <- function(item, period, original_period) {
    start <- lubridate::floor_date(dplyr::first(item$datetime), period)
    last_period <- lubridate::floor_date(dplyr::last(item$datetime), period)
    end <- last_period + lubridate::as.period(period) - lubridate::seconds(1)
    item <- .agg_extend_item(item, start, end, original_period)
    return(item)
}

.agg_extend_item <- function(item, start, end, original_period) {
    original_step_period <- lubridate::as.period(original_period)
    first_datetime <- dplyr::first(item$datetime)
    from_start_interval <- lubridate::interval(start, first_datetime)
    missed_modulo <- as.numeric(from_start_interval) %% as.numeric(original_step_period)
    if(missed_modulo != 0) {
        count_missing <- as.numeric(from_start_interval) %/% as.numeric(original_step_period)
        start <- first_datetime - count_missing * original_step_period
    }
    last_datetime <- dplyr::last(item$datetime)
    to_end_interval <- lubridate::interval(last_datetime, end)
    missed_modulo <- as.numeric(to_end_interval) %% as.numeric(original_step_period)
    if(missed_modulo != 0) {
        count_missing <- as.numeric(to_end_interval) %/% as.numeric(original_step_period)
        end <- last_datetime + count_missing * original_step_period
    }
    if(start == dplyr::first(item$datetime) && end == dplyr::last(item$datetime)) {
        return(item)
    }
    datetime <- seq(start, end, by=original_period)
    datetime_table <- tibble::as_tibble(datetime)
    colnames(datetime_table) <- "datetime"
    item_table <- .common_sensor_values_as_tibble(item)
    result_table <- dplyr::left_join(datetime_table, item_table, by="datetime")
    item$datetime <- result_table$datetime
    sensor_function <- function(sensor){
        sensor$values <- result_table[[sensor$metadata@name]]
        sensor
    }
    item$sensors <- purrr::map(item$sensors, sensor_function)
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

.agg_get_functions <- function(sensor, fun, percentiles, min_coverage, custom_functions) {
    if(is(fun, "character")) {
        functions_to_convert <- fun
    } else if (sensor$metadata@name %in% names(fun)) {
        functions_to_convert <- fun[[sensor$metadata@name]]
    } else {
        return(NULL)
    }
    value_type <- myClim::mc_data_sensors[[sensor$metadata@sensor_id]]@value_type
    purrr::flatten(purrr::map(functions_to_convert, function(x) .agg_convert_function(x, percentiles, min_coverage, value_type, custom_functions)))
}

.agg_convert_function <- function(function_text, percentiles, min_coverage, value_type, custom_functions) {
    if(function_text == .agg_const_FUNCTION_MIN) {
        return(list(min=function(x) {
            x <- .agg_function_prepare_data(x, min_coverage)
            if(length(x) == 0) return(NA)
            .agg_function_convert_result(min(x), value_type)
        }))
    } else if(function_text == .agg_const_FUNCTION_MAX) {
        return(list(max=function(x){
            x <- .agg_function_prepare_data(x, min_coverage)
            if(length(x) == 0) return(NA)
            .agg_function_convert_result(max(x), value_type)
        }))
    } else if(function_text == .agg_const_FUNCTION_MEAN) {
        return(list(mean=function(x) {
            x <- .agg_function_prepare_data(x, min_coverage)
            if(length(x) == 0) return(NA)
            .agg_function_convert_result(mean(x), value_type)
        }))
    } else if(function_text == .agg_const_FUNCTION_PERCENTILE) {
        return(.agg_convert_percentile_functions(percentiles, min_coverage, value_type))
    } else if(function_text == .agg_const_FUNCTION_SUM) {
        return(list(sum=function(x) {
            x <- .agg_function_prepare_data(x, min_coverage)
            if(length(x) == 0) return(NA)
            if(value_type == .model_const_VALUE_TYPE_LOGICAL) {
                value_type <- .model_const_VALUE_TYPE_INTEGER
            }
            .agg_function_convert_result(sum(x), value_type)
        }))
    } else if(function_text == .agg_const_FUNCTION_RANGE) {
        return(list(range=function(x) {
            x <- .agg_function_prepare_data(x, min_coverage)
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
            x <- .agg_function_prepare_data(x, min_coverage)
            if(length(x) == 0) return(NA)
            custom_functions[[function_text]] (x)
        }
        return(result)
    }
    NULL
}

.agg_function_prepare_data <- function(values, min_coverage) {
    if(min_coverage == 1 || length(values) == 0){
        return(values)
    }
    coverage <- length(values[!is.na(values)]) / length(values)
    if(coverage >= min_coverage){
        return(values[!is.na(values)])
    }
    return(values)
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

.agg_convert_percentile_functions <- function(percentiles, min_coverage, value_type) {
    percentile_function <- function(percentile) {
        quantile <- percentile / 100
        function(x) {
            x <- .agg_function_prepare_data(x, min_coverage)
            if(any(is.na(x))) {
                return(NA)
            }
            .agg_function_convert_result(unname(quantile(x, quantile, na.rm=FALSE)), value_type)
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
        sensor_info <- myClim::mc_data_sensors[[sensor$metadata@sensor_id]]
        new_sensor <- sensor
        if(.y %in% c(.agg_const_FUNCTION_COUNT, .agg_const_FUNCTION_COVERAGE)) {
            new_sensor$metadata@sensor_id <- .y
        }
        if(sensor_info@value_type == .model_const_VALUE_TYPE_LOGICAL &&
            .y == .agg_const_FUNCTION_SUM) {
            new_sensor$metadata@sensor_id <- mc_const_SENSOR_integer
        }
        new_sensor$metadata@name <- .agg_get_aggregated_sensor_name(new_sensor$metadata@name, .y)
        new_sensor$values <- aggregate(new_sensor$values, by_aggregate, .x)$x
        if(.y %in% names(custom_functions)) {
            if(is.logical(new_sensor$values)) {
                new_sensor$metadata@sensor_id <- mc_const_SENSOR_logical
            } else if(is.integer(new_sensor$values)) {
                new_sensor$metadata@sensor_id <- mc_const_SENSOR_integer
            } else if(is.numeric(new_sensor$values)) {
                new_sensor$metadata@sensor_id <- mc_const_SENSOR_real
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
    height <- stringr::str_replace_all(height, "[- ]", "_")
    make.names(height)
}
