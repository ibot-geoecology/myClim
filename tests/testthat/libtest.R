not_applicable_format_warning <- function(x) {
    expect_warning(x, regexp = ".* is not applicable format to .*\\. File is skipped.")
}

differnt_values_warning <- function(x) {
    expect_warning(x, regexp = "In logger .* are different values of .* in same time\\.")
}

test_raw_data_format <- function(data) {
    test_myClimList(data)
    purrr::walk(data$localities, test_raw_locality)
}

test_agg_data_format <- function(data) {
    test_myClimList(data)
    expect_true(is(data$metadata, "mc_MainMetadataAgg"))
    purrr::walk(data$localities, test_agg_locality)
}

test_myClimList <- function(data) {
    expect_equal(class(data), c("myClimList", "list"))
    expect_equal(names(data), c("metadata", "localities"))
    expect_true(is(data$metadata, "mc_MainMetadata"))
    expect_equal(class(data$localities), "list")
    expect_false(is.null(names(data$localities)))
}

test_raw_locality <- function(locality) {
    expect_equal(class(locality), "list")
    expect_equal(names(locality), c("metadata", "loggers"))
    expect_true(is(locality$metadata, "mc_LocalityMetadata"))
    expect_equal(class(locality$loggers), "list")
    purrr::walk(locality$loggers, test_logger)
}

test_agg_locality <- function(locality) {
    expect_equal(class(locality), "list")
    expect_equal(names(locality), c("metadata", "datetime", "sensors"))
    expect_true(is(locality$metadata, "mc_LocalityMetadata"))
    test_datetime(locality$datetime)
    expect_equal(class(locality$sensors), "list")
    test_data_length(locality)
    test_sensors(locality)
}

test_logger <- function(logger) {
    expect_equal(class(logger), "list")
    expect_equal(names(logger), c("metadata", "clean_info", "datetime", "sensors"))
    expect_true(is(logger$metadata, "mc_LoggerMetadata"))
    expect_true(is.na(logger$metadata@type) || logger$metadata@type %in% myClim:::.model_logger_types)
    expect_true(is(logger$clean_info, "mc_LoggerCleanInfo"))
    test_datetime(logger$datetime)
    expect_equal(class(logger$sensors), "list")
    expect_true(length(logger$sensors) > 0)
    test_data_length(logger)
    test_sensors(logger)
}

test_datetime <- function(datetime) {
    expect_equal(class(datetime), c("POSIXct", "POSIXt"))
    expect_true(all(!is.na(datetime)))
    expect_equal(attr(datetime, "tzone"), "UTC")
}

test_data_length <- function(item) {
    datetime_length <- length(item$datetime)
    purrr::walk(item$sensors, ~ {expect_equal(length(.x$values), datetime_length)})
}

test_sensors <- function(item) {
    sensor_names <- unname(purrr::map_chr(item$sensors, ~ .x$metadata@name))
    expect_equal(names(item$sensors), sensor_names)
    purrr::pwalk(list(sensor=item$sensors), test_sensor)
}

test_sensor <- function(sensor) {
    expect_equal(class(sensor), "list")
    expect_equal(names(sensor), c("metadata", "values", "calibration", "states"))
    expect_true(is(sensor$metadata, "mc_SensorMetadata"))
    expect_true(sensor$metadata@sensor_id %in% names(mc_data_sensors))
    expect_true(is.numeric(sensor$values) || is.logical(sensor$values) || all(is.na(sensor$values)))
    test_calibration(sensor)
    test_states(sensor)
}

test_calibration <- function(sensor) {
    expect_equal(class(sensor$calibration), "data.frame")
    if(nrow(sensor$calibration) == 0) {
        return()
    }
    expect_true(all(colnames(sensor$calibration) == c("datetime", "cor_factor", "cor_slope")))
    expect_false(any(is.na(sensor$calibration$datetime)))
}

test_states <- function (sensor) {
    expect_equal(class(sensor$states), "data.frame")
    if(nrow(sensor$states) == 0) {
        return()
    }
    expect_true(all(colnames(sensor$states) == c("tag", "start", "end", "value")))
    states <- dplyr::filter(sensor$states, .data$tag == myClim:::.model_const_SENSOR_STATE_SOURCE)
    if(length(sensor$values) == 0) {
        expect_equal(nrow(states), 0)
        return()
    }
}

get_empty_raw_data <- function() {
    data <- mc_read_files("../data/TOMST/data_94184102_0.csv", "TOMST", silent=T)
    data <- mc_prep_crop(data, end=as.POSIXct("2020-01-01", tz="UTC"))
    data
}

get_empty_agg_data <- function() {
    data <- get_empty_raw_data()
    data <- mc_agg(data)
    data
}
