library(purrr)
library(testthat)

test_standard_data_format <- function(data) {
    expect_equal(class(data), "list")
    walk(data, test_locality)
}

test_locality <- function(locality) {
    expect_equal(class(locality), "list")
    expect_equal(names(locality), c("metadata", "loggers"))
    expect_equal(class(locality$metadata)[[1]], "mc_LocalityMetadata")
    expect_equal(class(locality$loggers), "list")
    walk(locality$loggers, test_logger)
}

test_logger <- function(logger) {
    expect_equal(class(logger), "list")
    expect_equal(names(logger), c("metadata", "clean_log", "datetime", "sensors"))
    expect_equal(class(logger$metadata)[[1]], "mc_LoggerMetadata")
    expect_equal(class(logger$clean_log), "list")
    expect_equal(class(logger$datetime), c("POSIXct", "POSIXt"))
    expect_equal(class(logger$sensors), "list")
    expect_true(length(logger$sensors) > 0)
    test_data_length(logger)
    walk(logger$sensors, test_sensor)
}

test_data_length <- function(logger) {
    datetime_length <- length(logger$datetime)
    walk(logger$sensors, ~ {expect_equal(length(.x$values), datetime_length)})
}

test_sensor <- function(sensor) {
    expect_equal(class(sensor), "list")
    expect_equal(names(sensor), c("metadata", "values", "states"))
    expect_equal(class(sensor$metadata)[[1]], "mc_SensorMetadata")
    expect_equal(class(sensor$states), "list")
    expect_true(class(sensor$values) %in% c("integer", "numeric"))
}
