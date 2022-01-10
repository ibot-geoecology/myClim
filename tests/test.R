library(testthat)
library(microclim)

test_prep_data_format <- function(data) {
    expect_equal(class(data), "list")
    purrr::walk(data, test_prep_locality)
}

test_calc_data_format <- function(data) {
    expect_equal(class(data), "list")
    expect_equal(names(data), c("metadata", "localities"))
    expect_equal(class(data$metadata)[[1]], "mc_MainMetadata")
    expect_equal(class(data$localities), "list")
    purrr::walk(data$localities, test_calc_locality)
}

test_prep_locality <- function(locality) {
    expect_equal(class(locality), "list")
    expect_equal(names(locality), c("metadata", "loggers"))
    expect_equal(class(locality$metadata)[[1]], "mc_LocalityMetadata")
    expect_equal(class(locality$loggers), "list")
    purrr::walk(locality$loggers, test_logger)
}

test_calc_locality <- function(locality) {
    expect_equal(class(locality), "list")
    expect_equal(names(locality), c("metadata", "datetime", "sensors"))
    expect_equal(class(locality$metadata)[[1]], "mc_LocalityMetadata")
    expect_equal(class(locality$datetime), c("POSIXct", "POSIXt"))
    expect_true(all(!is.na(locality$datetime)))
    expect_equal(class(locality$sensors), "list")
    test_data_length(locality)
    purrr::walk(locality$sensors, test_sensor)
}

test_logger <- function(logger) {
    expect_equal(class(logger), "list")
    expect_equal(names(logger), c("metadata", "clean_info", "datetime", "sensors"))
    expect_equal(class(logger$metadata)[[1]], "mc_LoggerMetadata")
    expect_equal(class(logger$clean_info)[[1]], "mc_LoggerCleanInfo")
    expect_equal(class(logger$datetime), c("POSIXct", "POSIXt"))
    expect_true(all(!is.na(logger$datetime)))
    expect_equal(class(logger$sensors), "list")
    expect_true(length(logger$sensors) > 0)
    test_data_length(logger)
    test_cleaning(logger)
    purrr::walk(logger$sensors, test_sensor)
}

test_data_length <- function(item) {
    datetime_length <- length(item$datetime)
    purrr::walk(item$sensors, ~ {expect_equal(length(.x$values), datetime_length)})
}

test_cleaning <- function(logger) {
    if(!microclim:::.prep_is_logger_cleaned(logger)) {
        return()
    }
    expect_equal(logger$clean_info@count_missed, length(purrr::keep(logger$sensors[[1]]$values, ~ is.na(.x))))
}

test_sensor <- function(sensor) {
    expect_equal(class(sensor), "list")
    expect_equal(names(sensor), c("metadata", "values", "states"))
    expect_equal(class(sensor$metadata)[[1]], "mc_SensorMetadata")
    expect_equal(class(sensor$states), "list")
    expect_true(is.numeric(sensor$values))
}

get_empty_prep_data <- function() {
    data <- mc_read_files("data/TOMST/data_94184102_0.csv", "TOMST")
    data <- mc_prep_clean(data, silent=T)
    data <- mc_prep_crop(data, end=as.POSIXct("2020-01-01", tz="UTC"))
    data
}

get_empty_calc_data <- function() {
    data <- get_empty_prep_data()
    data <- mc_agg(data)
    data
}
