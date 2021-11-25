library(testthat)
library(microclim)
source("test.R")

test_that("mc_clean_datetime_step", {
    data <- mc_feed_TOMST_directory("data/clean-datetime_step")
    cleaned_data <- mc_clean_datetime_step(data)
    test_standard_data_format(cleaned_data)
    expect_true("data contains 1x duplicits" %in% cleaned_data$None$loggers[[1]]$clean_log[[mc_const_CLEAN_DATETIME_STEP]])
    expect_true("data contains 1x 45 min gap" %in% cleaned_data$None$loggers[[1]]$clean_log[[mc_const_CLEAN_DATETIME_STEP]])
    test_function <- if(exists(".clean_is_logger_datetime_step_proceessed")) .clean_is_logger_datetime_step_proceessed else microclim:::.clean_is_logger_datetime_step_proceessed
    expect_true(test_function(cleaned_data$None$loggers[[1]]))
    expect_true(microclim:::.clean_was_error_in_logger_datetime_step(cleaned_data$None$loggers[[1]]))
    expect_equal(length(cleaned_data$None$loggers[[1]]$datetime), 49)
    expect_true(is.na(cleaned_data$None$loggers[[1]]$sensors$TMS_T1$values[[19]]))
})

test_that("mc_clean_datetime_step_ok", {
    data <- mc_feed_TOMST_files("data/TOMST/data_94184102_0.csv")
    cleaned_data <- mc_clean_datetime_step(data)
    expect_true(microclim:::.clean_is_logger_datetime_step_proceessed(cleaned_data$None$loggers[[1]]))
    expect_false(microclim:::.clean_was_error_in_logger_datetime_step(cleaned_data$None$loggers[[1]]))
})

test_that("mc_clean_logs", {
    data <- mc_feed_TOMST_directory("data/clean-datetime_step")
    cleaned_data <- mc_clean_datetime_step(data)
    logs <- mc_clean_logs(cleaned_data)
    expect_equal(colnames(logs), c("locality_id", "serial_number", "clean_type", "message"))
})

test_that("mc_clean_solar_tz", {
    data <- mc_feed_from_csv("data/TOMST/files_table.csv", "data/TOMST/localities_table.csv")
    data <- mc_clean_solar_tz(data)
    test_standard_data_format(data)
    expect_equal(data$A1E05$metadata@tz_offset, 57)
})

test_that("clean_add_log", {
    logger <- list()
    logger$clean_log <- list()
    step <- 15
    logger$clean_log <- microclim:::.clean_add_log(logger$clean_log, mc_const_CLEAN_DATETIME_STEP,
                                                   as.character(stringr::str_glue("first message")))
    logger$clean_log <- microclim:::.clean_add_log(logger$clean_log, mc_const_CLEAN_DATETIME_STEP,
                                                   "other message")
    expect_equal(length(logger$clean_log), 1)
    expect_equal(length(logger$clean_log[[mc_const_CLEAN_DATETIME_STEP]]), 2)
})

test_that("mc_clean_crop", {
    data <- mc_feed_from_csv("data/TOMST/files_table.csv", "data/TOMST/localities_table.csv")
    data <- mc_clean_crop(data, start=as.POSIXct("2020-10-16 08:00", tz="UTC"))
    test_standard_data_format(data)
    expect_equal(length(data$A2E32$loggers[[1]]$datetime), 68)
    expect_equal(length(data$A2E32$loggers[[1]]$sensors$TMS_T1$values), 68)
    expect_equal(length(data$A2E32$loggers[[1]]$clean_log[[mc_const_CLEAN_CROP]]), 1)
})

