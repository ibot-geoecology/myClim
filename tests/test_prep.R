library(testthat)
library(microclim)
source("test.R")

test_that("mc_prep_datetime_step", {
    data <- mc_read_TOMST_directory("data/clean-datetime_step")
    cleaned_data <- mc_prep_datetime_step(data)
    test_standard_data_format(cleaned_data)
    expect_true("1x duplicits" %in% cleaned_data$None$loggers[[1]]$clean_log[[mc_const_CLEAN_DATETIME_STEP]])
    expect_true("1x 45 min gap" %in% cleaned_data$None$loggers[[1]]$clean_log[[mc_const_CLEAN_DATETIME_STEP]])
    expect_true("25x duplicits" %in% cleaned_data$None$loggers[[2]]$clean_log[[mc_const_CLEAN_DATETIME_STEP]])
    expect_true("1x disordered" %in% cleaned_data$None$loggers[[3]]$clean_log[[mc_const_CLEAN_DATETIME_STEP]])
    diff_datetime <- diff(as.numeric(cleaned_data$None$loggers[[3]]$datetime)) %/% 60
    expect_equal(diff_datetime, rep(15, 5))
    diff_datetime <- diff(as.numeric(cleaned_data$None$loggers[[4]]$datetime)) %/% 60
    expect_equal(diff_datetime, rep(15, 5))
    test_function <- if(exists(".prep_is_logger_datetime_step_processed")) .prep_is_logger_datetime_step_processed else microclim:::.prep_is_logger_datetime_step_processed
    expect_true(test_function(cleaned_data$None$loggers[[1]]))
    expect_true(microclim:::.prep_was_error_in_logger_datetime_step(cleaned_data$None$loggers[[1]]))
    expect_equal(length(cleaned_data$None$loggers[[1]]$datetime), 49)
    expect_true(is.na(cleaned_data$None$loggers[[1]]$sensors$TMS_T1$values[[19]]))
})

test_that("mc_prep_datetime_step_ok", {
    data <- mc_read_TOMST_files("data/TOMST/data_94184102_0.csv")
    cleaned_data <- mc_prep_datetime_step(data)
    expect_true(microclim:::.prep_is_logger_datetime_step_processed(cleaned_data$None$loggers[[1]]))
    expect_false(microclim:::.prep_was_error_in_logger_datetime_step(cleaned_data$None$loggers[[1]]))
})

test_that("mc_prep_logs", {
    data <- mc_read_TOMST_directory("data/clean-datetime_step")
    cleaned_data <- mc_prep_datetime_step(data)
    logs <- mc_prep_logs(cleaned_data)
    expect_equal(colnames(logs), c("locality_id", "serial_number", "clean_type", "message"))
})

test_that("mc_prep_solar_tz", {
    data <- mc_read_from_csv("data/TOMST/files_table.csv", "data/TOMST/localities_table.csv")
    data <- mc_prep_solar_tz(data)
    test_standard_data_format(data)
    expect_equal(data$A1E05$metadata@tz_offset, 57)
})

test_that("mc_prep_user_tz", {
    data <- mc_read_from_csv("data/TOMST/files_table.csv", "data/TOMST/localities_table.csv")
    data <- mc_prep_user_tz(data, list(A1E05=50))
    test_standard_data_format(data)
    expect_equal(data$A1E05$metadata@tz_offset, 50)
})

test_that("clean_add_log", {
    logger <- list()
    logger$clean_log <- list()
    step <- 15
    logger$clean_log <- microclim:::.prep_add_log(logger$clean_log, mc_const_CLEAN_DATETIME_STEP,
                                                   as.character(stringr::str_glue("first message")))
    logger$clean_log <- microclim:::.prep_add_log(logger$clean_log, mc_const_CLEAN_DATETIME_STEP,
                                                   "other message")
    expect_equal(length(logger$clean_log), 1)
    expect_equal(length(logger$clean_log[[mc_const_CLEAN_DATETIME_STEP]]), 2)
})

test_that("mc_prep_crop", {
    data <- mc_read_from_csv("data/TOMST/files_table.csv", "data/TOMST/localities_table.csv")
    data <- mc_prep_crop(data, start=as.POSIXct("2020-10-16 08:00", tz="UTC"))
    test_standard_data_format(data)
    expect_equal(length(data$A2E32$loggers[[1]]$datetime), 68)
    expect_equal(length(data$A2E32$loggers[[1]]$sensors$TMS_T1$values), 68)
    expect_equal(length(data$A2E32$loggers[[1]]$clean_log[[mc_const_CLEAN_CROP]]), 1)
})

test_that(".prep_get_loggers_datetime_step_unprocessed", {
    data <- mc_read_from_csv("data/TOMST/files_table.csv", "data/TOMST/localities_table.csv")
    test_function <- if(exists(".prep_get_loggers_datetime_step_unprocessed")) .prep_get_loggers_datetime_step_unprocessed else microclim:::.prep_get_loggers_datetime_step_unprocessed
    expect_equal(test_function(data), c("91184101", "94184102", "94184103"))
    data_clean <- mc_prep_datetime_step(data)
    expect_equal(length(test_function(data_clean)), 0)
})

test_that(".prep_get_utc_localities", {
    expect_warning(data <- mc_read_TOMST_directory("data/TOMST"))
    test_function <- if(exists(".prep_get_utc_localities")) .prep_get_utc_localities else microclim:::.prep_get_utc_localities
    expect_equal(test_function(data), "None")
    data_clean <- mc_prep_user_tz(data, list(None=60))
    expect_equal(length(test_function(data_clean)), 0)
})

