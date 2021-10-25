library(testthat)
library(microclim)

test_that("clean-log", {
    logger <- list()
    logger$clean_log <- list()
    step <- 15
    logger$clean_log <- microclim:::.clean_add_log(logger$clean_log, mc_const_CLEAN_DATETIME_STEP,
                                                   as.character(stringr::str_glue("detected step: {step} min")))
    logger$clean_log <- microclim:::.clean_add_log(logger$clean_log, mc_const_CLEAN_DATETIME_STEP,
                                                   "other message")
    expect_equal(length(logger$clean_log), 1)
    expect_equal(length(logger$clean_log[[mc_const_CLEAN_DATETIME_STEP]]), 2)
})

test_that("mc_clean_datetime_step", {
    data <- mc_feed_TMS_directory("data/clean-datetime_step")
    cleaned_data <- mc_clean_datetime_step(data)
    expect_true("data contains 1x duplicits" %in% cleaned_data$None$loggers[[1]]$clean_log[[mc_const_CLEAN_DATETIME_STEP]])
    expect_true("data contains 1x 45 min gap" %in% cleaned_data$None$loggers[[1]]$clean_log[[mc_const_CLEAN_DATETIME_STEP]])
    expect_true(microclim:::.clean_is_logger_datetime_step_proceessed(cleaned_data$None$loggers[[1]]))
    expect_true(microclim:::.clean_was_error_in_logger_datetime_step(cleaned_data$None$loggers[[1]]))
    expect_equal(length(cleaned_data$None$loggers[[1]]$datetime), 49)
    expect_true(is.na(cleaned_data$None$loggers[[1]]$sensors$T1$values[[19]]))
})

test_that("mc_clean_datetime_step_ok", {
    data <- mc_feed_TMS_files("data/TMS/data_94184102_0.csv")
    cleaned_data <- mc_clean_datetime_step(data)
    expect_true(microclim:::.clean_is_logger_datetime_step_proceessed(cleaned_data$None$loggers[[1]]))
    expect_false(microclim:::.clean_was_error_in_logger_datetime_step(cleaned_data$None$loggers[[1]]))
})
