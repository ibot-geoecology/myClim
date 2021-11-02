library(testthat)
library(microclim)

test_that("mc_clean_datetime_step", {
    data <- mc_feed_TOMST_directory("data/clean-datetime_step")
    cleaned_data <- mc_clean_datetime_step(data)
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
