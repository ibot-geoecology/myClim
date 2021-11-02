library(testthat)
library(microclim)


test_that("mc_eco_snow", {
    data <- mc_feed_TOMST_directory("data/eco-snow")
    cleaned_data <- mc_clean_datetime_step(data)
    snow <- mc_eco_snow(cleaned_data, "TMS_T3", dr=1.5, tmax=0.5)
    expect_equal(ncol(snow), 3)
    expect_equal(nrow(snow), 96+576)
    expect_true(is.na(snow[["94184102"]][[1]]))
    expect_false(is.na(snow[4 * 24 + 1, 3]))
    expect_equal(nrow(snow[!is.na(snow[["94184102"]]) & snow[["94184102"]] == TRUE, ]), 63)
})

test_that("mc_eco_snow_logger_without_sensor", {
    expect_warning(data <- mc_feed_TOMST_directory("data/TOMST"))
    cleaned_data <- mc_clean_datetime_step(data)
    snow <- mc_eco_snow(cleaned_data, "TMS_T3", dr=1.5, tmax=0.5)
    expect_equal(ncol(snow), 4)
    expect_true(all(is.na(snow[["94184103"]])))
})

test_that("mc_eco_snow_agg", {
    data <- mc_feed_TOMST_directory("data/eco-snow")
    cleaned_data <- mc_clean_datetime_step(data)
    snow <- mc_eco_snow(cleaned_data, "TMS_T3", dr=1.5, tmax=0.5)
    snow_agg <- mc_eco_snow_agg(snow)
    expect_equal(colnames(snow_agg), c("serial_number", "snow_days", "first_day", "last_day", "first_day_period", "last_day_period"))
    expect_equal(nrow(snow_agg), 2)
    expect_true(is.na(snow_agg[2, 3]))
})
