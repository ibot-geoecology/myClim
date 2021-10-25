library(testthat)
library(microclim)


test_that("mc_eco_snow", {
    data <- microclim::mc_feed_TMS_directory("data/eco-snow")
    snow <- mc_eco_snow(data, "T3", dr=1.5, tmax=0.5)
    expect_equal(ncol(snow), 3)
    expect_equal(nrow(snow), 96+576)
    expect_true(is.na(snow[1, 3]))
    expect_false(is.na(snow[4 * 24 + 1, 3]))
    expect_equal(nrow(snow[!is.na(snow[["94184102"]]) & snow[["94184102"]] == TRUE, ]), 63)
})

test_that("mc_eco_snow_agg", {
    data <- microclim::mc_feed_TMS_directory("data/eco-snow")
    snow <- microclim::mc_eco_snow(data, "T3", dr=1.5, tmax=0.5)
    snow_agg <- microclim::mc_eco_snow_agg(snow)
    expect_equal(colnames(snow_agg), c("serial_number", "snow_days", "first_day", "last_day", "first_day_period", "last_day_period"))
    expect_equal(nrow(snow_agg), 2)
    expect_true(is.na(snow_agg[2, 3]))
})
