library(testthat)
library(microclim)

source("test.R")

test_that("mc_calc_snow", {
    data <- mc_read_directory("data/eco-snow", "TOMST")
    cleaned_data <- mc_prep_clean(data, silent=T)
    expect_error(mc_calc_snow(cleaned_data, "TMS_T3", output_sensor="T3_snow", dr=1.5, tmax=0.5))
    calc_data <- mc_agg(cleaned_data)
    calc_data <- mc_calc_snow(calc_data, "TMS_T3", output_sensor="T3_snow", dr=1.5, tmax=0.5)
    expect_true(is.na(calc_data$localities[["94184102"]]$sensors$T3_snow$values[[1]]))
    expect_false(is.na(calc_data$localities[["94184102"]]$sensors$T3_snow$values[[4 * 24 + 1]]))
})

test_that("mc_calc_snow long period", {
    data <- mc_read_directory("data/eco-snow", "TOMST")
    cleaned_data <- mc_prep_clean(data, silent=T)
    expect_warning(calc_data <- mc_agg(cleaned_data, "mean", "week"))
    expect_error(calc_data <- mc_calc_snow(calc_data, "TMS_T3_mean", output_sensor="T3_snow", dr=1.5, tmax=0.5))
})

test_that("mc_calc_snow_logger_without_sensor", {
    expect_warning(data <- mc_read_directory("data/TOMST", "TOMST"))
    cleaned_data <- mc_prep_clean(data, silent=T)
    calc_data <- mc_agg(cleaned_data)
    calc_data <- mc_calc_snow(calc_data, "TMS_T3", output_sensor="T3_snow", dr=1.5, tmax=0.5)
    expect_equal(length(calc_data$localities[["91184101"]]$sensors), 1)
})

test_that("mc_calc_snow_agg", {
    data <- mc_read_directory("data/eco-snow", "TOMST")
    cleaned_data <- mc_prep_clean(data, silent=T)
    cleaned_data <- mc_prep_user_tz(cleaned_data, list(`94184102`=60, `94184103`=60))
    calc_data <- mc_agg(cleaned_data)
    calc_data <- mc_calc_snow(calc_data, "TMS_T3", dr=1.5, tmax=0.5)
    snow_agg <- mc_calc_snow_agg(calc_data, "snow")
    expect_equal(colnames(snow_agg), c("locality_id", "snow_days", "first_day", "last_day", "first_day_period", "last_day_period"))
    expect_equal(nrow(snow_agg), 2)
    expect_equal(snow_agg[1, 3], as.Date("2021-01-07"))
    expect_equal(snow_agg[1, 4], as.Date("2021-01-30"))
    expect_equal(snow_agg[1, 5], as.Date("2021-01-07"))
    expect_equal(snow_agg[1, 6], as.Date("2021-01-30"))
    expect_equal(snow_agg[2, 3], as.Date("2021-01-24"))
    expect_equal(snow_agg[2, 4], as.Date("2021-01-25"))
    expect_true(is.na(snow_agg[2, 5]))
})


