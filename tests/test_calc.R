library(testthat)
library(microclim)

source("test.R")

test_that("mc_calc_snow", {
    data <- mc_read_directory("data/eco-snow", "TOMST")
    cleaned_data <- mc_prep_clean(data, silent=T)
    expect_error(mc_calc_snow(cleaned_data, "TMS_T3", output_sensor="T3_snow", dr=1.5, tmax=0.5))
    calc_data <- mc_prep_flat(cleaned_data)
    calc_data <- mc_calc_snow(calc_data, "TMS_T3", output_sensor="T3_snow", dr=1.5, tmax=0.5)
    expect_true(is.na(calc_data[["94184102"]]$sensors$T3_snow$values[[1]]))
    expect_false(is.na(calc_data[["94184102"]]$sensors$T3_snow$values[[4 * 24 + 1]]))
})

test_that("mc_calc_snow_logger_without_sensor", {
    expect_warning(data <- mc_read_directory("data/TOMST", "TOMST"))
    cleaned_data <- mc_prep_clean(data, silent=T)
    calc_data <- mc_prep_flat(cleaned_data)
    calc_data <- mc_calc_snow(calc_data, "TMS_T3", output_sensor="T3_snow", dr=1.5, tmax=0.5)
    expect_equal(length(calc_data[["91184101"]]$sensors), 1)
})

test_that("mc_calc_snow_agg", {
    data <- mc_read_directory("data/eco-snow", "TOMST")
    cleaned_data <- mc_prep_clean(data, silent=T)
    cleaned_data <- mc_prep_user_tz(cleaned_data, list(`94184102`=60, `94184103`=60))
    calc_data <- mc_prep_flat(cleaned_data)
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

test_that("mc_calc_agg UTC", {
    data <- mc_read_directory("data/clean-datetime_step", "TOMST")
    cleaned_data <- mc_prep_clean(data, silent=T)
    expect_error(mc_calc_agg(cleaned_data, quantile, "hour", use_utc = TRUE, probs = 0.5, na.rm=TRUE))
    calc_data <- mc_prep_flat(cleaned_data)
    calc_data <- mc_calc_agg(calc_data, quantile, "hour", use_utc = TRUE, probs = 0.5, na.rm=TRUE)
    expect_equal(length(calc_data[["94184102_agg"]]$sensors), 4)
    expect_equal(calc_data[["94184102_agg"]]$sensors$TMS_T1$values[[1]], 10)
    expect_equal(calc_data[["94184102_agg"]]$sensors$TMS_T1$values[[5]], 10)
    expect_equal(calc_data[["94184102_agg"]]$step, 60)
    expect_warning(calc_data <- mc_calc_agg(calc_data, quantile, "hour", localities="94184102", sensors="TMS_T1", use_utc = TRUE, probs = 0.5, na.rm=TRUE))
    expect_equal(length(calc_data), 10)
    expect_equal(length(calc_data[["94184102_agg"]]$sensors), 1)
})

test_that("mc_calc_agg solar time day", {
    data <- mc_read_csv("data/TOMST/files_table.csv", "data/TOMST/localities_table.csv")
    data <- mc_prep_clean(data, silent=T)
    data <- mc_prep_solar_tz(data)
    calc_data <- mc_prep_flat(data)
    calc_data <- mc_calc_agg(calc_data, quantile, "day", probs = 0.5, na.rm=TRUE, suffix="_day")
    test_calc_data_format(calc_data)
    expect_equal(length(calc_data$A2E32_day$datetime), 2)
})

test_that("mc_calc_agg_empty_data", {
    expect_warning(data <- get_empty_calc_data())
    data <- mc_calc_agg(data, quantile, "hour", use_utc=TRUE, probs=0.5, na.rm=TRUE)
    expect_equal(length(data[[1]]$loggers[[1]]$datetime), 0)
})

test_that("mc_calc_agg_mean", {
    data <- mc_read_directory("data/clean-datetime_step", "TOMST")
    data <- mc_prep_clean(data, silent=T)
    calc_data <- mc_prep_flat(data)
    calc_data <- mc_calc_agg(calc_data, mean, "hour", use_utc=T, na.rm=TRUE)
    expect_equal(calc_data[["94184102_agg"]]$sensors$TMS_T1$values[[3]], (3 * 10 + 9.9375) / 4)
})

test_that("mc_calc_agg_quantile", {
    data <- mc_read_directory("data/clean-datetime_step", "TOMST")
    data <- mc_prep_clean(data, silent=T)
    calc_data <- mc_prep_flat(data)
    calc_data <- mc_calc_agg(calc_data, quantile, "hour", use_utc=T, probs=0.1, na.rm=TRUE)
    expect_equal(calc_data[["94184102_agg"]]$sensors$TMS_T1$values[[1]], 10)
})

