library(testthat)
library(myClim)

source("test.R")

test_that("mc_calc_snow", {
    data <- mc_read_files("data/eco-snow", "TOMST")
    cleaned_data <- mc_prep_clean(data, silent=T)
    expect_error(mc_calc_snow(cleaned_data, "TMS_T3", output_sensor="T3_snow", dr=1.5, tmax=0.5))
    calc_data <- mc_agg(cleaned_data)
    calc_data <- mc_calc_snow(calc_data, "TMS_T3", output_sensor="T3_snow", dr=1.5, tmax=0.5)
    expect_true(is.na(calc_data$localities[["94184102"]]$sensors$T3_snow$values[[1]]))
    expect_false(is.na(calc_data$localities[["94184102"]]$sensors$T3_snow$values[[4 * 24 + 1]]))
    expect_equal(class(calc_data$localities[["94184102"]]$sensors$T3_snow$values), "logical")
})

test_that("mc_calc_snow long period", {
    data <- mc_read_files("data/eco-snow", "TOMST")
    cleaned_data <- mc_prep_clean(data, silent=T)
    expect_warning(calc_data <- mc_agg(cleaned_data, "mean", "week"))
    expect_error(calc_data <- mc_calc_snow(calc_data, "TMS_T3_mean", output_sensor="T3_snow", dr=1.5, tmax=0.5))
})

test_that("mc_calc_snow_logger_without_sensor", {
    expect_warning(data <- mc_read_files("data/TOMST", "TOMST"))
    cleaned_data <- mc_prep_clean(data, silent=T)
    calc_data <- mc_agg(cleaned_data)
    expect_warning(calc_data <- mc_calc_snow(calc_data, "TMS_T3", output_sensor="T3_snow", dr=1.5, tmax=0.5))
    expect_equal(length(calc_data$localities[["91184101"]]$sensors), 1)
})

test_that("mc_calc_snow_agg", {
    data <- mc_read_files("data/eco-snow", "TOMST")
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

test_that("mc_calc_snow_agg no sensor", {
    data <- mc_read_files("data/eco-snow", "TOMST")
    cleaned_data <- mc_prep_clean(data, silent=T)
    calc_data <- mc_agg(cleaned_data)
    expect_error(snow_agg <- mc_calc_snow_agg(calc_data, "snow"))
})

test_that("mc_calc_vwc", {
    data <- mc_read_data("data/TOMST/files_table.csv")
    cleaned_data <- mc_prep_clean(data, silent=T)
    calib_table <- as.data.frame(tibble::tribble(
        ~serial_number,          ~sensor_id,                         ~datetime, ~slope, ~intercept,
            "94184103",   "TMS_TMSmoisture",          lubridate::ymd(20201016),    1.1,       0.02,
            "94184103",   "TMS_TMSmoisture", lubridate::ymd_h("2020-10-16 14"),      1,     -0.015,
    ))
    cleaned_data <- mc_prep_calib_load(cleaned_data, calib_table)
    calc_data <- mc_agg(cleaned_data)
    expect_warning(calc_data <- mc_calc_vwc(calc_data, localities=c("A1E05", "A2E32")))
    test_calc_data_format(calc_data)
    expect_false(calc_data$localities$A2E32$sensors$TMS_TMSmoisture$metadata@calibrated)
    expect_true(calc_data$localities$A2E32$sensors$vwc_moisture$metadata@calibrated)
    expect_equal(calc_data$localities$A2E32$sensors$TMS_TMSmoisture$calibration,
                 calc_data$localities$A2E32$sensors$vwc_moisture$calibration)
})

test_that("mc_calc_vwc wrong", {
    data <- mc_read_data("data/TOMST/files_table.csv")
    cleaned_data <- mc_prep_clean(data, silent=T)
    calc_data <- mc_agg(cleaned_data)
    expect_error(calc_data <- mc_calc_vwc(calc_data, temp_sensor="TMS_TMSmoisture", localities="A2E32"))
    expect_error(calc_data <- mc_calc_vwc(calc_data, moist_sensor="TMS_T1", localities="A2E32"))
})

test_that("mc_calc_gdd", {
    data <- mc_read_files("data/calc-gdd", "TOMST")
    cleaned_data <- mc_prep_clean(data, silent=T)
    expect_error(mc_calc_gdd(cleaned_data, "TM_T"))
    calc_data <- mc_agg(cleaned_data)
    calc_data <- mc_calc_gdd(calc_data, "TM_T")
    expect_equal(calc_data$localities$`91184101`$sensors$GDD5$values[1], (12.4375 - 5) * 15/(60*24))
    expect_equal(calc_data$localities$`91184101`$sensors$GDD5$values[176], 0)
})

test_that("mc_calc_fdd", {
    data <- mc_read_files("data/calc-gdd", "TOMST")
    cleaned_data <- mc_prep_clean(data, silent=T)
    expect_error(mc_calc_fdd(cleaned_data, "TM_T"))
    calc_data <- mc_agg(cleaned_data)
    calc_data <- mc_calc_fdd(calc_data, "TM_T")
    expect_equal(calc_data$localities$`91184101`$sensors$FDD0$values[1], 0)
    expect_equal(calc_data$localities$`91184101`$sensors$FDD0$values[288], (0.5) * 15/(60*24))
})
