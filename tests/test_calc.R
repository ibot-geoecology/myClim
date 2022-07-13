library(testthat)
library(myClim)

source("test.R")

test_that("mc_calc_snow", {
    cleaned_data <- mc_read_files("data/eco-snow", "TOMST", silent=T)
    snow_prep_data <- mc_calc_snow(cleaned_data, "TMS_T3", output_sensor="T3_snow", range=1.5, tmax=0.5)
    test_prep_data_format(snow_prep_data)
    expect_true("T3_snow" %in% names(snow_prep_data$`94184102`$loggers[[1]]$sensors))
    calc_data <- mc_agg(cleaned_data)
    calc_data <- mc_calc_snow(calc_data, "TMS_T3", output_sensor="T3_snow", range=1.5, tmax=0.5)
    expect_true(is.na(calc_data$localities[["94184102"]]$sensors$T3_snow$values[[1]]))
    expect_false(is.na(calc_data$localities[["94184102"]]$sensors$T3_snow$values[[4 * 24 + 1]]))
    expect_equal(class(calc_data$localities[["94184102"]]$sensors$T3_snow$values), "logical")
})

test_that("mc_calc_snow long period", {
    cleaned_data <- mc_read_files("data/eco-snow", "TOMST", silent=T)
    expect_warning(calc_data <- mc_agg(cleaned_data, "mean", "week"))
    expect_error(calc_data <- mc_calc_snow(calc_data, "TMS_T3_mean", output_sensor="T3_snow", range=1.5, tmax=0.5))
})

test_that("mc_calc_snow_logger_without_sensor", {
    expect_warning(cleaned_data <- mc_read_files("data/TOMST", "TOMST", silent = T))
    calc_data <- mc_agg(cleaned_data)
    expect_warning(calc_data <- mc_calc_snow(calc_data, "TMS_T3", output_sensor="T3_snow", range=1.5, tmax=0.5))
    expect_equal(length(calc_data$localities[["91184101"]]$sensors), 1)
})

test_that("mc_calc_snow_agg", {
    cleaned_data <- mc_read_files("data/eco-snow", "TOMST", silent = T)
    cleaned_data <- mc_prep_meta_locality(cleaned_data, list(`94184102`=60, `94184103`=60), "tz_offset")
    prep_data <- mc_calc_snow(cleaned_data, "TMS_T3", range=1.5, tmax=0.5)
    snow_agg <- mc_calc_snow_agg(prep_data, "snow")
    expect_equal(colnames(snow_agg), c("locality_id", "snow_days", "first_day", "last_day", "first_day_period", "last_day_period"))
    calc_data <- mc_agg(cleaned_data)
    calc_data <- mc_calc_snow(calc_data, "TMS_T3", range=1.5, tmax=0.5)
    snow_agg <- mc_calc_snow_agg(calc_data, "snow")
    expect_equal(colnames(snow_agg), c("locality_id", "snow_days", "first_day", "last_day", "first_day_period", "last_day_period"))
    expect_equal(nrow(snow_agg), 2)
    expect_equal(snow_agg[1, 1], "94184103")
    expect_true(is.na(snow_agg[1, 3]))
    expect_true(is.na(snow_agg[1, 4]))
    expect_true(is.na(snow_agg[1, 5]))
    expect_true(is.na(snow_agg[1, 6]))
    expect_equal(snow_agg[2, 1], "94184102")
    expect_equal(snow_agg[2, 3], as.Date("2021-01-06"))
    expect_equal(snow_agg[2, 4], as.Date("2021-01-15"))
    expect_equal(snow_agg[2, 5], as.Date("2021-01-06"))
    expect_equal(snow_agg[2, 6], as.Date("2021-01-15"))
})

test_that("mc_calc_snow_agg no sensor", {
    cleaned_data <- mc_read_files("data/eco-snow", "TOMST", silent = T)
    calc_data <- mc_agg(cleaned_data)
    expect_error(snow_agg <- mc_calc_snow_agg(calc_data, "snow"))
})

test_that("mc_calc_vwc", {
    cleaned_data <- mc_read_data("data/TOMST/files_table.csv", silent = T)
    calib_table <- as.data.frame(tibble::tribble(
        ~serial_number,          ~sensor_id,                         ~datetime, ~cor_factor, ~cor_slope,
            "94184103",   "TMS_TMSmoisture",          lubridate::ymd(20201016),        0.02,        1.1,
            "94184103",   "TMS_TMSmoisture", lubridate::ymd_h("2020-10-16 14"),      -0.015,          1,
    ))
    cleaned_data <- mc_prep_calib_load(cleaned_data, calib_table)
    expect_warning(prep_data <- mc_calc_vwc(cleaned_data, localities=c("A1E05", "A2E32")))
    test_prep_data_format(prep_data)
    expect_true("VWC_moisture" %in% names(prep_data$A2E32$loggers[[1]]$sensors))
    calc_data <- mc_agg(cleaned_data)
    expect_warning(calc_data <- mc_calc_vwc(calc_data, localities=c("A1E05", "A2E32")))
    test_calc_data_format(calc_data)
    expect_false(calc_data$localities$A2E32$sensors$TMS_TMSmoisture$metadata@calibrated)
    expect_true(calc_data$localities$A2E32$sensors$VWC_moisture$metadata@calibrated)
    expect_equal(calc_data$localities$A2E32$sensors$TMS_TMSmoisture$calibration,
                 calc_data$localities$A2E32$sensors$VWC_moisture$calibration)
})

test_that("mc_calc_vwc frozen2NA", {
    cleaned_data <- mc_read_files("data/eco-snow/data_94184103_0.csv", "TOMST", silent = T)
    calc_data <- mc_agg(cleaned_data)
    vwc_data <- mc_calc_vwc(calc_data, temp_sensor = "TMS_T3")
    expect_true(is.na(vwc_data$localities$`94184103`$sensors$VWC_moisture$values[[8]]))
    vwc_data <- mc_calc_vwc(calc_data, temp_sensor = "TMS_T3", frozen2NA = FALSE)
    expect_false(is.na(vwc_data$localities$`94184103`$sensors$VWC_moisture$values[[8]]))
})

test_that("mc_calc_vwc wrong", {
    cleaned_data <- mc_read_data("data/TOMST/files_table.csv", silent = T)
    calc_data <- mc_agg(cleaned_data)
    expect_error(calc_data <- mc_calc_vwc(calc_data, temp_sensor="TMS_TMSmoisture", localities="A2E32"))
    expect_error(calc_data <- mc_calc_vwc(calc_data, moist_sensor="TMS_T1", localities="A2E32"))
})

test_that("mc_calc_gdd", {
    cleaned_data <- mc_read_files("data/calc-gdd", "TOMST", silent = T)
    prep_data <- mc_calc_gdd(cleaned_data, "TS_T")
    test_prep_data_format(prep_data)
    expect_true("GDD5" %in% names(prep_data$`91184101`$loggers[[1]]$sensors))
    calc_data <- mc_agg(cleaned_data)
    calc_data <- mc_calc_gdd(calc_data, "TS_T")
    expect_equal(calc_data$localities$`91184101`$sensors$GDD5$values[1], (12.4375 - 5) * 15/(60*24))
    expect_equal(calc_data$localities$`91184101`$sensors$GDD5$values[176], 0)
})

test_that("mc_calc_fdd", {
    cleaned_data <- mc_read_files("data/calc-gdd", "TOMST", silent=T)
    prep_data <- mc_calc_fdd(cleaned_data, "TS_T")
    test_prep_data_format(prep_data)
    expect_true("FDD0" %in% names(prep_data$`91184101`$loggers[[1]]$sensors))
    calc_data <- mc_agg(cleaned_data)
    calc_data <- mc_calc_fdd(calc_data, "TS_T")
    expect_equal(calc_data$localities$`91184101`$sensors$FDD0$values[1], 0)
    expect_equal(calc_data$localities$`91184101`$sensors$FDD0$values[288], (0.5) * 15/(60*24))
})

test_that("mc_calc_cumsum", {
    cleaned_data <- mc_read_data("data/TOMST/files_table.csv", silent=T)
    expect_warning(prep_data <- mc_calc_cumsum(cleaned_data, c("TMS_T1", "TMS_T2")))
    test_prep_data_format(prep_data)
    expect_true("TMS_T1_cumsum" %in% names(prep_data$A2E32$loggers[[1]]$sensors))
    calc_data <- mc_agg(cleaned_data)
    expect_warning(calc_data <- mc_calc_snow(calc_data, "TMS_T3", range=1.5, tmax=0.5))
    expect_warning(cumsum_data <- mc_calc_cumsum(calc_data, c("TMS_T1", "TMS_T2", "snow")))
    test_calc_data_format(calc_data)
    expect_equal(cumsum(cumsum_data$localities$A2E32$sensors$TMS_T1$values), cumsum_data$localities$A2E32$sensors$TMS_T1_cumsum$values)
    expect_equal(cumsum_data$localities$A2E32$sensors$snow_cumsum$metadata@sensor_id, myClim:::.model_const_SENSOR_integer)
})

test_that("mc_calc_tomst_dendro", {
    expect_warning(cleaned_data <- mc_read_files("data/TOMST", "TOMST", silent=T))
    expect_warning(prep_data <- mc_calc_tomst_dendro(cleaned_data))
    test_prep_data_format(prep_data)
    expect_true("dendro_l_um" %in% names(prep_data$`92192250`$loggers[[1]]$sensors))
    calc_data <- mc_agg(cleaned_data)
    expect_warning(calc_data <- mc_calc_tomst_dendro(calc_data))
    test_calc_data_format(calc_data)
    expect_equal(calc_data$localities$`92192250`$sensors$dendro_l_um$values[[1]],
                 (calc_data$localities$`92192250`$sensors$DEND_TOMSTdendro$values[[1]] - 1279) * (8890 / (34000 - 1279)))
})

test_that("mc_calc_vpd", {
    data <- mc_read_files("data/HOBO/20024354.txt", "HOBO", date_format = "%d.%m.%Y %H:%M:%S", clean=FALSE, silent=TRUE)
    data <- mc_prep_meta_locality(data, list(`20024354`="LOC"), param_name = "locality_id")
    cleaned_data <- mc_prep_clean(data, silent=T)
    prep_data <- mc_calc_vpd(cleaned_data, myClim:::.model_const_SENSOR_HOBO_T_C, myClim:::.model_const_SENSOR_HOBO_RH)
    test_prep_data_format(prep_data)
    expect_true("VPD" %in% names(prep_data$LOC$loggers[[1]]$sensors))
    calc_data <- mc_agg(cleaned_data)
    calc_data <- mc_calc_vpd(calc_data, myClim:::.model_const_SENSOR_HOBO_T_C, myClim:::.model_const_SENSOR_HOBO_RH)
    test_calc_data_format(calc_data)
    vpd_martin <- function(t,rh,elev = 0) {
        a <- 0.61121
        b <- 18.678 - (t/234.5)
        c <- 257.14
        P <- 101300*exp(-elev/8200)
        f <- 1.00072 + (10e-7*P*(0.032+5.9*10e-6*t^2)) #enhancement factor
        vpd <- f*a*exp(b*t/(c+t))*(1-rh/100)
        return(vpd)
    }
    expect_equal(vpd_martin(calc_data$localities$LOC$sensors$HOBO_T_C$values[[1]],
                            calc_data$localities$LOC$sensors$HOBO_RH$values[[1]]),
                 calc_data$localities$LOC$sensors$VPD$values[[1]])
    calc_data <- mc_prep_meta_locality(calc_data, list(LOC = 500), "altitude")
    calc_data <- mc_calc_vpd(calc_data, myClim:::.model_const_SENSOR_HOBO_T_C, myClim:::.model_const_SENSOR_HOBO_RH,
                             output_sensor = "VPD500")
    expect_equal(vpd_martin(calc_data$localities$LOC$sensors$HOBO_T_C$values[[1]],
                            calc_data$localities$LOC$sensors$HOBO_RH$values[[1]], elev = 500),
                 calc_data$localities$LOC$sensors$VPD500$values[[1]])
})
