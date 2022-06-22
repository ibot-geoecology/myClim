library(testthat)
source("test.R")

test_that("mc_join", {
    data <- mc_read_files("data/join", "TOMST")
    expect_error(joined_data <- mc_join(data))
    cleaned_data <- mc_prep_clean(data, silent=T)
    calib_table <- as.data.frame(tibble::tribble(
        ~serial_number, ~sensor_id,                         ~datetime, ~cor_factor,
            "91184101",     "TM_T", lubridate::ymd_h("2020-10-28 00"),         0.1,
            "94184102",   "TMS_T1", lubridate::ymd_h("2020-10-16 02"),       -0.15,
    ))
    cleaned_data <- mc_prep_calib_load(cleaned_data, calib_table)
    cleaned_data$`91184101`$loggers[[2]]$sensors$TM_T$calibration <- data.frame()
    cleaned_data$`91184101`$loggers[[3]]$sensors$TM_T$calibration <- data.frame()
    expect_warning(joined_data <- mc_join(cleaned_data, comp_sensors=c("TMS_T1", "TMS_T2")))
    test_prep_data_format(joined_data)
    expect_equal(length(joined_data$`91184101`$loggers), 1)
    expect_equal(length(joined_data$`94184102`$loggers), 1)
    expect_equal(length(joined_data$`94184103`$loggers), 1)
    expect_equal(nrow(joined_data$`91184101`$loggers[[1]]$sensors$TM_T$states), 2)
    expect_equal(nrow(joined_data$`91184101`$loggers[[1]]$sensors$TM_T$calibration), 2)
    expect_true(is.na(joined_data$`91184101`$loggers[[1]]$sensors$TM_T$calibration$cor_factor[[2]]))
    expect_equal(nrow(joined_data$`94184102`$loggers[[1]]$sensors$TMS_T1$states), 2)
    cleaned_data <- mc_prep_calib(cleaned_data, sensors = "TMS_T1", localities = "94184102")
    cleaned_data$`94184102`$loggers[[2]]$sensors$TMS_T1$metadata@calibrated <- FALSE
    expect_error(joined_data <- mc_join(cleaned_data))
})
