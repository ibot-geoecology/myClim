source("libtest.R")

test_that("mc_join", {
    data <- mc_read_files("../data/join", "TOMST", clean=FALSE)
    expect_error(joined_data <- mc_join(data), "Data aren't cleaned.")
    cleaned_data <- mc_prep_clean(data, silent=T)
    calib_table <- as.data.frame(tibble::tribble(
        ~serial_number, ~sensor_id,                         ~datetime, ~cor_factor,
            "91184101",     "TS_T", lubridate::ymd_h("2020-10-28 00"),         0.1,
            "94184102",   "TMS_T1", lubridate::ymd_h("2020-10-16 02"),       -0.15,
    ))
    cleaned_data <- mc_prep_calib_load(cleaned_data, calib_table)
    cleaned_data$localities$`91184101`$loggers[[2]]$sensors$TS_T$calibration <- data.frame()
    cleaned_data$localities$`91184101`$loggers[[3]]$sensors$TS_T$calibration <- data.frame()
    expect_warning(joined_data <- mc_join(cleaned_data, comp_sensors=c("TMS_T1", "TMS_T2")),
                   "Selected sensors not found - TS_T used.") %>%
        expect_warning("Selected sensors not found - TS_T used.")
    test_raw_data_format(joined_data)
    expect_equal(length(joined_data$localities$`91184101`$loggers), 1)
    expect_equal(length(joined_data$localities$`94184102`$loggers), 1)
    expect_equal(length(joined_data$localities$`94184103`$loggers), 1)
    expect_equal(nrow(joined_data$localities$`91184101`$loggers[[1]]$sensors$TS_T$states), 2)
    expect_equal(nrow(joined_data$localities$`91184101`$loggers[[1]]$sensors$TS_T$calibration), 2)
    expect_true(is.na(joined_data$localities$`91184101`$loggers[[1]]$sensors$TS_T$calibration$cor_factor[[2]]))
    expect_equal(nrow(joined_data$localities$`94184102`$loggers[[1]]$sensors$TMS_T1$states), 2)
    cleaned_data <- mc_prep_calib(cleaned_data, sensors = "TMS_T1", localities = "94184102")
    cleaned_data$localities$`94184102`$loggers[[2]]$sensors$TMS_T1$metadata@calibrated <- FALSE
    expect_error(joined_data <- mc_join(cleaned_data), "Calibration in sensors is inconsistent.")
})

test_that("mc_join same height", {
    data <- mc_read_files("../data/join", "TOMST", clean=TRUE, silent=TRUE)
    data <- mc_filter(data, localities = c("94184102", "94184103"))
    data <- mc_calc_vwc(data)
    joined_data <- mc_join(data)
    test_raw_data_format(joined_data)
    expect_equal(length(joined_data$localities$`94184102`$loggers), 1)
    expect_equal(length(joined_data$localities$`94184102`$loggers[[1]]$sensors), 5)
    expect_equal(length(joined_data$localities$`94184103`$loggers), 1)
    expect_equal(length(joined_data$localities$`94184103`$loggers[[1]]$sensors), 5)
    data$localities$`94184102`$loggers[[1]]$sensors <- data$localities$`94184102`$loggers[[1]]$sensors[-5]
    data$localities$`94184103`$loggers[[2]]$sensors <- data$localities$`94184103`$loggers[[2]]$sensors[-5]
    joined_data <- mc_join(data)
    test_raw_data_format(joined_data)
    expect_equal(length(joined_data$localities$`94184102`$loggers), 1)
    expect_equal(length(joined_data$localities$`94184102`$loggers[[1]]$sensors), 5)
    expect_true(is.na(dplyr::first(joined_data$localities$`94184102`$loggers[[1]]$sensors$VWC_moisture$values)))
    expect_equal(length(joined_data$localities$`94184103`$loggers), 1)
    expect_equal(length(joined_data$localities$`94184103`$loggers[[1]]$sensors), 5)
    expect_true(is.na(dplyr::last(joined_data$localities$`94184103`$loggers[[1]]$sensors$VWC_moisture$values)))
})
