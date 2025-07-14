source("libtest.R")

test_that("mc_join", {
    data <- mc_read_files("../data/join", "TOMST", clean=FALSE)
    expect_error(joined_data <- mc_join(data), .prep_const_MESSAGE_UNCLEANED_DATA)
    cleaned_data <- mc_prep_clean(data, silent=T)
    calib_table <- as.data.frame(tibble::tribble(
        ~serial_number, ~sensor_id,                         ~datetime, ~cor_factor,
            "91184101",     "Thermo_T", lubridate::ymd_h("2020-10-28 00"),         0.1,
            "94184102",   "TMS_T1", lubridate::ymd_h("2020-10-16 02"),       -0.15,
    ))
    cleaned_data <- mc_prep_calib_load(cleaned_data, calib_table)
    cleaned_data$localities$`91184101`$loggers[[2]]$sensors$Thermo_T$calibration <- data.frame()
    cleaned_data$localities$`91184101`$loggers[[3]]$sensors$Thermo_T$calibration <- data.frame()
    expect_warning(joined_data <- mc_join(cleaned_data, comp_sensors=c("TMS_T1", "TMS_T2")),
                   "Selected sensors not found - Thermo_T used.") %>%
        expect_warning("Selected sensors not found - Thermo_T used.")
    test_raw_data_format(joined_data)
    expect_equal(length(joined_data$localities$`91184101`$loggers), 1)
    expect_equal(length(joined_data$localities$`94184102`$loggers), 1)
    expect_equal(length(joined_data$localities$`94184103`$loggers), 1)
    expect_equal(nrow(joined_data$localities$`91184101`$loggers[[1]]$sensors$Thermo_T$states), 2)
    expect_equal(nrow(joined_data$localities$`91184101`$loggers[[1]]$sensors$Thermo_T$calibration), 2)
    expect_true(is.na(joined_data$localities$`91184101`$loggers[[1]]$sensors$Thermo_T$calibration$cor_factor[[2]]))
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

test_that("mc_join missed sensor", {
    data <- mc_read_files("../data/join", "TOMST", clean=TRUE, silent=TRUE)
    data <- mc_filter(data, localities = "94184102")
    data <- mc_calc_vwc(data)
    data$localities$`94184102`$loggers[[1]]$sensors$VWC_moisture <- NULL
    joined_data <- mc_join(data)
    test_raw_data_format(joined_data)
    expect_equal(length(joined_data$localities$`94184102`$loggers[[1]]$sensors), 5)
})

test_that("mc_join NA values", {
    files_table <- as.data.frame(tibble::tribble(
        ~path,                                 ~locality_id, ~data_format,
        "../data/join_na/201804_93141375.csv",   "ABC",      "TOMST_join",
        "../data/join_na/201804_93141375_2.csv", "ABC",      "TOMST_join",
    ))
    data <- mc_read_data(files_table, clean=TRUE, silent=TRUE)
    joined_data <- mc_join(data, comp_sensors = c("TMS_T1", "TMS_T2", "TMS_T3"))
    test_raw_data_format(joined_data)
    expect_equal(length(joined_data$localities$ABC$loggers), 1)
})

test_that("mc_join not error", {
    data <- mc_read_files("../data/join", "TOMST", clean=TRUE, silent=TRUE)
    data1 <- mc_filter(data, localities = "91184101")
    data2 <- mc_filter(data, localities = "94184103")
    data1 <- mc_prep_meta_locality(data1, values=list(`91184101`="ABC"), param_name="locality_id")
    data2 <- mc_prep_meta_locality(data2, values=list(`94184103`="ABC"), param_name="locality_id")
    merged_data <- mc_prep_merge(list(data1, data2))
    merged_data$localities$ABC$loggers[[1]]$metadata@type <- "TMS"
    expect_warning(joined_data <- mc_join(merged_data))
    test_raw_data_format(joined_data)
})

test_that("mc_join calibration later", {
    cleaned_data <- mc_read_files("../data/join", "TOMST", clean=TRUE, silent=TRUE)
    cleaned_data <- mc_filter(cleaned_data, localities = "91184101")
    calib_table <- as.data.frame(tibble::tribble(
        ~serial_number, ~sensor_id,                         ~datetime, ~cor_factor,
        "91184101",     "Thermo_T", lubridate::ymd_h("2021-01-01 00"),         0.1,
    ))
    cleaned_data <- mc_prep_calib_load(cleaned_data, calib_table)
    joined_data <- mc_join(cleaned_data)
    test_raw_data_format(joined_data)
    expect_equal(joined_data$localities$`91184101`$loggers[[1]]$sensors$Thermo_T$calibration$datetime[[1]], lubridate::ymd_hm("2020-10-28 8:45"))
    expect_equal(nrow(joined_data$localities$`91184101`$loggers[[1]]$sensors$Thermo_T$calibration), 1)
    calib_table <- as.data.frame(tibble::tribble(
        ~serial_number, ~sensor_id,                         ~datetime, ~cor_factor,
        "91184101",     "Thermo_T", lubridate::ymd_h("2020-10-28 09"),         0.2,
        "91184101",     "Thermo_T", lubridate::ymd_h("2021-01-01 00"),         0.1,
    ))
    cleaned_data <- mc_prep_calib_load(cleaned_data, calib_table)
    joined_data <- mc_join(cleaned_data)
    test_raw_data_format(joined_data)
    expect_equal(joined_data$localities$`91184101`$loggers[[1]]$sensors$Thermo_T$calibration$cor_factor[[1]], 0.2)
    expect_equal(nrow(joined_data$localities$`91184101`$loggers[[1]]$sensors$Thermo_T$calibration), 1)
    calib_table <- as.data.frame(tibble::tribble(
        ~serial_number, ~sensor_id,                         ~datetime, ~cor_factor,
        "91184101",     "Thermo_T", lubridate::ymd_h("2020-10-27 00"),         0.2,
        "91184101",     "Thermo_T", lubridate::ymd_h("2020-10-28 09"),         0.3,
        "91184101",     "Thermo_T", lubridate::ymd_h("2021-01-01 00"),         0.4,
    ))
    cleaned_data <- mc_prep_calib_load(cleaned_data, calib_table)
    joined_data <- mc_join(cleaned_data)
    test_raw_data_format(joined_data)
    expect_equal(nrow(joined_data$localities$`91184101`$loggers[[1]]$sensors$Thermo_T$calibration), 2)
    expect_equal(joined_data$localities$`91184101`$loggers[[1]]$sensors$Thermo_T$calibration$cor_factor, c(0.2, 0.3))
    expect_equal(joined_data$localities$`91184101`$loggers[[1]]$sensors$Thermo_T$calibration$datetime[[1]],
                 lubridate::ymd_hm("2020-10-28 8:45"))
    calib_table <- as.data.frame(tibble::tribble(
        ~serial_number, ~sensor_id,                         ~datetime, ~cor_factor,
        "91184101",     "Thermo_T", lubridate::ymd_h("2020-10-26 00"),         0.1,
        "91184101",     "Thermo_T", lubridate::ymd_h("2020-10-27 00"),         0.2,
        "91184101",     "Thermo_T", lubridate::ymd_h("2020-10-28 09"),         0.3,
        "91184101",     "Thermo_T", lubridate::ymd_h("2021-01-01 00"),         0.4,
    ))
    cleaned_data <- mc_prep_calib_load(cleaned_data, calib_table)
    joined_data <- mc_join(cleaned_data)
    test_raw_data_format(joined_data)
    expect_equal(nrow(joined_data$localities$`91184101`$loggers[[1]]$sensors$Thermo_T$calibration), 2)
    expect_equal(joined_data$localities$`91184101`$loggers[[1]]$sensors$Thermo_T$calibration$cor_factor, c(0.2, 0.3))
    expect_equal(joined_data$localities$`91184101`$loggers[[1]]$sensors$Thermo_T$calibration$datetime[[1]],
                 lubridate::ymd_hm("2020-10-28 8:45"))
})

test_that("mc_join calibration", {
    files_table <- as.data.frame(tibble::tribble(
        ~path, ~locality_id, ~data_format,
        "../data/join_calib/data_91184101_0.csv", "A", "TOMST",
        "../data/join_calib/data_91184102_0.csv", "A", "TOMST",
    ))
    data <- mc_read_data(files_table, silent=TRUE)
    test_raw_data_format(data)
    joined_data <- mc_join(data)
    test_raw_data_format(joined_data)
    expect_equal(nrow(joined_data$localities$A$loggers$Thermo_1$sensors$Thermo_T$calibration), 0)
    calib_table <- as.data.frame(tibble::tribble(
        ~serial_number, ~sensor_id,                         ~datetime, ~cor_factor,
        "91184102",     "Thermo_T", lubridate::ymd_h("2020-10-27 00"),         0.1,
    ))
    data <- mc_prep_calib_load(data, calib_table)
    joined_data <- mc_join(data)
    test_raw_data_format(joined_data)
    expect_equal(nrow(joined_data$localities$A$loggers[[1]]$sensors$Thermo_T$calibration), 4)
    expect_equal(joined_data$localities$A$loggers[[1]]$sensors$Thermo_T$calibration$cor_factor, c(NA_real_, 0.1, NA_real_, 0.1))
})

test_that("mc_join join_serial", {
    files_table <- as.data.frame(tibble::tribble(
        ~path, ~locality_id, ~data_format,
        "../data/join_serial/data_94184101_0.csv", "A", "TOMST",
        "../data/join_serial/data_94184101_1.csv", "A", "TOMST",
        "../data/join_serial/data_94184102_0.csv", "A", "TOMST",
        "../data/join_serial/data_94184103_0.csv", "A", "TOMST",
        "../data/join_serial/data_94184104_0.csv", "A", "TOMST",
        "../data/join_serial/data_94184105_0.csv", "A", "TOMST",
    ))
    data <- mc_read_data(files_table, silent=TRUE)
    join_data <- mc_join(data, by_type = FALSE)
    test_raw_data_format(data)
    data <- mc_prep_meta_locality(data, list(A=list(c("94184102", "94184103"), c("94184104", "94184105"))), param_name="join_serial")
    test_raw_data_format(data)
    join_data <- mc_join(data, by_type = FALSE)
    test_raw_data_format(join_data)
    join_data <- mc_join(join_data, by_type = FALSE)
})

test_that("mc_join tolerance", {
    data <- mc_read_files("../data/join_tolerance", "TOMST", silent=TRUE)
    tolerance <- list(T_C=0.5)
    join_data <- mc_join(data, tolerance = tolerance)
    test_raw_data_format(join_data)
    expect_equal(join_data$localities$`94184101`$loggers[[1]]$sensors$TMS_T1$values[17:19], c(9.525, 9.525, 9.525))
})

