source("libtest.R")

test_that("mc_prep_clean", {
    cleaned_data <- mc_read_files("../data/clean-datetime_step", "TOMST", silent=T)
    expect_warning(mc_prep_clean(cleaned_data, silent=T))
    test_raw_data_format(cleaned_data)
    expect_equal(cleaned_data$localities[["94184102"]]$loggers[[1]]$clean_info@count_duplicities, 1)
    expect_equal(cleaned_data$localities[["94184102"]]$loggers[[1]]$clean_info@count_missing, 2)
    expect_equal(cleaned_data$localities[["94184165"]]$loggers[[1]]$clean_info@count_duplicities, 25)
    expect_equal(cleaned_data$localities[["94184169"]]$loggers[[1]]$clean_info@count_disordered, 1)
    diff_datetime <- diff(as.numeric(cleaned_data$localities[["94184169"]]$loggers[[1]]$datetime)) %/% 60
    expect_equal(diff_datetime, rep(15, 5))
    diff_datetime <- diff(as.numeric(cleaned_data$localities[["94184170"]]$loggers[[1]]$datetime)) %/% 60
    expect_equal(diff_datetime, rep(15, 5))
    test_function <- if(exists(".prep_is_logger_cleaned")) .prep_is_logger_cleaned else .prep_is_logger_cleaned
    expect_true(test_function(cleaned_data$localities[["94184102"]]$loggers[[1]]))
    expect_true(.prep_clean_was_error_in_logger(cleaned_data$localities[["94184102"]]$loggers[[1]]))
    expect_equal(length(cleaned_data$localities[["94184102"]]$loggers[[1]]$datetime), 49)
    expect_true(is.na(cleaned_data$localities[["94184102"]]$loggers[[1]]$sensors$TMS_T1$values[[19]]))
    expect_equal(cleaned_data$localities[["91184133"]]$loggers[[1]]$clean_info@step, 15 * 60)
    expect_equal(cleaned_data$localities[["91184133"]]$loggers[[1]]$sensors$TS_T$states$start, dplyr::first(cleaned_data$localities[["91184133"]]$loggers[[1]]$datetime))
    agg_data <- mc_agg(cleaned_data)
    expect_error(mc_prep_clean(agg_data))
})

test_that("mc_prep_clean defined step", {
    cleaned_data <- mc_read_files("../data/clean-datetime_step", "TOMST", step=30*60, silent=T)
    expect_equal(cleaned_data$localities[["94184102"]]$loggers[[1]]$clean_info@step, 30*60)
    expect_equal(length(cleaned_data$localities[["94184102"]]$loggers[[1]]$datetime), 25)
})

test_that("mc_prep_clean rounding", {
    cleaned_data <- mc_read_files("../data/clean-rounding", "TOMST_join", silent=T)
    test_raw_data_format(cleaned_data)
    expect_equal(cleaned_data$localities$A1E01_TS$loggers[[1]]$datetime, c(lubridate::ymd_hm("2018-10-18 09:00"),
                                                                           lubridate::ymd_hm("2018-10-18 11:00"),
                                                                           lubridate::ymd_hm("2018-10-18 13:00"),
                                                                           lubridate::ymd_hm("2018-10-18 15:00"),
                                                                           lubridate::ymd_hm("2018-10-18 17:00"),
                                                                           lubridate::ymd_hm("2018-10-18 19:00")))
})

test_that("mc_prep_clean 1.5 hour step", {
    data <- mc_read_files("../data/HOBO/6265.csv", "HOBO", date_format = "%m/%d/%y %I:%M:%S %p", clean=FALSE, silent=TRUE)
    cleaned_data <- mc_prep_clean(data, silent=T)
    test_raw_data_format(cleaned_data)
    expect_equal(data$`20396265`$loggers[[1]]$datetime, cleaned_data$`20396265`$loggers[[1]]$datetime)
})

test_that("mc_prep_clean one record", {
    data <- mc_read_files("../data/clean-one-record", "TOMST", clean = FALSE)
    expect_warning(cleaned_data <- mc_prep_clean(data, silent=T))
    expect_true(is.na(cleaned_data$localities[["94208611"]]$loggers[[1]]$clean_info@step))
    expect_true(is.na(cleaned_data$localities[["94208611"]]$loggers[[1]]$clean_info@count_duplicities))
    expect_true(is.na(cleaned_data$localities[["94208611"]]$loggers[[1]]$clean_info@count_disordered))
    expect_true(is.na(cleaned_data$localities[["94208611"]]$loggers[[1]]$clean_info@count_missing))
})

test_that("mc_prep_clean ok", {
    cleaned_data <- mc_read_files("../data/TOMST/data_94184102_0.csv", "TOMST", silent=T)
    expect_true(.prep_is_logger_cleaned(cleaned_data$localities[[1]]$loggers[[1]]))
    expect_false(.prep_clean_was_error_in_logger(cleaned_data$localities[[1]]$loggers[[1]]))
})

test_that("mc_prep_solar_tz", {
    data <- mc_read_data("../data/TOMST/files_table.csv", "../data/TOMST/localities_table.csv", clean = FALSE)
    data <- mc_prep_solar_tz(data)
    test_raw_data_format(data)
    expect_equal(data$localities$A1E05$metadata@tz_offset, 57)
})

test_that("mc_prep_meta_locality", {
    data <- mc_read_data("../data/TOMST/files_table.csv", "../data/TOMST/localities_table.csv", clean = FALSE)
    expect_error(changed_data <- mc_prep_meta_locality(data, list(A1E05=50)))
    changed_data <- mc_prep_meta_locality(data, list(A1E05=50), param_name="tz_offset")
    test_raw_data_format(changed_data)
    expect_equal(changed_data$localities$A1E05$metadata@tz_offset, 50)
    expect_equal(changed_data$localities$A1E05$metadata@tz_type, myClim:::.model_const_TZ_USER_DEFINED)
    changed_data <- mc_prep_meta_locality(data, list(A1E05="abc", A2E32="def"), param_name="my_super_param")
    test_raw_data_format(changed_data)
    expect_equal(changed_data$localities$A1E05$metadata@user_data[["my_super_param"]], "abc")
    metadata <- as.data.frame(tibble::tribble(
        ~locality_id, ~lat_wgs84, ~lon_wgs84, ~my,
        "A1E05"     ,          1,          2,  NA,
        "A2E32"     ,          3,          4,  0,
        "TEST"      ,          1,          1, 10
    ))
    expect_error(changed_data <- mc_prep_meta_locality(data, metadata, param_name="tz_offset"))
    expect_warning(changed_data <- mc_prep_meta_locality(data, metadata), "There isn't locality TEST.") %>%
        expect_warning("There isn't locality TEST.") %>%
        expect_warning("There isn't locality TEST.")
    test_raw_data_format(changed_data)
    expect_equal(changed_data$localities$A1E05$metadata@lat_wgs84, 1)
    expect_equal(changed_data$localities$A1E05$metadata@lon_wgs84, 2)
    expect_true(is.na(changed_data$localities$A1E05$metadata@user_data[["my"]]))
    data_clean <- mc_prep_clean(data, silent=T)
    data_calc <- mc_agg(data_clean)
    expect_warning(changed_data <- mc_prep_meta_locality(data_calc, metadata), "There isn't locality TEST.") %>%
        expect_warning("There isn't locality TEST.") %>%
        expect_warning("There isn't locality TEST.")
    test_agg_data_format(changed_data)
    expect_equal(changed_data$localities$A1E05$metadata@lat_wgs84, 1)
})

test_that("mc_prep_meta_locality rename", {
    data <- mc_read_data("../data/TOMST/files_table.csv", clean=FALSE)
    renamed_data <- mc_prep_meta_locality(data, list(A1E05="ABC05", A2E32="CDE32"), "locality_id")
    expect_equal(sort(names(renamed_data$localities)), sort(c("ABC05", "CDE32", "A6W79")))
    values <- as.data.frame(tibble::tribble(
        ~locality_id, ~new_locality_id,
        "A1E05"     ,          "ABC05",
        "A2E32"     ,          "CDE32",
    ))
    renamed_data <- mc_prep_meta_locality(data, values)
    expect_equal(sort(names(renamed_data$localities)), sort(c("ABC05", "CDE32", "A6W79")))
    renamed_data <- mc_prep_clean(renamed_data, silent=T)
    renamed_data <- mc_agg(renamed_data, c("min", "max"), "hour")
    renamed_data <- mc_prep_meta_locality(renamed_data, list(ABC05="AAA05"), "locality_id")
    expect_equal(names(renamed_data$localities), c("AAA05", "CDE32", "A6W79"))
})

test_that("mc_prep_crop", {
    data <- mc_read_data("../data/TOMST/files_table.csv", "../data/TOMST/localities_table.csv", clean=FALSE)
    cropped_data <- mc_prep_crop(data, start=as.POSIXct("2020-10-16 08:00", tz="UTC"))
    test_raw_data_format(cropped_data)
    expect_equal(length(cropped_data$localities$A2E32$loggers[[1]]$datetime), 68)
    expect_equal(length(cropped_data$localities$A2E32$loggers[[1]]$sensors$TMS_T1$values), 68)
    expect_equal(cropped_data$localities$A2E32$loggers[[1]]$sensors$TMS_T1$states$start, lubridate::ymd_h("2020-10-16 08"))
    cropped_data <- mc_prep_crop(data, end=as.POSIXct("2020-10-16 08:00", tz="UTC"))
    test_raw_data_format(cropped_data)
    expect_equal(length(cropped_data$localities$A2E32$loggers[[1]]$datetime), 8)
    expect_equal(length(cropped_data$localities$A2E32$loggers[[1]]$sensors$TMS_T1$values), 8)
    expect_equal(cropped_data$localities$A2E32$loggers[[1]]$sensors$TMS_T1$states$end, lubridate::ymd_h("2020-10-16 08"))
    cropped_data <- mc_prep_crop(data, end=as.POSIXct("2020-10-16 08:00", tz="UTC"), end_included=FALSE)
    test_raw_data_format(cropped_data)
    expect_equal(length(cropped_data$localities$A2E32$loggers[[1]]$datetime), 7)
    expect_equal(length(cropped_data$localities$A2E32$loggers[[1]]$sensors$TMS_T1$values), 7)
    data_clean <- mc_prep_clean(data, silent=T)
    data_calc <- mc_agg(data_clean)
    data_calc <- mc_calc_snow(data_calc, "TMS_T2", localities = c("A2E32", "A6W79"))
    cropped_agg_data <- mc_prep_crop(data_calc, start=as.POSIXct("2020-10-16 06:00", tz="UTC"), end=as.POSIXct("2020-10-16 08:00", tz="UTC"))
    expect_equal(cropped_agg_data$localities$A6W79$sensors$TMS_T1$states$start, lubridate::ymd_h("2020-10-16 06"))
    expect_equal(cropped_agg_data$localities$A6W79$sensors$TMS_T1$states$end, lubridate::ymd_h("2020-10-16 08"))
    expect_equal(nrow(cropped_agg_data$localities$A1E05$sensors$TS_T$states), 0)
    test_agg_data_format(cropped_agg_data)
})

test_that("mc_prep_crop errors", {
    expect_warning(data <- mc_read_files("../data/TOMST-error", "TOMST", clean=FALSE))
    cropped_data <- mc_prep_crop(data, start=lubridate::ymd_hm("2022-02-24 07:45"), end=lubridate::ymd_hm("2022-02-24 10:30"))
    states <- dplyr::filter(cropped_data$localities$data_93142777$loggers[[1]]$sensors$TMS_T2$states, .data$tag == .model_const_SENSOR_STATE_ERROR)
    expect_equal(nrow(states), 4)
    expect_equal(states$start, c(lubridate::ymd_hm("2022-02-24 07:45"),
                                 lubridate::ymd_hm("2022-02-24 09:00"),
                                 lubridate::ymd_hm("2022-02-24 09:45"),
                                 lubridate::ymd_hm("2022-02-24 10:15")))
    expect_equal(states$end, c(lubridate::ymd_hm("2022-02-24 08:30"),
                               lubridate::ymd_hm("2022-02-24 09:15"),
                               lubridate::ymd_hm("2022-02-24 09:45"),
                               lubridate::ymd_hm("2022-02-24 10:30")))
})

test_that(".prep_get_loggers_datetime_step_unprocessed", {
    data <- mc_read_data("../data/TOMST/files_table.csv", "../data/TOMST/localities_table.csv", clean=FALSE)
    test_function <- if(exists(".prep_get_uncleaned_loggers")) .prep_get_uncleaned_loggers else .prep_get_uncleaned_loggers
    expect_equal(test_function(data), c("91184101", "94184103", "94184102"))
    data_clean <- mc_prep_clean(data, silent=T)
    expect_equal(length(test_function(data_clean)), 0)
})

test_that(".prep_get_utc_localities", {
    not_supported_format_warning(data <- mc_read_files("../data/TOMST", "TOMST", clean=FALSE)) %>%
        not_supported_format_warning() %>%
        not_supported_format_warning()
    test_function <- if(exists(".prep_get_utc_localities")) .prep_get_utc_localities else .prep_get_utc_localities
    expect_equal(test_function(data), c("91184101", "92192250", "94184102", "94184103", "94184104", "94230002"))
    data_clean <- mc_prep_meta_locality(data, list(`91184101`=60, `92192250`=60, `94184102`=60, `94184103`=60, `94184104`=60, `94230002`=60), "tz_offset")
    expect_equal(length(test_function(data_clean)), 0)
})

test_that("mc_prep_meta_sensor", {
    cleaned_data <- mc_read_data("../data/flat/files_table.csv", silent=T)
    cleaned_data <- mc_prep_meta_sensor(cleaned_data, list(TMS_T1="TMS_Tsoil"), "name")
    expect_true("TMS_Tsoil" %in% names(cleaned_data$localities$main$loggers[[1]]$sensors))
    cleaned_data <- mc_prep_meta_sensor(cleaned_data, list(TMS_T2="T2"), param_name = "name", logger_types="TMS")
    expect_true("T2" %in% names(cleaned_data$localities$main$loggers[[1]]$sensors))
    expect_false("T2" %in% names(cleaned_data$localities$main$loggers[[2]]$sensors))
    expect_warning(agg_data <- mc_agg(cleaned_data), "TMS_Tsoil is renamed to TMS_Tsoil_1") %>%
        expect_warning("sensor TMS_T3 is renamed to TMS_T3_1") %>%
        expect_warning("sensor TMS_TMSmoisture is renamed to TMS_TMSmoisture_1")
    agg_data <- mc_prep_meta_sensor(agg_data, list(TMS_T3_1="TMS_T3_secondary"), localities="main", param_name="name")
    expect_true("TMS_T3_secondary" %in% names(agg_data$localities$main$sensors))
    agg_data <- mc_prep_meta_sensor(agg_data, list(TMS_T3_secondary="air"), param_name="height")
    expect_equal(agg_data$localities$main$sensors$TMS_T3_secondary$metadata@height, "air")
})

test_that("mc_prep_meta_sensor wrong", {
    data <- mc_read_data("../data/flat/files_table.csv", clean=FALSE)
    expect_error(data <- mc_prep_meta_sensor(data, list(TMS_T1="TMS_T2"), param_name = "name"))
})

test_that("mc_prep_merge wrong", {
    data <- mc_read_data("../data/TOMST/files_table.csv", clean=FALSE)
    cleaned_data <- mc_prep_clean(data, silent=T)
    agg_data <- mc_agg(cleaned_data)
    expect_error(mc_prep_merge(data, agg_data))
    hour_data <- mc_agg(agg_data, "max", "hour")
    expect_error(mc_prep_merge(agg_data, hour_data))
})

test_that("mc_prep_merge", {
    data1 <- mc_read_files(c("../data/TOMST/data_91184101_0.csv", "../data/TOMST/data_94184102_0.csv"), "TOMST", clean=FALSE)
    data2 <- mc_read_files("../data/TOMST/data_94184103_0.csv", "TOMST", clean=FALSE)
    merged_data <- mc_prep_merge(list(data1, data2))
    test_raw_data_format(merged_data)
    expect_equal(length(merged_data$localities), 3)
    data1 <- mc_prep_clean(data1, silent=T)
    data2 <- mc_prep_clean(data2, silent=T)
    merged_data <- mc_prep_merge(list(data1, data2))
    test_raw_data_format(merged_data)
    expect_equal(length(merged_data$localities), 3)
    hour_data1 <- mc_agg(data1, c("min", "max"), "hour")
    hour_data2 <- mc_agg(data2, c("min", "max"), "hour")
    merged_hour_data <- mc_prep_merge(list(hour_data1, hour_data2))
    test_agg_data_format(merged_hour_data)
    expect_equal(length(merged_hour_data$localities), 3)
})

test_that("mc_prep_merge raw-format same name", {
    data <- mc_read_data("../data/TOMST/files_table.csv", clean=FALSE)
    merged_data <- mc_prep_merge(list(data, data))
    test_raw_data_format(merged_data)
    expect_equal(names(merged_data$localities), c("A1E05", "A2E32", "A6W79"))
    expect_equal(length(merged_data$localities$A1E05$loggers), 2)
})

test_that("mc_prep_merge agg-format same name", {
    data <- mc_read_data("../data/TOMST/files_table.csv", silent=T)
    day_data_1 <- mc_agg(data, "mean", "hour")
    day_data_2 <- mc_agg(data, c("mean", "max"), "hour")
    expect_warning(merged_data <- mc_prep_merge(list(day_data_1, day_data_2)), "sensor .+ is renamed to .+") %>%
        suppressWarnings()
    test_agg_data_format(merged_data)
    expect_equal(names(merged_data$localities), c("A1E05", "A2E32", "A6W79"))
    expect_equal(length(merged_data$localities$A2E32$sensors), 12)
})

test_that("mc_prep_rename_locality wrong", {
    data <- mc_read_data("../data/TOMST/files_table.csv", clean=FALSE)
    expect_error(data <- mc_prep_rename_locality(data, list(A1E05="A6W79")))
})

test_that("mc_prep_calib_load, mc_prep_calib", {
    data <- mc_read_data("../data/TOMST/files_table.csv", clean=FALSE)
    calib_table <- as.data.frame(tibble::tribble(
        ~serial_number,          ~sensor_id,                     ~datetime, ~cor_factor,
        "91184101",              "TS_T", lubridate::ymd_h("2020-10-28 00"),         0.1,
        "91184101",              "TS_T", lubridate::ymd_h("2020-10-28 10"),           0,
        "94184102",            "TMS_T1", lubridate::ymd_h("2020-10-16 00"),        0.12,
        "94184102",            "TMS_T2", lubridate::ymd_h("2020-10-16 01"),        0.15,
        "94184102",            "TMS_T3", lubridate::ymd_h("2020-10-16 00"),         0.2,
        "94184102",   "TMS_TMSmoisture", lubridate::ymd_h("2020-10-16 00"),        0.01,
    ))
    param_data <- mc_prep_calib_load(data, calib_table)
    test_raw_data_format(param_data)
    calib_table <- as.data.frame(tibble::tribble(
        ~serial_number,          ~sensor_id,                         ~datetime, ~cor_factor, ~cor_slope,
            "91184101",              "TS_T", lubridate::ymd_h("2020-10-28 00"),         0.1,          0,
            "91184101",              "TS_T", lubridate::ymd_h("2020-10-28 10"),           0,      -0.05,
            "94184102",            "TMS_T1", lubridate::ymd_h("2020-10-16 00"),        0.12,        0.1,
            "94184102",            "TMS_T2", lubridate::ymd_h("2020-10-16 01"),        0.15,       0.05,
            "94184102",            "TMS_T3", lubridate::ymd_h("2020-10-16 00"),         0.2,          0,
            "94184102",   "TMS_TMSmoisture", lubridate::ymd_h("2020-10-16 00"),        0.01,          0,
    ))
    param_data <- mc_prep_calib_load(data, calib_table)
    test_raw_data_format(param_data)
    expect_error(calib_data <- mc_prep_calib(param_data))
    param_data <- mc_prep_clean(param_data, silent = T)
    calib_data <- mc_prep_calib(param_data)
    test_raw_data_format(calib_data)
    expect_true(calib_data$localities$A1E05$loggers[[1]]$sensors$TS_T$metadata@calibrated)
    expect_equal(calib_data$localities$A1E05$loggers[[1]]$sensors$TS_T$values[[1]], 9.875 + 0.1)
    expect_equal(calib_data$localities$A1E05$loggers[[1]]$sensors$TS_T$values[[6]], 6.875 * 0.95)
    expect_equal(calib_data$localities$A6W79$loggers[[1]]$sensors$TMS_T2$values[[1]], 9.5 * 1.05 + 0.15)
    expect_equal(calib_data$localities$A6W79$loggers[[1]]$sensors$TMS_T2$values[[5]], 9.5 * 1.05 + 0.15)
    expect_true(calib_data$localities$A6W79$loggers[[1]]$sensors$TMS_T3$metadata@calibrated)
    expect_false(calib_data$localities$A6W79$loggers[[1]]$sensors$TMS_TMSmoisture$metadata@calibrated)
    agg_data <- mc_agg(param_data)
    calib_data <- mc_prep_calib(agg_data, sensors = "TS_T")
    test_agg_data_format(calib_data)
    expect_true(calib_data$localities$A1E05$sensors$TS_T$metadata@calibrated)
    expect_equal(calib_data$localities$A1E05$sensors$TS_T$values[[1]], 9.875 + 0.1)
    expect_equal(calib_data$localities$A1E05$sensors$TS_T$values[[6]], 6.875 * 0.95)
})

test_that("mc_prep_fillNA", {
    data <- mc_read_files("../data/agg", "TOMST", clean=F)
    expect_error(mc_prep_fillNA(data))
    data <- mc_prep_clean(data, silent=T)
    data$localities$`91184101`$loggers[[1]]$sensors$TS_T$values[1:4] <- NA_real_
    data$localities$`91184101`$loggers[[1]]$sensors$TS_T$values[50:52] <- NA_real_
    data$localities$`91184101`$loggers[[1]]$sensors$TS_T$values[71:75] <- NA_real_
    data$localities$`91184101`$loggers[[1]]$sensors$TS_T$values[90:95] <- NA_real_
    data$localities$`91184101`$loggers[[1]]$sensors$TS_T$values[[192]] <- NA_real_
    data_dupl <- mc_prep_meta_locality(data, list(`91184101`="abc"), "locality_id")
    data_dupl <- mc_prep_meta_sensor(data_dupl, list(TS_T="T"), "name")
    data <- mc_prep_merge(list(data, data_dupl))
    approx_data <- mc_prep_fillNA(data, maxgap=5, localities="91184101")
    expect_true(is.na(approx_data$localities$`91184101`$loggers[[1]]$sensors$TS_T$values[[1]]))
    expect_false(is.na(approx_data$localities$`91184101`$loggers[[1]]$sensors$TS_T$values[[51]]))
    expect_false(is.na(approx_data$localities$`91184101`$loggers[[1]]$sensors$TS_T$values[[71]]))
    expect_true(is.na(approx_data$localities$`91184101`$loggers[[1]]$sensors$TS_T$values[[95]]))
    expect_true(is.na(approx_data$localities$`91184101`$loggers[[1]]$sensors$TS_T$values[[192]]))
    expect_true(is.na(approx_data$localities$abc$loggers[[1]]$sensors$T$values[[50]]))
    approx_data <- mc_prep_fillNA(data, maxgap=5, sensors="T")
    expect_false(is.na(approx_data$localities$abc$loggers[[1]]$sensors$T$values[[50]]))
    expect_true(is.na(approx_data$localities$`91184101`$loggers[[1]]$sensors$TS_T$values[[50]]))
})

