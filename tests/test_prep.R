library(testthat)
library(myClim)
source("test.R")

test_that("mc_prep_clean", {
    data <- mc_read_directory("data/clean-datetime_step", "TOMST")
    cleaned_data <- mc_prep_clean(data, silent=T)
    test_prep_data_format(cleaned_data)
    expect_equal(cleaned_data[["94184102"]]$loggers[[1]]$clean_info@count_duplicits, 1)
    expect_equal(cleaned_data[["94184102"]]$loggers[[1]]$clean_info@count_missed, 2)
    expect_equal(cleaned_data[["94184165"]]$loggers[[1]]$clean_info@count_duplicits, 25)
    expect_equal(cleaned_data[["94184169"]]$loggers[[1]]$clean_info@count_disordered, 1)
    diff_datetime <- diff(as.numeric(cleaned_data[["94184169"]]$loggers[[1]]$datetime)) %/% 60
    expect_equal(diff_datetime, rep(15, 5))
    diff_datetime <- diff(as.numeric(cleaned_data[["94184170"]]$loggers[[1]]$datetime)) %/% 60
    expect_equal(diff_datetime, rep(15, 5))
    test_function <- if(exists(".prep_is_logger_cleaned")) .prep_is_logger_cleaned else myClim:::.prep_is_logger_cleaned
    expect_true(test_function(cleaned_data[["94184102"]]$loggers[[1]]))
    expect_true(myClim:::.prep_clean_was_error_in_logger(cleaned_data[["94184102"]]$loggers[[1]]))
    expect_equal(length(cleaned_data[["94184102"]]$loggers[[1]]$datetime), 49)
    expect_true(is.na(cleaned_data[["94184102"]]$loggers[[1]]$sensors$TMS_T1$values[[19]]))
    expect_equal(cleaned_data[["91184133"]]$loggers[[1]]$clean_info@step, 15)
})

test_that("mc_prep_clean one record", {
    data <- mc_read_directory("data/clean-one-record", "TOMST")
    expect_warning(cleaned_data <- mc_prep_clean(data, silent=T))
    expect_true(is.na(cleaned_data[["94208611"]]$loggers[[1]]$clean_info@step))
    expect_true(is.na(cleaned_data[["94208611"]]$loggers[[1]]$clean_info@count_duplicits))
    expect_true(is.na(cleaned_data[["94208611"]]$loggers[[1]]$clean_info@count_disordered))
    expect_true(is.na(cleaned_data[["94208611"]]$loggers[[1]]$clean_info@count_missed))
})

test_that("mc_prep_clean ok", {
    data <- mc_read_files("data/TOMST/data_94184102_0.csv", "TOMST")
    cleaned_data <- mc_prep_clean(data, silent=T)
    expect_true(myClim:::.prep_is_logger_cleaned(cleaned_data[[1]]$loggers[[1]]))
    expect_false(myClim:::.prep_clean_was_error_in_logger(cleaned_data[[1]]$loggers[[1]]))
})

test_that("mc_prep_solar_tz", {
    data <- mc_read_csv("data/TOMST/files_table.csv", "data/TOMST/localities_table.csv")
    data <- mc_prep_solar_tz(data)
    test_prep_data_format(data)
    expect_equal(data$A1E05$metadata@tz_offset, 57)
})

test_that("mc_prep_user_tz", {
    data <- mc_read_csv("data/TOMST/files_table.csv", "data/TOMST/localities_table.csv")
    data <- mc_prep_user_tz(data, list(A1E05=50))
    test_prep_data_format(data)
    expect_equal(data$A1E05$metadata@tz_offset, 50)
})

test_that("mc_prep_crop", {
    data <- mc_read_csv("data/TOMST/files_table.csv", "data/TOMST/localities_table.csv")
    cropped_data <- mc_prep_crop(data, start=as.POSIXct("2020-10-16 08:00", tz="UTC"))
    test_prep_data_format(cropped_data)
    expect_equal(length(cropped_data$A2E32$loggers[[1]]$datetime), 68)
    expect_equal(length(cropped_data$A2E32$loggers[[1]]$sensors$TMS_T1$values), 68)
    cropped_data <- mc_prep_crop(data, end=as.POSIXct("2020-10-16 08:00", tz="UTC"))
    test_prep_data_format(cropped_data)
    expect_equal(length(cropped_data$A2E32$loggers[[1]]$datetime), 8)
    expect_equal(length(cropped_data$A2E32$loggers[[1]]$sensors$TMS_T1$values), 8)
    cropped_data <- mc_prep_crop(data, end=as.POSIXct("2020-10-16 08:00", tz="UTC"), end_included=FALSE)
    test_prep_data_format(cropped_data)
    expect_equal(length(cropped_data$A2E32$loggers[[1]]$datetime), 7)
    expect_equal(length(cropped_data$A2E32$loggers[[1]]$sensors$TMS_T1$values), 7)
    data_clean <- mc_prep_clean(data, silent=T)
    data_calc <- mc_agg(data_clean)
    cropped_calc_data <- mc_prep_crop(data_calc, start=as.POSIXct("2020-10-16 06:00", tz="UTC"), end=as.POSIXct("2020-10-16 08:00", tz="UTC"))
    test_calc_data_format(cropped_calc_data)
})

test_that(".prep_get_loggers_datetime_step_unprocessed", {
    data <- mc_read_csv("data/TOMST/files_table.csv", "data/TOMST/localities_table.csv")
    test_function <- if(exists(".prep_get_uncleaned_loggers")) .prep_get_uncleaned_loggers else myClim:::.prep_get_uncleaned_loggers
    expect_equal(test_function(data), c("91184101", "94184103", "94184102"))
    data_clean <- mc_prep_clean(data, silent=T)
    expect_equal(length(test_function(data_clean)), 0)
})

test_that(".prep_get_utc_localities", {
    expect_warning(data <- mc_read_directory("data/TOMST", "TOMST"))
    test_function <- if(exists(".prep_get_utc_localities")) .prep_get_utc_localities else myClim:::.prep_get_utc_localities
    expect_equal(test_function(data), c("91184101", "94184102", "94184103", "94184104"))
    data_clean <- mc_prep_user_tz(data, list(`91184101`=60, `94184102`=60, `94184103`=60, `94184104`=60))
    expect_equal(length(test_function(data_clean)), 0)
})

test_that("mc_prep_rename_sensor", {
    data <- mc_read_csv("data/flat/files_table.csv")
    cleaned_data <- mc_prep_clean(data, silent=T)
    cleaned_data <- mc_prep_rename_sensor(cleaned_data, list(TMS_T1="TMS_Tsoil"))
    expect_true("TMS_Tsoil" %in% names(cleaned_data$main$loggers[[1]]$sensors))
    cleaned_data <- mc_prep_rename_sensor(cleaned_data, list(TMS_T2="T2"), serial_numbers="94184102")
    expect_true("T2" %in% names(cleaned_data$main$loggers[[1]]$sensors))
    expect_false("T2" %in% names(cleaned_data$main$loggers[[2]]$sensors))
    expect_warning(calc_data <- mc_agg(cleaned_data))
    calc_data <- mc_prep_rename_sensor(calc_data, list(TMS_T3_1="TMS_T3_secondary"), localities="main")
    expect_true("TMS_T3_secondary" %in% names(calc_data$localities$main$sensors))
})
