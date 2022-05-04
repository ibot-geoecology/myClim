library(testthat)
source("test.R")

test_that("mc_read_data csv without localities", {
    data <- mc_read_data("data/TOMST/files_table.csv")
    test_prep_data_format(data)
    expect_equal(length(data), 3)
    expect_equal(length(data$A6W79$loggers[[1]]$datetime), 49)
    expect_equal(data$A6W79$metadata@tz_type, mc_const_TZ_UTC)
    expect_equal(length(data$A6W79$loggers[[1]]$sensors), 4)
    expect_equal(data$A6W79$loggers[[1]]$metadata@type, "TMS")
    expect_equal(data$A6W79$loggers[[1]]$metadata@serial_number, "94184102")
    expect_equal(length(data$A2E32$loggers[[1]]$datetime), 75)
    expect_equal(length(data$A2E32$loggers[[1]]$sensors), 4)
    expect_equal(length(data$A2E32$loggers[[1]]$sensors$TMS_T1), 4)
    expect_equal(length(data$A1E05$loggers[[1]]$datetime), 11)
    expect_equal(length(data$A1E05$loggers[[1]]$sensors), 1)
    expect_true(is.na(data$A1E05$metadata@altitude))
})

test_that("mc_read_data TOMST format datetime", {
    table <- data.frame(path="data/format/201911_93164272.csv", locality_id="AAA",
                        data_format="TOMST", serial_number="93164272")
    data <- mc_read_data(table)
    test_prep_data_format(data)
})

test_that("mc_read_data csv with localities", {
    data <- mc_read_data("data/TOMST/files_table.csv", "data/TOMST/localities_table.csv")
    test_prep_data_format(data)
    expect_equal(data$A1E05$metadata@altitude, 255)
    expect_equal(data$A6W79$metadata@tz_type, mc_const_TZ_USER_DEFINED)
})

test_that("mc_read_files TOMST directory", {
    expect_warning(data <- mc_read_files(c("data/TOMST", "data/eco-snow"), "TOMST"))
    test_prep_data_format(data)
    expect_equal(data[[1]]$metadata@tz_type, mc_const_TZ_UTC)
    expect_equal(length(data), 4)
    expect_equal(length(data[[1]]$loggers), 1)
})

test_that("mc_read_files HOBO", {
    data <- mc_read_files(c("data/HOBO/20024354_comma.csv", "data/HOBO/20024354_semicolon.txt", "data/HOBO/20024354_tab.txt"),
                          "HOBO",  date_format = "%y.%m.%d %H:%M:%S", tz_offset = 120)
    test_prep_data_format(data)
})

test_that("mc_read_data HOBO", {
    files_table <- as.data.frame(tibble::tribble(
        ~path,                                   ~locality_id, ~data_format, ~serial_number, ~date_format,        ~tz_offset,
        "data/HOBO/20024354.txt",                "A",          "HOBO",       NA_character_,  "%d.%m.%Y %H:%M:%S", NA_integer_,
        "data/HOBO/20024354_comma.csv",          "B",          "HOBO",       NA_character_,  "%y.%m.%d %H:%M:%S", NA_integer_,
        "data/HOBO/20024354_fahrenheit.csv",     "C",          "HOBO",       NA_character_,  "%m.%d.%y %H:%M:%S", NA_integer_,
        "data/HOBO/20024354_minimal.csv",        "D",          "HOBO",       "20024356",     "%y.%m.%d %H:%M:%S", 120,
        "data/HOBO/20024354_minimal_title.csv",  "E",          "HOBO",       "20024356",     "%y.%m.%d %H:%M:%S", 120,
        "data/HOBO/20024354_semicolon.txt",      "F",          "HOBO",       NA_character_,  "%y.%m.%d %H:%M:%S", NA_integer_,
        "data/HOBO/20024354_separated_CEST.csv", "G",          "HOBO",       NA_character_,  "%y.%m.%d",          NA_integer_,
        "data/HOBO/20024354_separeted.csv",      "H",          "HOBO",       "20024356",     "%y.%m.%d",          NA_integer_,
        "data/HOBO/20024354_tab.txt",            "CH",         "HOBO",       NA_character_,  "%y.%m.%d %H:%M:%S", NA_integer_,
    ))
    expect_warning(data <- mc_read_data(files_table))
    test_prep_data_format(data)
    expect_equal(sort(names(data)), sort(c("A", "B", "C", "D", "E", "F", "CH")))
    expect_true(var(c(data$A$loggers[[1]]$datetime[[1]],
                      data$B$loggers[[1]]$datetime[[1]],
                      data$C$loggers[[1]]$datetime[[1]],
                      data$D$loggers[[1]]$datetime[[1]],
                      data$E$loggers[[1]]$datetime[[1]],
                      data$F$loggers[[1]]$datetime[[1]],
                      data$CH$loggers[[1]]$datetime[[1]])) == 0)
    expect_true(myClim:::.model_const_SENSOR_HOBO_T_F %in% names(data$C$loggers[[1]]$sensors))
})

test_that("mc_read_files error", {
    expect_error(data <- mc_read_files(c("data/TOMST", "data/eco-snow/data_94184102_0.csv"), "TOMST"))
})

test_that("mc_read_files TOMST comma in number", {
    data <- mc_read_files(c("data/comma_TOMST/data_91212414_0.csv", "data/comma_TOMST/data_94214606_0.csv"), "TOMST")
    test_prep_data_format(data)
})

test_that("mc_read_files joined TOMST direcory", {
    data <- mc_read_files("data/joined_TOMST", "TOMST_join")
    test_prep_data_format(data)
    expect_equal(names(data), c("A1E01_TS", "A1W14_TMS", "A4E53_TMS", "CKras_Loc_2_15", "CZ2_HRADEC_TMS", "CZ2_HRADEC_TS"))
    expect_equal(names(data$A1W14_TMS$loggers[[1]]$sensors), c("TMS_T1", "TMS_T2", "TMS_T3", "TMS_TMSmoisture"))
    expect_equal(names(data$CZ2_HRADEC_TMS$loggers[[1]]$sensors), c("TMS_T1", "TMS_T2", "TMS_T3", "TMS_TMSmoisture", "moisture"))
    expect_equal(names(data$CZ2_HRADEC_TS$loggers[[1]]$sensors), "TM_T")
})

test_that("mc_read_wide", {
    data_table <- readRDS("data/read_table/precip.Rds")
    expect_error(data <- mc_read_wide(data_table, myClim:::.model_const_SENSOR_precipitation))
    dates <- data_table$date
    data_table$date <- as.POSIXct(lubridate::ymd(dates))
    expect_error(data <- mc_read_wide(data_table, myClim:::.model_const_SENSOR_precipitation))
    data_table$date <- as.POSIXct(lubridate::ymd(dates, tz="UTC"))
    data <- mc_read_wide(data_table, myClim:::.model_const_SENSOR_precipitation)
    test_prep_data_format(data)
    expect_equal(length(data), 49)
    expect_equal(names(data$B1BLAT01$loggers[[1]]$sensors), myClim:::.model_const_SENSOR_precipitation)
    expect_equal(data$B1BLAT01$loggers[[1]]$sensors[[1]]$values[[1]], 0.7)
})

test_that("mc_read_long", {
    data_table_precip <- readRDS("data/read_table/precip.Rds")[1:10]
    data_table_precip <- tidyr::gather(data_table_precip, "locality_id", "value", -date)
    data_table_precip$sensor_name <- "precip"
    data_table_fresh_snow <- readRDS("data/read_table/fresh_snow.Rds")[1:10]
    data_table_fresh_snow <- tidyr::gather(data_table_fresh_snow, "locality_id", "value", -date)
    data_table_fresh_snow$sensor_name <- "snow_fresh"
    data_table <- dplyr::union_all(data_table_precip, data_table_fresh_snow)
    data_table <- dplyr::rename(data_table, datetime=date)
    data_table <- dplyr::select(data_table, locality_id, sensor_name, datetime, value)
    data_table$datetime <- as.POSIXct(lubridate::ymd(data_table$datetime, tz="UTC"))
    data <- mc_read_long(data_table, list(precip="precipitation"))
    test_prep_data_format(data)
    expect_equal(length(data), 9)
})
