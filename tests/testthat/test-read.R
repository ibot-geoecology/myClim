source("libtest.R")

test_that("mc_read_data csv without localities", {
    data <- mc_read_data("../data/TOMST/files_table2.csv", clean=FALSE)
    test_raw_data_format(data)
    expect_equal(length(data$localities), 3)
    expect_equal(length(data$localities$A6W79$loggers[[1]]$datetime), 49)
    expect_equal(data$localities$A6W79$metadata@tz_type, myClim:::.model_const_TZ_UTC)
    expect_equal(length(data$localities$A6W79$loggers[[1]]$sensors), 4)
    expect_equal(data$localities$A6W79$loggers[[1]]$sensors$TMS_T1$metadata@height,
                 mc_data_heights$height[mc_data_heights$logger_type == .model_const_LOGGER_TOMST_TMS_L45 &
                                        mc_data_heights$sensor_name == .model_const_SENSOR_TMS_T1])
    expect_equal(data$localities$A6W79$loggers[[1]]$metadata@type, .model_const_LOGGER_TOMST_TMS_L45)
    expect_equal(data$localities$A6W79$loggers[[1]]$metadata@serial_number, "94184102")
    expect_equal(data$localities$A6W79$loggers[[1]]$metadata@step, 15)
    expect_equal(data$localities$A2E32$loggers[[1]]$metadata@type, .model_const_LOGGER_TOMST_TMS)
    expect_equal(length(data$localities$A2E32$loggers[[1]]$datetime), 75)
    expect_equal(length(data$localities$A2E32$loggers[[1]]$sensors), 4)
    expect_equal(length(data$localities$A2E32$loggers[[1]]$sensors$TMS_T1), 4)
    expect_equal(length(data$localities$A1E05$loggers[[1]]$datetime), 11)
    expect_equal(length(data$localities$A1E05$loggers[[1]]$sensors), 1)
    expect_true(is.na(data$localities$A1E05$loggers[[1]]$metadata@step))
    expect_true(is.na(data$localities$A1E05$metadata@elevation))
    expect_equal(data$localities$A1E05$loggers[[1]]$sensors$TS_T$states$tag, myClim:::.model_const_SENSOR_STATE_SOURCE)
    expect_equal(data$localities$A1E05$loggers[[1]]$sensors$TS_T$states$start, dplyr::first(data$localities$A1E05$loggers[[1]]$datetime))
    expect_equal(data$localities$A1E05$loggers[[1]]$sensors$TS_T$states$end, dplyr::last(data$localities$A1E05$loggers[[1]]$datetime))
    expect_equal(data$localities$A1E05$loggers[[1]]$sensors$TS_T$states$value, normalizePath("../data/TOMST/data_91184101_0.csv"))
    expect_equal(data$localities$A1E05$loggers[[2]]$metadata@serial_number, "94230002")
})

test_that("mc_read_data TOMST format datetime", {
    table <- data.frame(path= "../data/format/201911_93164272.csv", locality_id="AAA",
                        data_format="TOMST", serial_number="93164272")
    data <- mc_read_data(table, clean=FALSE)
    test_raw_data_format(data)
})

test_that("mc_read_data csv with localities", {
    data <- mc_read_data("../data/TOMST/files_table2.csv", "../data/TOMST/localities_table.csv", clean=FALSE)
    test_raw_data_format(data)
    expect_equal(data$localities$A1E05$metadata@elevation, 255)
    expect_equal(data$localities$A6W79$metadata@tz_type, myClim:::.model_const_TZ_USER_DEFINED)
    expect_equal(data$localities$A6W79$loggers[[1]]$metadata@type, .model_const_LOGGER_TOMST_TMS_L45)
    expect_equal(data$localities$A6W79$loggers[[1]]$sensors$TMS_T1_L45$metadata@height,
                 mc_data_heights$height[mc_data_heights$logger_type == .model_const_LOGGER_TOMST_TMS_L45 &
                                            mc_data_heights$sensor_name == .model_const_SENSOR_TMS_T1])
})

test_that("mc_read_files TOMST directory", {
    not_supported_format_warning(data <- mc_read_files(c("../data/TOMST", "../data/eco-snow"), "TOMST", clean=FALSE)) %>%
        not_supported_format_warning() %>%
        not_supported_format_warning()
    test_raw_data_format(data)
    expect_equal(data$localities[[1]]$metadata@tz_type, myClim:::.model_const_TZ_UTC)
    expect_equal(length(data$localities), 6)
    expect_equal(length(data$localities[[1]]$loggers), 1)
    expect_equal(data$localities$`92192250`$loggers[[1]]$metadata@type, .model_const_LOGGER_TOMST_DENDROMETER)
    expect_equal(length(data$localities$`92192250`$loggers[[1]]$sensors), 2)
})

test_that("mc_read_files HOBO", {
    data <- mc_read_files(c("../data/HOBO/20024354_comma.csv", "../data/HOBO/20024354_semicolon.txt", "../data/HOBO/20024354_tab.txt"),
                          "HOBO",  date_format = "%y.%m.%d %H:%M:%S", tz_offset = 120, clean=FALSE)
    test_raw_data_format(data)
})

test_that("mc_read_data HOBO", {
    files_table <- as.data.frame(tibble::tribble(
        ~path, ~locality_id, ~data_format, ~serial_number, ~date_format, ~tz_offset,
        "../data/HOBO/20024354.txt", "A", "HOBO", NA_character_, "%d.%m.%Y %H:%M:%S", NA_integer_,
        "../data/HOBO/20024354_comma.csv", "B", "HOBO", NA_character_, "%y.%m.%d %H:%M:%S", NA_integer_,
        "../data/HOBO/20024354_fahrenheit.csv", "C", "HOBO", NA_character_, "%m.%d.%y %H:%M:%S", NA_integer_,
        "../data/HOBO/20024354_minimal.csv", "D", "HOBO", "20024356", "%y.%m.%d %H:%M:%S", 120,
        "../data/HOBO/20024354_minimal_title.csv", "E", "HOBO", "20024356", "%y.%m.%d %H:%M:%S", 120,
        "../data/HOBO/20024354_semicolon.txt", "F", "HOBO", NA_character_, "%y.%m.%d %H:%M:%S", NA_integer_,
        "../data/HOBO/20024354_separated_CEST.csv", "G", "HOBO", NA_character_, "%y.%m.%d", NA_integer_,
        "../data/HOBO/20024354_separeted.csv", "H", "HOBO", "20024356", "%y.%m.%d", NA_integer_,
        "../data/HOBO/20024354_tab.txt", "CH", "HOBO", NA_character_, "%y.%m.%d %H:%M:%S", NA_integer_,
        "../data/HOBO/6265.csv", "I", "HOBO", NA_character_, "%m/%d/%y %I:%M:%S %p", NA_integer_,
    ))
    not_supported_format_warning(data <- mc_read_data(files_table, clean=FALSE)) %>%
        not_supported_format_warning() %>%
        expect_warning("Separated time in source data isn't supported.") %>%
        expect_warning("Separated time in source data isn't supported.")
    test_raw_data_format(data)
    expect_equal(sort(names(data$localities)), sort(c("A", "B", "C", "D", "E", "F", "CH", "I")))
    expect_true(var(c(data$localities$A$loggers[[1]]$datetime[[1]],
                      data$localities$B$loggers[[1]]$datetime[[1]],
                      data$localities$C$loggers[[1]]$datetime[[1]],
                      data$localities$D$loggers[[1]]$datetime[[1]],
                      data$localities$E$loggers[[1]]$datetime[[1]],
                      data$localities$F$loggers[[1]]$datetime[[1]],
                      data$localities$CH$loggers[[1]]$datetime[[1]])) == 0)
    expect_true(.model_const_SENSOR_HOBO_T_F %in% names(data$localities$C$loggers[[1]]$sensors))
    expect_equal(length(data$localities$A$loggers[[1]]$sensors), 2)
    expect_equal(length(data$localities$I$loggers[[1]]$sensors), 1)
    cleaned_data <- mc_prep_clean(data, silent = T)
    clean_info <- mc_info_clean(cleaned_data)
    expect_true(all(clean_info$count_duplicities == 0))
    expect_true(all(clean_info$count_missing == 0))
})

test_that("mc_read_data HOBO skip wrong datetime", {
    files_table <- as.data.frame(tibble::tribble(
        ~path, ~locality_id, ~data_format, ~serial_number, ~date_format, ~tz_offset,
        "../data/HOBO/20024354.txt", "A", "HOBO", NA_character_, "%d.%m.%Y %H:%M:%S", NA_integer_,
        "../data/HOBO/20024354_comma.csv", "B", "HOBO", NA_character_, "%m.%d.%Y %H:%M:%S", NA_integer_
    ))
    expect_warning(data <- mc_read_data(files_table, clean=FALSE))
    test_raw_data_format(data)
    expect_equal(length(data$localities), 1)
})

test_that("mc_read_files error", {
    expect_error(data <- mc_read_files(c("../data/TOMST", "data/eco-snow/data_94184102_0.csv"), "TOMST", clean=FALSE))
})

test_that("mc_read_files TOMST comma in number", {
    data <- mc_read_files(c("../data/comma_TOMST/data_91212414_0.csv",
                            "../data/comma_TOMST/data_94214606_0.csv"), "TOMST", clean=FALSE)
    test_raw_data_format(data)
})

test_that("mc_read_files TOMST with error in data", {
    expect_warning(data <- mc_read_files("../data/TOMST-error", "TOMST", clean=FALSE))
    expect_true(is.na(data$localities$data_93142777$loggers[[1]]$sensors$TMS_T2$values[[7]]))
    states <- dplyr::filter(data$localities$data_93142777$loggers[[1]]$sensors$TMS_T2$states, .data$tag == .model_const_SENSOR_STATE_ERROR)
    expect_equal(nrow(states), 6)
    expect_equal(states$start, c(lubridate::ymd_hm("2022-02-24 06:00"),
                                 lubridate::ymd_hm("2022-02-24 07:15"),
                                 lubridate::ymd_hm("2022-02-24 09:00"),
                                 lubridate::ymd_hm("2022-02-24 09:45"),
                                 lubridate::ymd_hm("2022-02-24 10:15"),
                                 lubridate::ymd_hm("2022-02-24 11:15")))
    expect_equal(states$end, c(lubridate::ymd_hm("2022-02-24 06:45"),
                               lubridate::ymd_hm("2022-02-24 08:30"),
                               lubridate::ymd_hm("2022-02-24 09:15"),
                               lubridate::ymd_hm("2022-02-24 09:45"),
                               lubridate::ymd_hm("2022-02-24 10:45"),
                               lubridate::ymd_hm("2022-02-24 16:30")))
})

test_that("mc_read_files joined TOMST direcory", {
    data <- mc_read_files("../data/joined_TOMST", "TOMST_join", clean=FALSE)
    test_raw_data_format(data)
    expect_equal(names(data$localities), c("A1E01_TS", "A1W14_TMS", "A4E53_TMS", "CKras_Loc_2_15", "CZ2_HRADEC_TMS", "CZ2_HRADEC_TS"))
    expect_equal(names(data$localities$A1W14_TMS$loggers[[1]]$sensors), c("TMS_T1", "TMS_T2", "TMS_T3", "TMS_TMSmoisture"))
    expect_equal(names(data$localities$CZ2_HRADEC_TMS$loggers[[1]]$sensors), c("TMS_T1", "TMS_T2", "TMS_T3", "TMS_TMSmoisture", "moisture"))
    expect_equal(names(data$localities$CZ2_HRADEC_TS$loggers[[1]]$sensors), "TS_T")
})

test_that("mc_read_wide", {
    data_table <- readRDS("../data/read_table/precip.Rds")
    expect_error(data <- mc_read_wide(data_table, .model_const_SENSOR_precipitation, clean=FALSE))
    dates <- data_table$date
    data_table$date <- as.POSIXct(lubridate::ymd(dates), tz="CET")
    expect_error(data <- mc_read_wide(data_table, .model_const_SENSOR_precipitation, clean=FALSE))
    data_table$date <- as.POSIXct(lubridate::ymd(dates, tz="UTC"))
    data <- mc_read_wide(data_table, .model_const_SENSOR_precipitation, clean=FALSE)
    test_raw_data_format(data)
    expect_equal(length(data$localities), 49)
    expect_equal(names(data$localities$B1BLAT01$loggers[[1]]$sensors), .model_const_SENSOR_precipitation)
    expect_equal(data$localities$B1BLAT01$loggers[[1]]$sensors[[1]]$values[[1]], 0.7)
})

test_that("mc_read_long", {
    data_table_precip <- readRDS("../data/read_table/precip.Rds")[1:10]
    data_table_precip <- tidyr::gather(data_table_precip, "locality_id", "value", -date)
    data_table_precip$sensor_name <- "precip"
    data_table_fresh_snow <- readRDS("../data/read_table/fresh_snow.Rds")[1:10]
    data_table_fresh_snow <- tidyr::gather(data_table_fresh_snow, "locality_id", "value", -date)
    data_table_fresh_snow$sensor_name <- "snow_fresh"
    data_table <- dplyr::union_all(data_table_precip, data_table_fresh_snow)
    data_table <- dplyr::rename(data_table, datetime=date)
    data_table <- dplyr::select(data_table, locality_id, sensor_name, datetime, value)
    data_table$datetime <- as.POSIXct(lubridate::ymd(data_table$datetime, tz="UTC"))
    data <- mc_read_long(data_table, list(precip="precipitation"), clean=FALSE)
    test_raw_data_format(data)
    expect_equal(length(data$localities), 9)
})

test_that("mc_read_files TOMST serial_number", {
    expect_warning(data <- mc_read_files("../data/format/201911_93164272.csv", "TOMST", clean=FALSE))
    expect_equal(names(data$localities), "201911_93164272")
})

