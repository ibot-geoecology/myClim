source("libtest.R")

test_that("mc_read_data csv without localities", {
    data <- mc_read_data("../data/TOMST/files_table2.csv", clean=FALSE)
    test_raw_data_format(data)
    expect_equal(length(data$localities), 3)
    expect_equal(length(data$localities$A6W79$loggers[["TMS_L45_1"]]$datetime), 49)
    expect_equal(data$localities$A6W79$metadata@tz_type, myClim:::.model_const_TZ_UTC)
    expect_equal(length(data$localities$A6W79$loggers[[1]]$sensors), 4)
    expect_equal(data$localities$A6W79$loggers[["TMS_L45_1"]]$sensors$TMS_T1$metadata@height,
                 mc_data_heights$height[mc_data_heights$logger_type == .model_const_LOGGER_TOMST_TMS_L45 &
                                        mc_data_heights$sensor_name == mc_const_SENSOR_TMS_T1])
    expect_equal(data$localities$A6W79$loggers[["TMS_L45_1"]]$metadata@type, .model_const_LOGGER_TOMST_TMS_L45)
    expect_equal(data$localities$A6W79$loggers[["TMS_L45_1"]]$metadata@serial_number, "94184102")
    expect_equal(data$localities$A6W79$loggers[["TMS_L45_1"]]$metadata@step, 15)
    expect_equal(data$localities$A2E32$loggers[["TMS_1"]]$metadata@type, .model_const_LOGGER_TOMST_TMS)
    expect_equal(length(data$localities$A2E32$loggers[["TMS_1"]]$datetime), 75)
    expect_equal(length(data$localities$A2E32$loggers[["TMS_1"]]$sensors), 4)
    expect_equal(length(data$localities$A2E32$loggers[["TMS_1"]]$sensors$TMS_T1), 4)
    expect_equal(length(data$localities$A1E05$loggers[["Thermo_1"]]$datetime), 11)
    expect_equal(length(data$localities$A1E05$loggers[["Thermo_1"]]$sensors), 1)
    expect_true(is.na(data$localities$A1E05$loggers[["Thermo_1"]]$metadata@step))
    expect_true(is.na(data$localities$A1E05$metadata@elevation))
    expect_equal(data$localities$A1E05$loggers[["Thermo_1"]]$sensors$Thermo_T$states$tag, myClim:::.model_const_SENSOR_STATE_SOURCE)
    expect_equal(data$localities$A1E05$loggers[["Thermo_1"]]$sensors$Thermo_T$states$start, dplyr::first(data$localities$A1E05$loggers[[1]]$datetime))
    expect_equal(data$localities$A1E05$loggers[["Thermo_1"]]$sensors$Thermo_T$states$end, dplyr::last(data$localities$A1E05$loggers[[1]]$datetime))
    expect_equal(data$localities$A1E05$loggers[["Thermo_1"]]$sensors$Thermo_T$states$value, normalizePath("../data/TOMST/data_91184101_0.csv"))
    expect_equal(data$localities$A1E05$loggers[["TMS_1"]]$metadata@serial_number, "94230002")
})

test_that("mc_read_data missed file", {
    table <- read.csv("../data/TOMST/files_table2.csv")
    expect_warning(data <- mc_read_data(table, clean=FALSE), "File data_94184103_0.csv does not exist - skipping.") %>%
        expect_warning("File data_91184101_0.csv does not exist - skipping.") %>%
        expect_warning("File data_94230002_2022_06_10_0.csv does not exist - skipping.")
    test_raw_data_format(data)
    expect_equal(length(data), 1)
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
    expect_equal(data$localities$A6W79$loggers[["TMS_L45_1"]]$metadata@type, .model_const_LOGGER_TOMST_TMS_L45)
    expect_equal(data$localities$A6W79$loggers[["TMS_L45_1"]]$sensors$TMS_T1_L45$metadata@height,
                 mc_data_heights$height[mc_data_heights$logger_type == .model_const_LOGGER_TOMST_TMS_L45 &
                                            mc_data_heights$sensor_name == mc_const_SENSOR_TMS_T1])
})

test_that("mc_read_data csv with localities and metadata", {
    data <- mc_read_data("../data/TOMST/files_table.csv", "../data/TOMST2/localities_table_metadata.csv", clean=FALSE)
    test_raw_data_format(data)
    expect_true(is.na(data$localities$A1E05$metadata@elevation))
    expect_equal(data$localities$A2E32$metadata@user_data$description, "")
    expect_equal(data$localities$A6W79$metadata@user_data$description, "most important")
    expect_equal(data$localities$A6W79$metadata@user_data$distance, 10)
})

test_that("mc_read_files TOMST directory", {
    not_applicable_format_warning(data <- mc_read_files(c("../data/TOMST", "../data/eco-snow"), "TOMST", clean=FALSE)) %>%
        not_applicable_format_warning() %>%
        not_applicable_format_warning()
    test_raw_data_format(data)
    expect_equal(data$localities[[1]]$metadata@tz_type, myClim:::.model_const_TZ_UTC)
    expect_equal(length(data$localities), 6)
    expect_equal(length(data$localities[[1]]$loggers), 1)
    expect_equal(data$localities$`92192250`$loggers[["Dendro_1"]]$metadata@type, .model_const_LOGGER_TOMST_DENDROMETER)
    expect_equal(length(data$localities$`92192250`$loggers[["Dendro_1"]]$sensors), 2)
})

test_that("mc_read_data TOMST 2024 format changes", {
    data <- mc_read_files("../data/TOMST-2024", dataformat_name="TOMST", clean=FALSE)
    test_raw_data_format(data)
})

test_that("mc_read_files HOBO", {
    data <- mc_read_files(c("../data/HOBO/20024354_comma.csv", "../data/HOBO/20024354_semicolon.txt", "../data/HOBO/20024354_tab.txt"),
                          "HOBO",  date_format = "%y.%m.%d %H:%M:%S", tz_offset = 120, clean=FALSE)
    test_raw_data_format(data)
    data <- mc_read_files(c("../data/HOBO/20024354_comma.csv", "../data/HOBO/20024354_semicolon.txt", "../data/HOBO/20024354_tab.txt"),
                          "HOBO",  date_format = "%y.%m.%d %H:%M:%S@%y.%m.%d", tz_offset = 120, clean=FALSE)
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
        "../data/HOBO/20024370.txt", "J", "HOBO", NA_character_, "%d.%m.%Y %H:%M:%S", NA_integer_,
        "../data/HOBO/2015_10382557.txt", "K", "HOBO", NA_character_, "%d.%m.%Y %H:%M:%S", NA_integer_,
    ))
    not_applicable_format_warning(data <- mc_read_data(files_table, clean=FALSE)) %>%
        not_applicable_format_warning() %>%
        expect_warning("Separated time in source data isn't supported.") %>%
        expect_warning("Temperature data in °F is converted to °C.") %>%
        expect_warning("Separated time in source data isn't supported.")
    test_raw_data_format(data)
    expect_equal(sort(names(data$localities)), sort(c("A", "B", "C", "D", "E", "F", "CH", "I", "J", "K")))
    expect_true(var(c(data$localities$A$loggers[["HOBO_U23-001A_1"]]$datetime[[1]],
                      data$localities$B$loggers[["HOBO_U23-001A_1"]]$datetime[[1]],
                      data$localities$C$loggers[["HOBO_U23-001A_1"]]$datetime[[1]],
                      data$localities$D$loggers[["HOBO_U23-001A_1"]]$datetime[[1]],
                      data$localities$E$loggers[["HOBO_U23-001A_1"]]$datetime[[1]],
                      data$localities$F$loggers[["HOBO_U23-001A_1"]]$datetime[[1]],
                      data$localities$CH$loggers[["HOBO_U23-001A_1"]]$datetime[[1]])) == 0)
    expect_true(mc_const_SENSOR_HOBO_T %in% names(data$localities$C$loggers[["HOBO_U23-001A_1"]]$sensors))
    expect_equal(data$localities$C$loggers[["HOBO_U23-001A_1"]]$sensors$HOBO_T$values[[1]], 5 * (65.788 - 32) / 9)
    expect_equal(length(data$localities$A$loggers[["HOBO_U23-001A_1"]]$sensors), 2)
    expect_equal(length(data$localities$I$loggers[["Logger_1"]]$sensors), 1)
    cleaned_data <- mc_prep_clean(data, silent = T)
    clean_info <- mc_info_clean(cleaned_data)
    expect_true(all(clean_info$count_duplicities == 0))
    expect_true(all(clean_info$count_missing == 0))
    expect_true(dplyr::near(data$localities$J$loggers[["HOBO_U23-001A_1"]]$sensors$HOBO_T$values[[1]], 7.87))
    expect_true(dplyr::near(data$localities$J$loggers[["HOBO_U23-001A_1"]]$sensors$HOBO_RH$values[[1]], 100.0))
    expect_true(all(c(data$localities$A$loggers[["HOBO_U23-001A_1"]]$metadata@type,
                   data$localities$B$loggers[["HOBO_U23-001A_1"]]$metadata@type,
                   data$localities$C$loggers[["HOBO_U23-001A_1"]]$metadata@type,
                   data$localities$D$loggers[["HOBO_U23-001A_1"]]$metadata@type,
                   data$localities$E$loggers[["HOBO_U23-001A_1"]]$metadata@type,
                   data$localities$F$loggers[["HOBO_U23-001A_1"]]$metadata@type,
                   data$localities$CH$loggers[["HOBO_U23-001A_1"]]$metadata@type,
                   data$localities$J$loggers[["HOBO_U23-001A_1"]]$metadata@type) == .model_const_LOGGER_HOBO_U23_001A))
    expect_true(is.na(data$localities$I$loggers[["Logger_1"]]$metadata@type))
    expect_true(data$localities$K$loggers[["HOBO_U23-004_1"]]$metadata@type == .model_const_LOGGER_HOBO_U23_004)
    expect_equal(length(data$localities$K$loggers[["HOBO_U23-004_1"]]$sensors), 2)
    expect_true(mc_const_SENSOR_HOBO_EXTT %in% names(data$localities$K$loggers[["HOBO_U23-004_1"]]$sensors))
    expect_true(data$localities$K$loggers[["HOBO_U23-004_1"]]$sensors$HOBO_T$metadata@height == "air 2 cm")
})

test_that("mc_read_data HOBO skip wrong datetime", {
    files_table <- as.data.frame(tibble::tribble(
        ~path, ~locality_id, ~data_format, ~serial_number, ~date_format, ~tz_offset,
        "../data/HOBO/20024354.txt", "A", "HOBO", NA_character_, "%d.%m.%Y %H:%M:%S", NA_integer_,
        "../data/HOBO/20024354_comma.csv", "B", "HOBO", NA_character_, "%m.%d.%Y %H:%M:%S", NA_integer_
    ))
    expect_warning(data <- mc_read_data(files_table, clean=FALSE)) %>%
        expect_warning()
    test_raw_data_format(data)
    expect_equal(length(data$localities), 1)
})

test_that("mc_read_files error", {
    expect_error(data <- mc_read_files(c("../data/TOMST", "data/eco-snow/data_94184102_0.csv"), "TOMST", clean=FALSE))
})

test_that("mc_read_files TOMST comma in number", {
    data <- mc_read_files(c("../data/comma_TOMST/data_91212414_0.csv",
                            "../data/comma_TOMST/data_94214606_0.csv"), "TOMST", clean=FALSE)
    expect_true(all(data$localities$`91212414`$loggers[["Thermo_1"]]$sensors$Thermo_T$values < 100))
    expect_true(is.numeric(data$localities$`91212414`$loggers[["Thermo_1"]]$sensors$Thermo_T$values))
    test_raw_data_format(data)
})

test_that("mc_read_files TOMST with error in data", {
    expect_warning(data <- mc_read_files("../data/TOMST-error", "TOMST", clean=FALSE))
    expect_true(is.na(data$localities$data_93142777$loggers[["TMS_1"]]$sensors$TMS_T2$values[[7]]))
    states <- dplyr::filter(data$localities$data_93142777$loggers[["TMS_1"]]$sensors$TMS_T2$states, .data$tag == .model_const_SENSOR_STATE_ERROR)
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
    data <- mc_read_files("../data/joined_TOMST", "TOMST_join", clean=FALSE, recursive=FALSE)
    test_raw_data_format(data)
    expect_equal(names(data$localities), c("202004_94199113", "202010_91183101", "A1E01_TS", "A1W14_TMS", "A4E53_TMS", "CKras_Loc_2_15",
                                           "CZ2_HRADEC_TMS", "CZ2_HRADEC_TS", "DP_0595"))
    expect_equal(names(data$localities$A1W14_TMS$loggers[["TMS_1"]]$sensors), c("TMS_T1", "TMS_T2", "TMS_T3", "TMS_moist"))
    expect_equal(names(data$localities$CZ2_HRADEC_TMS$loggers[["TMS_1"]]$sensors), c("TMS_T1", "TMS_T2", "TMS_T3", "TMS_moist", "VWC"))
    expect_equal(names(data$localities$CZ2_HRADEC_TS$loggers[["Thermo_1"]]$sensors), "Thermo_T")
})

test_that("mc_read_files joined TOMST NA begin", {
    data <- mc_read_files("../data/joined_TOMST/problems/202110_91201320.csv", "TOMST_join",
                          logger_type="Thermo", silent=TRUE)
    test_raw_data_format(data)
    expect_equal(data$localities$`202110_91201320`$loggers[["Thermo_1"]]$metadata@type, "Thermo")
    expect_equal(names(data$localities$`202110_91201320`$loggers[["Thermo_1"]]$sensors), "Thermo_T")
})

test_that("mc_read_data joined TOMST", {
    files_table <- tibble::tribble(
        ~path, ~locality_id, ~logger_type, ~data_format,
        "../data/joined_TOMST/CZ2_HRADEC_TMS.csv", "CZ2_HRADEC",    "TMS", "TOMST_join",
        "../data/joined_TOMST/CZ2_HRADEC_TS.csv" , "CZ2_HRADEC", "Thermo", "TOMST_join",
    )
    data <- mc_read_data(files_table, silent=TRUE)
    test_raw_data_format(data)
})

test_that("mc_read_wide", {
    data_table <- readRDS("../data/read_table/precip.Rds")
    expect_error(data <- mc_read_wide(data_table, mc_const_SENSOR_precipitation, clean=FALSE))
    dates <- data_table$date
    data_table$date <- as.POSIXct(lubridate::ymd(dates), tz="CET")
    expect_error(data <- mc_read_wide(data_table, mc_const_SENSOR_precipitation, clean=FALSE))
    data_table$date <- as.POSIXct(lubridate::ymd(dates, tz="UTC"))
    data <- mc_read_wide(data_table, mc_const_SENSOR_precipitation, clean=FALSE)
    test_raw_data_format(data)
    expect_equal(length(data$localities), 49)
    expect_equal(names(data$localities$B1BLAT01$loggers[["Logger_1"]]$sensors), mc_const_SENSOR_precipitation)
    expect_equal(data$localities$B1BLAT01$loggers[["Logger_1"]]$sensors[[1]]$values[[1]], 0.7)
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

test_that("mc_read_files user_data_formats", {
    user_data_formats <- list(HOBO=new("mc_DataFormat"))
    expect_error(my_data <- mc_read_files("../data/user_data_formats/21498648.csv", "HOBO", clean=FALSE,
                          user_data_formats=user_data_formats))
    user_data_formats <- list(myHOBO=new("mc_DataFormat"))
    user_data_formats$myHOBO@skip <- 1
    user_data_formats$myHOBO@separator <- ","
    user_data_formats$myHOBO@date_column <- 2
    user_data_formats$myHOBO@date_format <- "%m/%d/%Y %H:%M:%S"
    user_data_formats$myHOBO@separator <- ","
    user_data_formats$myHOBO@tz_offset <- 2 * 60
    user_data_formats$myHOBO@columns[[mc_const_SENSOR_T_C]] <- 3
    user_data_formats$myHOBO@columns[[mc_const_SENSOR_RH]] <- 4
    my_data <- mc_read_files("../data/user_data_formats/21498648.csv", "myHOBO", clean=FALSE,
                             user_data_formats=user_data_formats)
    test_raw_data_format(my_data)
    expect_equal(length(my_data$localities$`21498648`$loggers[["Logger_1"]]$sensors), 2)
    expect_equal(names(my_data$localities), "21498648")
    cleaned_data <- mc_prep_clean(my_data, silent = TRUE)
    expect_equal(dplyr::last(cleaned_data$localities$`21498648`$loggers[["Logger_1"]]$sensors$T_C$values), 13)
    expect_equal(dplyr::last(cleaned_data$localities$`21498648`$loggers[["Logger_1"]]$sensors$RH$values), 53)
})

test_that("mc_read_files user_data_formats auto datetime", {
    files <- c("../data/user_data_formats/TMS94184102.csv", "../data/user_data_formats/TMS94184102_CET.csv")
    user_data_formats <- list(my_logger=new("mc_DataFormat"))
    user_data_formats$my_logger@date_column <- 2
    user_data_formats$my_logger@tz_offset <- 0
    user_data_formats$my_logger@columns[[mc_const_SENSOR_T_C]] <- c(3, 4, 5)
    user_data_formats$my_logger@columns[[mc_const_SENSOR_real]] <- 6
    my_data <- mc_read_files(files, "my_logger", silent=TRUE, user_data_formats=user_data_formats)
    test_raw_data_format(my_data)
    expect_equal(length(my_data$localities$TMS94184102$loggers[["Logger_1"]]$sensors), 4)
    expect_equal(names(my_data$localities$TMS94184102$loggers[["Logger_1"]]$sensors), c("T_C1", "T_C2", "T_C3", "real"))
})

test_that("mc_read_files TOMST custom date time format", {
    expect_error(expect_warning(expect_warning(data <- mc_read_files("../data/TOMST-date/data_91171153_2023_11_14_0.csv", "TOMST"))))
    data <- mc_read_files("../data/TOMST-date/data_91171153_2023_11_14_0.csv", "TOMST", date_format=c("%d.%m.%Y %H:%M:%S", "%d.%m.%Y"),
                          silent=TRUE)
    test_raw_data_format(data)
    data <- mc_read_data("../data/TOMST-date/files_table.csv", clean=FALSE)
    test_raw_data_format(data)
    files_table <- read.table("../data/TOMST-date/files_table.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)
    data <- mc_read_data(files_table, clean=FALSE)
    test_raw_data_format(data)
})

test_that("mc_read_files HOBO logger_format", {
    files_table <- as.data.frame(tibble::tribble(
        ~path, ~locality_id, ~data_format, ~date_format, ~logger_type,
             "../data/HOBO/20024354.txt", "A", "HOBO", "%d.%m.%Y %H:%M:%S", "HOBO_U23-001A",
        "../data/HOBO/2015_10382557.txt", "A", "HOBO", "%d.%m.%Y %H:%M:%S", "HOBO_U23-004",
    ))
    data <- mc_read_data(files_table, clean=FALSE)
    test_raw_data_format(data)
    expect_equal(names(data$localities$A$loggers[["HOBO_U23-001A_1"]]$sensors), c("HOBO_T", "HOBO_RH"))
    expect_equal(names(data$localities$A$loggers[["HOBO_U23-004_1"]]$sensors), c("HOBO_T", "HOBO_extT"))
})
