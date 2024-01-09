test_that("wideformat-filter", {
    data <- mc_read_data("../data/TOMST/files_table.csv", clean=FALSE)
    table <- mc_reshape_wide(data, c("A6W79", "A2E32"), c("TMS_T1", "TMS_T2"))
    expect_true("A6W79_1_94184102_TMS_T1" %in% colnames(table))
    expect_equal(ncol(table), 5)
    expect_equal(nrow(table), 100)
    cleaned_data <- mc_prep_clean(data, silent=T)
    agg_data <- mc_agg(cleaned_data)
    table <- mc_reshape_wide(agg_data, c("A6W79", "A2E32"), c("TMS_T1", "TMS_T2"))
    expect_true("A6W79_TMS_T1" %in% colnames(table))
    expect_equal(ncol(table), 5)
    expect_equal(nrow(table), 100)
})

test_that("wideformat-all", {
    data <- mc_read_data("../data/TOMST/files_table.csv", clean=FALSE)
    table <- mc_reshape_wide(data)
    expect_equal(ncol(table), 10)
    expect_equal(nrow(table), 111)
})

test_that("wideformat-all local time", {
    data <- mc_read_data("../data/TOMST/files_table.csv",
                         localities_table = "../data/TOMST/localities_table.csv", clean=FALSE)
    table_utc <- mc_reshape_wide(data, use_utc=TRUE)
    table <- mc_reshape_wide(data, use_utc=FALSE)
    expect_equal(table_utc$A1E05_91184101_Thermo_T[table_utc$datetime == lubridate::ymd_h("2020-10-28 9")],
                 table$A1E05_91184101_Thermo_T[table$datetime == lubridate::ymd_h("2020-10-28 10")])
    table$datetime <- table$datetime - (60 * 60)
    expect_equal(table_utc, table)
    cleaned_data <- mc_prep_clean(data, silent=T)
    agg_data <- mc_agg(cleaned_data)
    table_utc <- mc_reshape_wide(agg_data, use_utc=TRUE)
    table <- mc_reshape_wide(agg_data, use_utc=FALSE)
    table$datetime <- table$datetime - (60 * 60)
    expect_equal(table_utc, table)
    agg_data <- mc_agg(cleaned_data, fun="coverage", period="day")
    expect_warning(table <- mc_reshape_wide(agg_data, use_utc=FALSE))
})

test_that("wideformat index no serial_number", {
    data <- mc_read_files("../data/join", "TOMST", clean=TRUE, silent=TRUE)
    data$localities$`91184101`$loggers[[3]]$metadata@serial_number <- NA_character_
    table <- mc_reshape_wide(data)
    expect_true("91184101_2_91184101_Thermo_T" %in% colnames(table))
    expect_true("91184101_3_Thermo_T" %in% colnames(table))
})

test_that("reshape long", {
    data <- mc_read_data("../data/TOMST/files_table.csv", clean=FALSE)
    expect_warning(table <- mc_reshape_long(data, c("A6W79", "A2E32"), c("TMS_T1", "TMS_T2")),
                   "Logger 94184103 isn't cleaned. I can't detect the last time_to.") %>%
        expect_warning("Logger 94184102 isn't cleaned. I can't detect the last time_to.")
    expect_equal(colnames(table), c("locality_id", "serial_number", "sensor_name", "height", "datetime", "time_to", "value"))
    expect_equal(nrow(table), 2*(49+75))
    cleaned_data <- mc_prep_clean(data, silent=T)
    agg_data <- mc_agg(cleaned_data)
    table <- mc_reshape_long(agg_data, c("A6W79", "A2E32"), c("TMS_T1", "TMS_T2"))
    expect_equal(colnames(table), c("locality_id", "serial_number", "sensor_name", "height", "datetime", "time_to", "value"))
    expect_equal(nrow(table), 2*(49+75))
    expect_true(all(is.na(table$serial_number)))
    all_data <- mc_agg(cleaned_data, "mean", "all")
    table <- mc_reshape_long(all_data)
    expect_true(all(table$time_to == lubridate::ymd_hm("2020-10-28 11:30")))
})

test_that("reshape long local time", {
    data <- mc_read_data("../data/TOMST/files_table.csv",
                         localities_table = "../data/TOMST/localities_table.csv", clean=TRUE, silent=TRUE)
    table_utc <- mc_reshape_long(data, use_utc=TRUE)
    table <- mc_reshape_long(data, use_utc=FALSE)
    table$datetime <- table$datetime - (60 * 60)
    table$time_to <- table$time_to - (60 * 60)
    expect_equal(table_utc, table)
    agg_data <- mc_agg(data)
    table_utc <- mc_reshape_long(agg_data, use_utc=TRUE)
    table <- mc_reshape_long(agg_data, use_utc=FALSE)
    table$datetime <- table$datetime - (60 * 60)
    table$time_to <- table$time_to - (60 * 60)
    expect_equal(table_utc, table)
})
