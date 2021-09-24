library(testthat)
library(microclim)

test_that("wideformat-filter", {
    data <- microclim::mc_feed_from_csv("data/TMS/files_table.csv")
    table <- microclim::mc_reshape_wideformat(data, c("LOC_1", "LOC_2"), c("T1", "T2"))
    expect_true("LOC_1-94184102-T1" %in% colnames(table))
    expect_equal(ncol(table), 5)
    expect_equal(nrow(table), 100)
})

test_that("wideformat-all", {
    data <- microclim::mc_feed_from_csv("data/TMS/files_table.csv")
    table <- microclim::mc_reshape_wideformat(data)
    expect_equal(ncol(table), 10)
    expect_equal(nrow(table), 111)
})

test_that("wideformat-interval", {
    data <- microclim::mc_feed_TMS_directory("data/simpleTMS")
    table <- microclim::mc_reshape_wideformat_interval(data, interval_length = 30)
    expect_equal(ncol(table), 3)
    expect_equal(nrow(table), 4)
    expect_equal(as.vector(table[["None-91184101-T1"]]), c(9.875, (6.8125+6.5)/2, 6.625, NA), tolerance = 0.001)
    expect_equal(as.vector(table[["None-91184102-T1"]]), c(NA, 6.5, (6.625+6.75)/2, 6.875), tolerance = 0.001)
})

test_that("logger-wideformat-interval", {
    files_table <- data.frame(path="data/simpleTMS/data_91184101_0.csv",
                              locality_id=microclim::mc_const_NONE_LOCALITY_ID,
                              logger="TMS", serial_number=NA_character_)
    data <- microclim::mc_feed_from_df(files_table)
    table <- microclim:::.reshape_wideformat_interval_logger(data[[1]]$loggers[[1]], interval_length = 30)
    expect_equal(ncol(table), 2)
    expect_equal(nrow(table), 3)
    expect_equal(as.vector(table[["T1"]]), c(9.875, (6.8125+6.5)/2, 6.625), tolerance = 0.001)
})

test_that("longformat-filter", {
    data <- microclim::mc_feed_from_csv("data/TMS/files_table.csv")
    table <- microclim::mc_reshape_longformat(data, c("LOC_1", "LOC_2"), c("T1", "T2"))
    expect_equal(ncol(table), 5)
    expect_equal(nrow(table), 2*(49+75))
})

test_that("datetimes_range", {
    datetimes <- c(as.POSIXct("2021-01-01 00:15:00",tz="UTC"), as.POSIXct("2021-01-01 00:48:22",tz="UTC"), as.POSIXct("2021-01-01 00:57:09",tz="UTC"))
    range <- microclim:::.reshape_get_datetimes_range(datetimes, 15)
    expect_equal(range[1], as.numeric(as.POSIXct("2021-01-01 00:15:00",tz="UTC")))
    expect_equal(range[2], as.numeric(as.POSIXct("2021-01-01 01:00:00",tz="UTC")))
    datetimes <- c(as.POSIXct("2021-01-01 00:15:50",tz="UTC"), as.POSIXct("2021-01-01 00:48:22",tz="UTC"), as.POSIXct("2021-01-01 01:00:00",tz="UTC"))
    range <- microclim:::.reshape_get_datetimes_range(datetimes, 15)
    expect_equal(range[1], as.numeric(as.POSIXct("2021-01-01 00:15:00",tz="UTC")))
    expect_equal(range[2], as.numeric(as.POSIXct("2021-01-01 01:15:00",tz="UTC")))
})
