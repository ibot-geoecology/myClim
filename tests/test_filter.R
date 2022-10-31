library(testthat)
library(myClim)
source("test.R")

test_that("mc_filter prep format", {
    data <- mc_read_data("data/TOMST/files_table.csv", clean=FALSE)
    filtered <- mc_filter(data, c("A6W79", "A2E32", "A1E05"), "TMS_T2")
    test_raw_data_format(filtered)
    expect_equal(length(filtered), 2)
    expect_equal(length(filtered$localities$A6W79$loggers[[1]]$sensors), 1)
    expect_equal(length(filtered$localities$A6W79$loggers[[1]]$sensors), 1)
    expect_false("TMS_T1" %in% names(filtered$localities$A6W79$loggers[[1]]$sensors))
    expect_true("TMS_T2" %in% names(filtered$localities$A6W79$loggers[[1]]$sensors))
})

test_that("mc_filter reverse prep format", {
    data <- mc_read_data("data/TOMST/files_table.csv", clean=FALSE)
    filtered <- mc_filter(data, "A6W79", "TS_T", reverse=T)
    test_raw_data_format(filtered)
    expect_equal(length(filtered$localities), 1)
    expect_equal(length(filtered$localities$A2E32$loggers[[1]]$sensors), 4)
})

test_that("mc_filter calc format", {
    cleaned_data <- mc_read_data("data/TOMST/files_table.csv", silent=T)
    agg_data <- mc_agg(cleaned_data)
    filtered <- mc_filter(agg_data, c("A6W79", "A2E32", "A1E05"), "TMS_T2")
    test_agg_data_format(filtered)
    expect_equal(length(filtered$localities), 2)
    expect_equal(length(filtered$localities$A2E32$sensors), 1)
    expect_equal(length(filtered$localities$A6W79$sensors), 1)
    expect_false("TMS_T1" %in% names(filtered$localities$A6W79$sensors))
    expect_true("TMS_T2" %in% names(filtered$localities$A6W79$sensors))
})

test_that("mc_filter reverse calc format", {
    cleaned_data <- mc_read_data("data/TOMST/files_table.csv", silent=T)
    agg_data <- mc_agg(cleaned_data)
    filtered <- mc_filter(agg_data, "A6W79", "TS_T", reverse=T)
    test_agg_data_format(filtered)
    expect_equal(length(filtered$localities), 1)
    expect_equal(length(filtered$localities$A2E32$sensors), 4)
})

