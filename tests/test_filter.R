library(testthat)
library(myClim)
source("test.R")

test_that("mc_filter prep format", {
    data <- mc_read_data("data/TOMST/files_table.csv", clean=FALSE)
    filtered <- mc_filter(data, c("A6W79", "A2E32", "A1E05"), "TMS_T2")
    test_prep_data_format(filtered)
    expect_equal(length(filtered), 2)
    expect_equal(length(filtered$A6W79$loggers[[1]]$sensors), 1)
    expect_equal(length(filtered$A6W79$loggers[[1]]$sensors), 1)
    expect_false("TMS_T1" %in% names(filtered$A6W79$loggers[[1]]$sensors))
    expect_true("TMS_T2" %in% names(filtered$A6W79$loggers[[1]]$sensors))
})

test_that("mc_filter reverse prep format", {
    data <- mc_read_data("data/TOMST/files_table.csv", clean=FALSE)
    filtered <- mc_filter(data, "A6W79", "TM_T", reverse=T)
    test_prep_data_format(filtered)
    expect_equal(length(filtered), 1)
    expect_equal(length(filtered$A2E32$loggers[[1]]$sensors), 4)
})

test_that("mc_filter calc format", {
    cleaned_data <- mc_read_data("data/TOMST/files_table.csv", silent=T)
    calc_data <- mc_agg(cleaned_data)
    filtered <- mc_filter(calc_data, c("A6W79", "A2E32", "A1E05"), "TMS_T2")
    test_calc_data_format(filtered)
    expect_equal(length(filtered$localities), 2)
    expect_equal(length(filtered$localities$A2E32$sensors), 1)
    expect_equal(length(filtered$localities$A6W79$sensors), 1)
    expect_false("TMS_T1" %in% names(filtered$localities$A6W79$sensors))
    expect_true("TMS_T2" %in% names(filtered$localities$A6W79$sensors))
})

test_that("mc_filter reverse calc format", {
    cleaned_data <- mc_read_data("data/TOMST/files_table.csv", silent=T)
    calc_data <- mc_agg(cleaned_data)
    filtered <- mc_filter(calc_data, "A6W79", "TM_T", reverse=T)
    test_calc_data_format(filtered)
    expect_equal(length(filtered$localities), 1)
    expect_equal(length(filtered$localities$A2E32$sensors), 4)
})

