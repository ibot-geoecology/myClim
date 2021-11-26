library(testthat)
library(microclim)
source("test.R")

test_that("mc_filter", {
    data <- mc_feed_from_csv("data/TOMST/files_table.csv")
    filtered <- mc_filter(data, c("A6W79", "A2E32", "A1E05"), "TMS_T2")
    test_standard_data_format(filtered)
    expect_equal(length(filtered), 2)
    expect_equal(length(filtered$A6W79$loggers[[1]]$sensors), 1)
    expect_equal(length(filtered$A6W79$loggers[[1]]$sensors), 1)
    expect_false("TMS_T1" %in% names(filtered$A6W79$loggers[[1]]$sensors))
    expect_true("TMS_T2" %in% names(filtered$A6W79$loggers[[1]]$sensors))
})

