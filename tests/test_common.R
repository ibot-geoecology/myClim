library(testthat)
library(microclim)
source("test.R")

test_that(".common_get_filtered_data", {
    data <- microclim::mc_feed_from_csv("data/TOMST/files_table.csv")
    filtered <- microclim:::.common_get_filtered_data(data, c("A6W79", "A2E32", "A1E05"), "TMS_T2")
    test_standard_data_format(filtered)
    expect_equal(length(filtered), 2)
    expect_equal(length(filtered$A6W79$loggers[[1]]$sensors), 1)
    expect_equal(length(filtered$A6W79$loggers[[1]]$sensors), 1)
    expect_false("TMS_T1" %in% names(filtered$A6W79$loggers[[1]]$sensors))
    expect_true("TMS_T2" %in% names(filtered$A6W79$loggers[[1]]$sensors))
})

test_that(".common_logger_values_as_tibble", {
    data <- mc_feed_TOMST_files("data/TOMST/data_94184102_0.csv")
    table <- microclim:::.common_logger_values_as_tibble(data$None$loggers[[1]])
    expect_equal(colnames(table), c("datetime", "TMS_T1", "TMS_T2", "TMS_T3", "TMS_TMSmoisture"))
})
