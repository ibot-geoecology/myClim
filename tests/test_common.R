library(testthat)
library(microclim)
source("test.R")

test_that(".common_logger_values_as_tibble", {
    data <- mc_feed_TOMST_files("data/TOMST/data_94184102_0.csv")
    table <- microclim:::.common_logger_values_as_tibble(data$None$loggers[[1]])
    expect_equal(colnames(table), c("datetime", "TMS_T1", "TMS_T2", "TMS_T3", "TMS_TMSmoisture"))
})
