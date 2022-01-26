library(testthat)
library(myClim)
source("test.R")

test_that(".common_sensor_values_as_tibble", {
    data <- mc_read_files("data/TOMST/data_94184102_0.csv", "TOMST")
    table <- myClim:::.common_sensor_values_as_tibble(data[[1]]$loggers[[1]])
    expect_equal(colnames(table), c("datetime", "TMS_T1", "TMS_T2", "TMS_T3", "TMS_TMSmoisture"))
})
