library(testthat)
library(microclim)

test_that(".plot_get_logger_sensors_by_physical", {
    data <- mc_read_csv("data/TOMST/files_table.csv")
    test_function <- if(exists(".plot_get_logger_sensors_by_physical")) .plot_get_logger_sensors_by_physical else microclim:::.plot_get_logger_sensors_by_physical
    physical <- test_function(data$A6W79$loggers[[1]])
    expect_equal(length(physical), 2)
    expect_equal(physical$T, c("TMS_T1", "TMS_T2", "TMS_T3"))
    expect_equal(physical$TMSmoisture, c("TMS_TMSmoisture"))
    physical <- test_function(data$A1E05$loggers[[1]])
    expect_equal(length(physical), 1)
    expect_equal(physical$T, "TM_T")
})
