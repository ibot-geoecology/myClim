library(testthat)
library(microclim)

test_that(".plot_get_logger_sensors_by_physical", {
    data <- microclim::mc_feed_from_csv("data/TMS/files_table.csv")
    physical <- microclim:::.plot_get_logger_sensors_by_physical(data$LOC_1$loggers[[1]])
    expect_equal(length(physical), 2)
    expect_equal(physical$T, c("T1", "T2", "T3"))
    expect_equal(physical$TMS_moisture, c("moisture"))
})
