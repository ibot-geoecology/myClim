library(testthat)
library(microclim)

test_that("read_files_by_csv", {
    data <- prepare.read_files_by_csv("data/files_table.csv")
    expect_equal(length(data), 3)
    expect_equal(length(data$LOC_1$loggers[[1]]$datetime), 49)
    expect_equal(length(data$LOC_1$loggers[[1]]$sensors_data), 4)
    expect_equal(length(data$LOC_2$loggers[[1]]$datetime), 75)
    expect_equal(length(data$LOC_2$loggers[[1]]$sensors_data), 4)
    expect_equal(length(data$LOC_3$loggers[[1]]$datetime), 11)
    expect_equal(length(data$LOC_3$loggers[[1]]$sensors_data), 1)
})
