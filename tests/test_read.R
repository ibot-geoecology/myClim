library(testthat)
library(microclim)

test_that("get_sensor_values_from_localities", {
    data <- prepare.read_files_by_csv("data/files_table.csv")
    table <- read.get_sensor_values_from_localities(data, "T1", c("LOC_1", "LOC_2"))
    expect_equal(nrow(table), 100)
})
