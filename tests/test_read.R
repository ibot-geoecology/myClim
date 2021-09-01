library(testthat)
library(microclim)

test_that("get_sensor_values_from_localities", {
    data <- microclim::prepare.read_files_by_csv("data/TMS/files_table.csv")
    table <- microclim::read.get_sensor_values_from_localities(data, "T1", c("LOC_1", "LOC_2"))
    expect_equal(nrow(table), 100)
})
