library(testthat)
library(microclim)

test_that("get_sensor_values_from_localities", {
    data <- microclim::mc_feed_from_csv("data/TMS/files_table.csv")
    table <- microclim::mc_reshape_wideformat(data, "T1", c("LOC_1", "LOC_2"))
    expect_equal(nrow(table), 100)
})
