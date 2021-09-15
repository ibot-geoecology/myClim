library(testthat)
library(microclim)

test_that(".common_get_filtered_data", {
    data <- microclim::mc_feed_from_csv("data/TMS/files_table.csv")
    filtered <- microclim:::.common_get_filtered_data(data, c("LOC_1", "LOC_2", "LOC_3"), "T2")
    expect_equal(length(filtered), 2)
    expect_equal(length(filtered$LOC_1$loggers[[1]]$sensors), 1)
    expect_equal(length(filtered$LOC_1$loggers[[1]]$sensors), 1)
    expect_false("T1" %in% names(filtered$LOC_1$loggers[[1]]$sensors))
    expect_true("T2" %in% names(filtered$LOC_1$loggers[[1]]$sensors))
})
