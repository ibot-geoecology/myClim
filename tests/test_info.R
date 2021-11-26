library(testthat)
library(microclim)

test_that("mc_info_count", {
    data <- mc_feed_from_csv("data/TOMST/files_table.csv")
    count_table <- mc_info_count(data)
    expect_equal(count_table$count, c(3, 3, 9))
})

