library(testthat)
library(microclim)

test_that("wideformat-filter", {
    data <- mc_feed_from_csv("data/TOMST/files_table.csv")
    table <- mc_reshape_wideformat(data, c("LOC_1", "LOC_2"), c("TMS_T1", "TMS_T2"))
    expect_true("LOC_1-94184102-TMS_T1" %in% colnames(table))
    expect_equal(ncol(table), 5)
    expect_equal(nrow(table), 100)
})

test_that("wideformat-all", {
    data <- mc_feed_from_csv("data/TOMST/files_table.csv")
    table <- mc_reshape_wideformat(data)
    expect_equal(ncol(table), 10)
    expect_equal(nrow(table), 111)
})

test_that("longformat-filter", {
    data <- mc_feed_from_csv("data/TOMST/files_table.csv")
    table <- mc_reshape_longformat(data, c("LOC_1", "LOC_2"), c("TMS_T1", "TMS_T2"))
    expect_equal(ncol(table), 5)
    expect_equal(nrow(table), 2*(49+75))
})
