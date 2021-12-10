library(testthat)
library(microclim)

test_that("wideformat-filter", {
    data <- mc_read_csv("data/TOMST/files_table.csv")
    table <- mc_reshape_wide(data, c("A6W79", "A2E32"), c("TMS_T1", "TMS_T2"))
    expect_true("A6W79-94184102-TMS_T1" %in% colnames(table))
    expect_equal(ncol(table), 5)
    expect_equal(nrow(table), 100)
})

test_that("wideformat-all", {
    data <- mc_read_csv("data/TOMST/files_table.csv")
    table <- mc_reshape_wide(data)
    expect_equal(ncol(table), 10)
    expect_equal(nrow(table), 111)
})

test_that("longformat-filter", {
    data <- mc_read_csv("data/TOMST/files_table.csv")
    table <- mc_reshape_long(data, c("A6W79", "A2E32"), c("TMS_T1", "TMS_T2"))
    expect_equal(ncol(table), 5)
    expect_equal(nrow(table), 2*(49+75))
})
