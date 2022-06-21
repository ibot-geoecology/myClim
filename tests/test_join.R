library(testthat)
source("test.R")

test_that("mc_join", {
    data <- mc_read_files("data/join", "TOMST")
    expect_error(joined_data <- mc_join(data))
    cleaned_data <- mc_prep_clean(data, silent=T)
    joined_data <- mc_join(cleaned_data)
})
