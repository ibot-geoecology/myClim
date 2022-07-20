library(testthat)
library(myClim)

source("test.R")

test_that("mc_env_temp", {
    cleaned_data <- mc_read_data("data/TOMST/files_table.csv", "data/TOMST/localities_table.csv", silent=T)
    mc_env_temp(cleaned_data)
})
