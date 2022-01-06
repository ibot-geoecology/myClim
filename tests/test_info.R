library(testthat)
library(microclim)

test_that("mc_info_count", {
    data <- mc_read_csv("data/TOMST/files_table.csv")
    count_table <- mc_info_count(data)
    expect_equal(count_table$count, c(3, 3, 9))
    cleaned_data <- mc_prep_clean(data, silent=T)
    calc_data <- mc_agg(cleaned_data)
    count_table <- mc_info_count(calc_data)
    expect_equal(count_table$count, c(3, 9))
})

test_that("mc_info_clean", {
    data <- mc_read_directory("data/clean-datetime_step", "TOMST")
    cleaned_data <- mc_prep_clean(data, silent=T)
    info_table <- mc_info_clean(cleaned_data)
    expect_equal(colnames(info_table), c("locality_id", "serial_number", "start_date", "end_date", "step", "count_duplicits", "count_missed", "count_disordered"))
})

