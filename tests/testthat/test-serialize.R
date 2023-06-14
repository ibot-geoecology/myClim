source("libtest.R")

test_that(".save_convert_classes_to_lists", {
    test_function <- if(exists(".save_convert_classes_to_lists")) .save_convert_classes_to_lists else .save_convert_classes_to_lists
    data <- mc_read_data("../data/TOMST/files_table.csv", silent = T)
    converted_data <- test_function(data)
    expect_true(is(converted_data$metadata, "list"))
    expect_true(is(converted_data$localities$A1E05$metadata, "list"))
    expect_true(is(converted_data$localities$A1E05$loggers[[1]]$metadata, "list"))
    expect_true(is(converted_data$localities$A1E05$loggers[[1]]$clean_info, "list"))
    expect_true(is(converted_data$localities$A1E05$loggers[[1]]$sensors$Thermo_T$metadata, "list"))
})

test_that("save and load", {
    data <- mc_read_data("../data/TOMST/files_table.csv", silent = T)
    data_file <- file.path(tempdir(), "data.RDS")
    mc_save(data, data_file)
    loaded_data <- mc_load(data_file)
    test_raw_data_format(loaded_data)
    expect_equal(data, loaded_data)
    agg_data <- mc_agg(data, "mean", "hour")
    mc_save(agg_data, data_file)
    loaded_agg_data <- mc_load(data_file)
    test_agg_data_format(loaded_agg_data)
    expect_equal(agg_data, loaded_agg_data)
    file.remove(data_file)
})

test_that("load objects version 0.2.1", {
    raw_data <- mc_load("../data/serialize/raw_data_0.2.1.rds")
    expect_equal(raw_data$localities$A1E05$metadata@elevation, 255)
    test_raw_data_format(raw_data)
    agg_data <- mc_load("../data/serialize/agg_data_0.2.1.rds")
    expect_equal(agg_data$localities$A6W79$metadata@elevation, 347)
    test_agg_data_format(agg_data)
})

test_that("load objects version 1.0.6", {
    raw_data <- mc_load("../data/serialize/raw_data_1.0.5.rds")
    test_raw_data_format(raw_data)
})
