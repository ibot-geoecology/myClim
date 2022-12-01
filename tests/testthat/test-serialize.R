source("libtest.R")

test_that(".save_convert_classes_to_lists", {
    test_function <- if(exists(".save_convert_classes_to_lists")) .save_convert_classes_to_lists else .save_convert_classes_to_lists
    data <- mc_read_data("../data/TOMST/files_table.csv", silent = T)
    converted_data <- test_function(data)
    expect_true(is(converted_data$metadata, "list"))
    expect_true(is(converted_data$localities$A1E05$metadata, "list"))
    expect_true(is(converted_data$localities$A1E05$loggers[[1]]$metadata, "list"))
    expect_true(is(converted_data$localities$A1E05$loggers[[1]]$clean_info, "list"))
    expect_true(is(converted_data$localities$A1E05$loggers[[1]]$sensors$TS_T$metadata, "list"))
})

test_that("save and load", {
    data <- mc_read_data("../data/TOMST/files_table.csv", silent = T)
    mc_save(data, "../temp/data.RDS")
    loaded_data <- mc_load("../temp/data.RDS")
    test_raw_data_format(loaded_data)
    expect_equal(data, loaded_data)
    agg_data <- mc_agg(data, "mean", "hour")
    mc_save(agg_data, "../temp/data.RDS")
    loaded_agg_data <- mc_load("../temp/data.RDS")
    test_agg_data_format(loaded_agg_data)
    expect_equal(agg_data, loaded_agg_data)
})
