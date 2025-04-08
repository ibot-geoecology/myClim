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

test_that("load objects version 1.1.0", {
    raw_data <- mc_load("../data/serialize/raw_data_1.0.19.rds")
    test_raw_data_format(raw_data)
    expect_equal(raw_data$localities$A2E32$loggers[[2]]$metadata@type, .model_const_LOGGER_HOBO_U23_001A)
})

test_that("load old versions of object", {
    raw_data <- mc_load("../data/serialize/raw_data_1.0.5.rds")
    test_raw_data_format(raw_data)
    raw_data <- mc_load("../data/serialize/raw_data_1.3.2.rds")
    test_raw_data_format(raw_data)
    raw_data <- mc_load("../data/serialize/raw_data_1.4.1.rds")
    test_raw_data_format(raw_data)
})

test_that("save and load separated localities", {
    data <- mc_read_data("../data/TOMST/files_table.csv", silent = T)
    temp_dir <- tempdir()
    data_dir <- file.path(temp_dir, "data")
    dir.create(data_dir)
    mc_save_localities(data, data_dir)
    files <- list.files(data_dir, full.names = FALSE)
    expect_equal(files, c("A1E05.rds", "A2E32.rds", "A6W79.rds"))
    files <- list.files(data_dir, full.names = TRUE)
    loaded_data <- mc_load(files)
    test_raw_data_format(loaded_data)
    expect_equal(names(loaded_data$localities), c("A1E05", "A2E32", "A6W79"))
    expect_equal(data, loaded_data)
    skip_item <- list("A", "B", "C")
    saveRDS(skip_item, file.path(data_dir, "skip_item.rds"))
    expect_warning(loaded_data <- mc_load(data_dir),
        regex="Data in file .+skip_item\\.rds is not myClim object and will be skipped\\.")
    test_raw_data_format(loaded_data)
    expect_equal(names(loaded_data$localities), c("A1E05", "A2E32", "A6W79"))
    expect_equal(data, loaded_data)
    new_a1e05_path <- file.path(temp_dir, "A1E05.rds")
    file.rename(file.path(data_dir, "A1E05.rds"), new_a1e05_path)
    expect_warning(loaded_data <- mc_load(c(data_dir, new_a1e05_path)),
        regex="Data in file .+skip_item\\.rds is not myClim object and will be skipped\\.")
    test_raw_data_format(loaded_data)
    expect_equal(names(loaded_data$localities), c("A2E32", "A6W79", "A1E05"))
    file.remove(new_a1e05_path)
    unlink(data_dir, recursive = TRUE)
})

