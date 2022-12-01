test_that("get_serial_number_from_filename", {
    serial_number <- .model_get_serial_number_from_file(mc_data_formats$TOMST, "data_91184101_0.csv")
    expect_equal(serial_number, "91184101")
    serial_number <- .model_get_serial_number_from_file(mc_data_formats$TOMST, "../data/TOMST/data_91184101_0.csv")
    expect_equal(serial_number, "91184101")
})

test_that("is_file_in_right_format", {
    is_ok <- .model_is_file_in_right_format(mc_data_formats$TOMST, "../data/TOMST/data_91184101_0.csv")
    expect_true(is_ok)
    is_ok <- .model_is_file_in_right_format(mc_data_formats$TOMST, "../data/TOMST/files_table.csv")
    expect_false(is_ok)
})

test_that(".model_hobo_get_separator", {
    test_function <- if(exists(".model_hobo_get_separator")) .model_hobo_get_separator else .model_hobo_get_separator
    lines <- .model_read_n_lines("../data/HOBO/20024354.txt", 5)
    expect_equal(test_function(lines), ";")
    lines <- .model_read_n_lines("../data/HOBO/20024354_comma.csv", 5)
    expect_equal(test_function(lines), ",")
    lines <- .model_read_n_lines("../data/HOBO/20024354_semicolon.txt", 5)
    expect_equal(test_function(lines), ";")
    lines <- .model_read_n_lines("../data/HOBO/20024354_tab.txt", 5)
    expect_equal(test_function(lines), "\t")
    lines <- .model_read_n_lines("../data/HOBO/20024354_minimal_title.csv", 5)
    expect_equal(test_function(lines), ",")
})

test_that(".model_load_data_format_params_from_data HOBO", {
    hobo_format <- mc_data_formats$HOBO
    path <- "../data/HOBO/20024354_comma.csv"
    test_function <- if(exists(".model_load_data_format_params_from_file")) .model_load_data_format_params_from_file else .model_load_data_format_params_from_file
    hobo_format@date_format <- "%y.%m.%d %H:%M:%S"
    hobo_format <- test_function(hobo_format, path)
    expect_equal(hobo_format@skip, 2)
    expect_equal(hobo_format@date_column, 2)
    expect_equal(hobo_format@tz_offset, 120)
    expect_equal(hobo_format@columns, list(HOBO_T_C=3, HOBO_RH=4))
})

test_that(".model_edit_data HOBO", {
    hobo_format <- mc_data_formats$HOBO
    hobo_format@columns <- list(HOBO_T_C=3, HOBO_RH=4)
    hobo_format@skip <- 2
    hobo_format@separator <- ","
    path <- "../data/HOBO/20024354_comma.csv"
    data_table <- .read_get_data_from_file(path, hobo_format)
    test_function <- if(exists(".model_edit_data")) .model_edit_data else .model_edit_data
    new_data_table <- test_function(hobo_format, data_table)
    expect_equal(nrow(data_table) - nrow(new_data_table), 3)
})

test_that(".model_is_physical", {
    cleaned_data <- mc_read_files("../data/calc-gdd", "TOMST", silent = T)
    raw_data <- mc_calc_gdd(cleaned_data, "TS_T")
    test_function <- if(exists(".model_is_physical")) .model_is_physical else .model_is_physical
    expect_false(test_function(raw_data$localities$`91184101`$loggers[[1]]$sensors$GDD5$metadata, "moisture"))
})

