library(testthat)
library(microclim)

test_that("tms_load_data_format_params_from_data", {
    data_table <- data.frame(0, "2020.10.06 09:00", 4, 10.875, 10.5, 12.375, 1257, 202, 0)
    data_format <- microclim:::.model_load_data_format_params_from_data(mc_data_formats$TOMST, data_table)
    expect_equal(data_format@date_format, "%Y.%m.%d %H:%M")
    expect_equal(data_format@columns, list(TMS_T1 = 4, TMS_T2 = 5, TMS_T3 = 6, TMS_TMSmoisture = 7))
})

test_that("get_serial_number_from_filename", {
    serial_number <- microclim:::.model_get_serial_number_from_filename(mc_data_formats$TOMST, "data_91184101_0.csv")
    expect_equal(serial_number, "91184101")
    serial_number <- microclim:::.model_get_serial_number_from_filename(mc_data_formats$TOMST, "data/TOMST/data_91184101_0.csv")
    expect_equal(serial_number, "91184101")
})

test_that("is_file_in_right_format", {
    is_ok <- microclim:::.model_is_file_in_right_format(mc_data_formats$TOMST, "data/TOMST/data_91184101_0.csv")
    expect_true(is_ok)
    is_ok <- microclim:::.model_is_file_in_right_format(mc_data_formats$TOMST, "data/TOMST/files_table.csv")
    expect_false(is_ok)
})
