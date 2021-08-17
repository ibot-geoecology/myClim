library(testthat)
library(microclim)

test_that("tms_load_info_from_data", {
    data_table <- data.frame(0, "2020.10.06 09:00", 4, 10.875, 10.5, 12.375, 1257, 202, 0)
    data_format <- model.load_info_from_data(data.source_data_formats$TMS, data_table)
    expect_equal(data_format@date_format, "%Y.%m.%d %H:%M")
    expect_equal(data_format@columns, list(T1 = 4, T2 = 5, T3 = 6, moisture = 7))
})
