library(testthat)
library(myClim)

test_that(".plot_get_logger_sensors_by_physical", {
    data <- mc_read_csv("data/TOMST/files_table.csv")
    test_function <- if(exists(".plot_get_logger_sensors_by_physical")) .plot_get_logger_sensors_by_physical else myClim:::.plot_get_logger_sensors_by_physical
    physical <- test_function(data$A6W79$loggers[[1]])
    expect_equal(length(physical), 2)
    expect_equal(physical$T, c("TMS_T1", "TMS_T2", "TMS_T3"))
    expect_equal(physical$TMSmoisture, c("TMS_TMSmoisture"))
    physical <- test_function(data$A1E05$loggers[[1]])
    expect_equal(length(physical), 1)
    expect_equal(physical$T, "TM_T")
})

test_that("all plots", {
    data <- mc_read_csv("data/TOMST/files_table.csv")
    data <- mc_prep_clean(data, silent = T)
    data_agg <- mc_agg(data)

    mc_plot_loggers(data, "plots")

    mc_plot_image(data, "plots/image.png", "T1 sensors", sensors="TMS_T1")
    mc_plot_image(data_agg, "plots/image.png", "T1 sensors", sensors="TMS_T1")

    mc_plot_raster(data, "plots/raster.pdf", sensors = c("TMS_T1", "TMS_T2"))
    mc_plot_raster(data_agg, "plots/raster.png", sensors = c("TMS_T1", "TMS_T2"), png_height = 500)

    expect_true(TRUE)
})
