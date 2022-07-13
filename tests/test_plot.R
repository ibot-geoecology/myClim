library(testthat)
library(myClim)

test_that(".plot_get_logger_sensors_by_physical", {
    data <- mc_read_data("data/TOMST/files_table.csv", clean=FALSE)
    test_function <- if(exists(".plot_get_logger_sensors_by_physical")) .plot_get_logger_sensors_by_physical else myClim:::.plot_get_logger_sensors_by_physical
    physical <- test_function(data$A6W79$loggers[[1]])
    expect_equal(length(physical), 2)
    expect_equal(physical$T_C, c("TMS_T1", "TMS_T2", "TMS_T3"))
    expect_equal(physical$TMSmoisture, "TMS_TMSmoisture")
    physical <- test_function(data$A1E05$loggers[[1]])
    expect_equal(length(physical), 1)
    expect_equal(physical$T_C, "TS_T")
})

test_that("all plots", {
    data <- mc_read_data("data/TOMST/files_table.csv", silent=TRUE)
    data_agg <- mc_agg(data)

    mc_plot_loggers(data, "plots")

    mc_plot_image(data, "plots/image.png", "T1 sensors", sensors="TMS_T1")
    mc_plot_image(data_agg, "plots/image.png", "T1 sensors", sensors="TMS_T1")
    mc_plot_image(data_agg, "plots/image.png")

    mc_plot_raster(data, "plots/raster.pdf", sensors = c("TMS_T1", "TMS_T2"))
    mc_plot_raster(data_agg, "plots/raster.png", sensors = c("TMS_T1", "TMS_T2"), png_height = 500)

    mc_plot_line(data, "plots/line.pdf")
    mc_plot_line(data_agg, "plots/line_T1.png", png_height = 500, sensors="TMS_T1")
    mc_plot_line(data_agg, "plots/line.png", png_height = 500)

    expect_true(TRUE)
})
