test_that(".plot_get_logger_sensors_by_physical", {
    data <- mc_read_data("../data/TOMST/files_table.csv", clean=FALSE)
    test_function <- if(exists(".plot_get_logger_sensors_by_physical")) .plot_get_logger_sensors_by_physical else .plot_get_logger_sensors_by_physical
    physical <- test_function(data$localities$A6W79$loggers[[1]])
    expect_equal(length(physical), 2)
    expect_equal(physical$T_C, c("TMS_T1", "TMS_T2", "TMS_T3"))
    expect_equal(physical$TMSmoisture, "TMS_TMSmoisture")
    physical <- test_function(data$localities$A1E05$loggers[[1]])
    expect_equal(length(physical), 1)
    expect_equal(physical$T_C, "TS_T")
})

test_that("all plots", {
    data <- mc_read_files("../data/eco-snow", dataformat_name = "TOMST", silent=TRUE)
    data_agg <- mc_agg(data)
    tmp_dir <- tempdir()
    plot_dir <- file.path(tmp_dir, "plot")
    mc_plot_loggers(data, plot_dir)

    image_path <- file.path(plot_dir, "image.png")
    mc_plot_image(data, image_path, "T1 sensors", sensors="TMS_T1")
    mc_plot_image(data_agg, image_path, "T1 sensors", sensors="TMS_T1")
    mc_plot_image(data_agg, image_path)

    mc_plot_raster(data, file.path(plot_dir, "raster.pdf"))
    mc_plot_raster(data_agg, file.path(plot_dir, "raster.png"), png_height = 500)

    mc_plot_line(data, file.path(plot_dir, "line.pdf"))
    mc_plot_line(data_agg, file.path(plot_dir, "line_T1.png"), png_height = 500, sensors="TMS_T1")
    mc_plot_line(data_agg, file.path(plot_dir, "line.png"), png_height = 500)

    unlink(plot_dir, recursive=TRUE)

    expect_true(TRUE)
})

test_that(".plot_get_data_sensors_by_physical", {
    data <- mc_read_data("../data/TOMST/files_table.csv", silent=T)
    test_function <- if(exists(".plot_get_data_sensors_by_physical")) .plot_get_data_sensors_by_physical else .plot_get_data_sensors_by_physical
    wrong_data <- mc_prep_meta_sensor(data, list(TMS_TMSmoisture="TS_T"), "name")
    expect_error(test_function(wrong_data))
    table <- test_function(data)
    expect_equal(nrow(table), 5)
    expect_equal(stringr::str_sort(unique(table$physical)), c("T_C", "TMSmoisture"))
})

