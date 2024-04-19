test_that(".plot_get_logger_sensors_by_physical", {
    data <- mc_read_data("../data/TOMST/files_table.csv", clean=FALSE)
    test_function <- if(exists(".plot_get_logger_sensors_by_physical")) .plot_get_logger_sensors_by_physical else .plot_get_logger_sensors_by_physical
    physical <- test_function(data$localities$A6W79$loggers[[1]])
    expect_equal(length(physical), 2)
    expect_equal(physical$T_C, c("TMS_T1", "TMS_T2", "TMS_T3"))
    expect_equal(physical$moisture_raw, "TMS_moist")
    physical <- test_function(data$localities$A1E05$loggers[[1]])
    expect_equal(length(physical), 1)
    expect_equal(physical$T_C, "Thermo_T")
})

test_that("all plots", {
    data <- mc_read_files("../data/eco-snow", dataformat_name = "TOMST", silent=TRUE)
    data <- mc_prep_meta_locality(data, list(`94184102`=30, `94184103`=90), param_name="tz_offset")
    logger <- data$localities$`94184102`$loggers[[1]]
    plot_data <- data
    plot_data$localities$`94184102`$loggers <- list(logger, logger)
    data_agg <- mc_agg(data)
    tmp_dir <- tempdir()
    plot_dir <- file.path(tmp_dir, "plot")
    mc_plot_loggers(plot_data, plot_dir)
    expect_true(file.exists(file.path(plot_dir, "94184102_1.png")))
    expect_true(file.exists(file.path(plot_dir, "94184102_2.png")))

    image_path <- file.path(plot_dir, "image.png")
    mc_plot_image(data, image_path, "T1 sensors", sensors="TMS_T1")
    mc_plot_image(data_agg, image_path, "T1 sensors", sensors="TMS_T1")
    mc_plot_image(data_agg, image_path)
    mc_plot_image(data_agg, image_path, use_utc=FALSE)

    mc_plot_raster(data, file.path(plot_dir, "raster.pdf"))
    mc_plot_raster(data, file.path(plot_dir, "raster.pdf"), use_utc=FALSE)
    mc_plot_raster(data_agg, file.path(plot_dir, "raster.png"), png_height = 500)

    mc_plot_line(data, facet="physical")
    mc_plot_line(data, file.path(plot_dir, "line.pdf"))
    mc_plot_line(data_agg, file.path(plot_dir, "line_T1.png"), png_height = 500, sensors="TMS_T1")
    mc_plot_line(data_agg, file.path(plot_dir, "line.png"), png_height = 500)
    mc_plot_line(data_agg, file.path(plot_dir, "line.png"), png_height = 500, use_utc=FALSE)
    mc_plot_line(data_agg, facet=NULL)
    mc_plot_line(mc_data_example_clean, file.path(plot_dir, "facet_NULL.pdf"), sensors=(c(mc_const_SENSOR_TMS_moist, mc_const_SENSOR_HOBO_RH)), facet=NULL)
    mc_plot_line(mc_data_example_clean, file.path(plot_dir, "facet_physical.pdf"), facet="physical")
    data_agg <- mc_calc_snow(data_agg, sensor=mc_const_SENSOR_TMS_T2)
    mc_plot_line(data_agg, sensors="snow")

    unlink(plot_dir, recursive=TRUE)
    unlink("Rplots.pdf")

    expect_true(TRUE)
})

test_that(".plot_get_data_sensors_by_physical", {
    data <- mc_read_data("../data/TOMST/files_table.csv", silent=T)
    test_function <- if(exists(".plot_get_data_sensors_by_physical")) .plot_get_data_sensors_by_physical else .plot_get_data_sensors_by_physical
    wrong_data <- mc_prep_meta_sensor(data, list(TMS_moist="Thermo_T"), "name")
    expect_error(test_function(wrong_data))
    table <- test_function(data)
    expect_equal(nrow(table), 5)
    expect_equal(stringr::str_sort(unique(table$physical)), c("moisture_raw", "T_C"))
})

test_that("mc_plot_raster snow", {
    data <- mc_read_files("../data/eco-snow", dataformat_name = "TOMST", silent=TRUE)
    data_agg <- mc_agg(data)
    data_agg <- mc_calc_snow(data_agg, "TMS_T2")
    plot <- mc_plot_raster(data_agg, sensors="snow")

    unlink("Rplots.pdf")
    expect_true(TRUE)
})
