test_that(".plot_get_logger_sensors_by_physical", {
    data <- mc_read_data("../data/TOMST/files_table.csv", clean=FALSE)
    physical <- .plot_get_logger_sensors_by_physical(data$localities$A6W79$loggers[["TMS_1"]])
    expect_equal(length(physical), 2)
    expect_equal(physical$T_C, c("TMS_T1", "TMS_T2", "TMS_T3"))
    expect_equal(physical$moisture_raw, "TMS_moist")
    physical <- .plot_get_logger_sensors_by_physical(data$localities$A1E05$loggers[["Thermo_1"]])
    expect_equal(length(physical), 1)
    expect_equal(physical$T_C, "Thermo_T")
})

test_that("all plots", {
    data <- mc_read_files("../data/eco-snow", dataformat_name = "TOMST", silent=TRUE)
    data <- mc_prep_meta_locality(data, list(`94184102`=30, `94184103`=90), param_name="tz_offset")
    logger <- data$localities$`94184102`$loggers[["TMS_1"]]
    plot_data <- data
    plot_data$localities$`94184102`$loggers <- list(logger, logger)
    names(plot_data$localities$`94184102`$loggers) <- c("TMS_1", "TMS_2")
    plot_data$localities$`94184102`$loggers[["TMS_2"]]$metadata@name <- "TMS_2"
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

test_that("mc_plot_line multiple loggers", {
    data <- mc_read_data("../data/TOMST/files_table2.csv", silent=T)
    mc_plot_line(data)
    expect_true(TRUE)
})

test_that("mc_plot_raster snow", {
    data <- mc_read_files("../data/eco-snow", dataformat_name = "TOMST", silent=TRUE)
    data_agg <- mc_agg(data)
    data_agg <- mc_calc_snow(data_agg, "TMS_T2")
    plot <- mc_plot_raster(data_agg, sensors="snow")

    unlink("Rplots.pdf")
    expect_true(TRUE)
})

test_that("mc_plot_line tags", {
    data <- mc_read_data("../data/TOMST/files_table2.csv", silent=T)
    offsoil_data <- mc_prep_TMSoffsoil(data)
    p <- mc_plot_line(offsoil_data, localities="A2E32", facet="physical")
    states <- as.data.frame(tibble::tribble(
        ~locality_id, ~logger_name, ~sensor_name,    ~tag,
        ~start,                                 ~end,        ~value,
        "A2E32"     ,      "TMS_1",     "TMS_T1", "error",
        lubridate::ymd_hm("2020-10-16 8:00"), lubridate::ymd_hm("2020-10-16 10:00"), NA_character_,
        "A2E32"     ,      "TMS_1",     "TMS_T1", "error",
        lubridate::ymd_hm("2020-10-16 11:00"), lubridate::ymd_hm("2020-10-16 11:30"), NA_character_,
        "A2E32"     ,      "TMS_1",     "TMS_T2", "error",
        lubridate::ymd_hm("2020-10-16 8:00"), lubridate::ymd_hm("2020-10-16 11:00"), NA_character_,
        "A2E32"     ,      "TMS_1",  "TMS_moist", "error",
        lubridate::ymd_hm("2020-10-16 14:00"), lubridate::ymd_hm("2020-10-16 15:00"), NA_character_,
        "A2E32"     ,      "TMS_1",  "TMS_moist", "error",
        lubridate::ymd_hm("2020-10-16 16:00"), lubridate::ymd_hm("2020-10-16 16:00"), NA_character_,
    ))
    states_data <- mc_states_insert(data, states)
    p <- mc_plot_line(states_data, localities="A2E32", tag="error2")
    p <- mc_plot_line(states_data, localities="A2E32", tag="error")
    p <- mc_plot_line(states_data, localities="A2E32", tag="error", facet="physical")
    p <- mc_plot_line(states_data, localities="A2E32", tag="error", facet=NULL)
    offsoil_data <- mc_prep_TMSoffsoil(states_data)
    p <- mc_plot_line(offsoil_data, localities="A2E32", tag="error", facet="physical")
    agg_data <- mc_agg(states_data, fun = "mean", period = "1 hour")
    p <- mc_plot_line(agg_data, localities="A2E32", tag="error")
    expect_true(TRUE)
})
