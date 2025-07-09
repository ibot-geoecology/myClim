test_that("mc_info_count", {
    data <- mc_read_data("../data/TOMST/files_table.csv", clean=FALSE)
    count_table <- mc_info_count(data)
    expect_equal(count_table$count, c(3, 3, 9))
    cleaned_data <- mc_prep_clean(data, silent=T)
    agg_data <- mc_agg(cleaned_data)
    count_table <- mc_info_count(agg_data)
    expect_equal(count_table$count, c(3, 9))
})

test_that("mc_info_clean", {
    expect_warning(cleaned_data <- mc_read_files("../data/clean-datetime_step", "TOMST", silent=T))
    info_table <- mc_info_clean(cleaned_data)
    expect_equal(colnames(info_table), c("locality_id", "serial_number", "logger_name", "start_date", "end_date", "step_seconds", "count_duplicities", "count_missing", "count_disordered", "rounded"))
    expect_equal(nrow(info_table), 5)
})

test_that("mc_info", {
    data <- mc_read_files("../data/clean-datetime_step", "TOMST", clean=FALSE)
    info_data <- mc_info(data)
    expect_equal(colnames(info_data), c("locality_id", "serial_number", "sensor_id", "sensor_name", "start_date", "end_date",
                                        "step_seconds", "period", "min_value", "max_value", "count_values", "count_na", "height", "calibrated"))
    expect_equal(nrow(info_data), 17)
    expect_warning(cleaned_data <- mc_prep_clean(data, silent=T))
    info_cleaned_data <- mc_info(cleaned_data)
    expect_equal(nrow(info_cleaned_data), 17)
    expect_warning(agg_data <- mc_agg(cleaned_data, list(TMS_T1=c("min", "max"), TMS_moist="mean"), "hour"))
    info_agg_data <- mc_info(agg_data)
    expect_equal(nrow(info_agg_data), 12)
})

test_that("mc_info no data FIX", {
    data <- mc_read_files("../data/eco-snow", "TOMST", silent=T)
    all_data <- mc_agg(data, "mean", "all")
    table <- mc_info(all_data)
    expect_equal(nrow(table), 8)
})

test_that("mc_info_meta", {
    data <- mc_read_data("../data/TOMST/files_table.csv", "../data/TOMST/localities_table.csv", clean=FALSE)
    meta_info <- mc_info_meta(data)
    expect_equal(colnames(meta_info), c("locality_id", "lon_wgs84", "lat_wgs84", "elevation", "tz_offset"))
    expect_equal(nrow(meta_info), 3)
    cleaned_data <- mc_prep_clean(data, silent=T)
    agg_data <- mc_agg(cleaned_data)
    agg_meta_info <- mc_info_meta(agg_data)
    expect_equal(agg_meta_info, meta_info)
})

test_that("mc_info_logger", {
    data <- mc_read_files("../data/join", "TOMST", clean=FALSE)
    info_data <- mc_info_logger(data)
    expect_equal(colnames(info_data), c("locality_id", "logger_name", "serial_number", "logger_type", "start_date", "end_date", "step_seconds"))
    expect_equal(nrow(info_data), 8)
    cleaned_data <- mc_prep_clean(data, silent=T)
    info_cleaned_data <- mc_info_logger(cleaned_data)
    expect_equal(nrow(info_cleaned_data), 8)
    expect_true(all(!is.na(info_cleaned_data$step_seconds)))
    agg_data <- mc_agg(mc_join(cleaned_data))
    expect_error(mc_info_logger(agg_data))
})

test_that("mc_info_states", {
    data <- mc_read_data("../data/TOMST/files_table.csv", silent=TRUE)
    states <- mc_info_states(data)
    expect_equal(colnames(states), c("locality_id", "logger_name", "sensor_name",
                                     "tag", "start", "end", "value"))
    expect_equal(nrow(states), 9)
    agg_data <- mc_agg(data, "max", period="hour")
    states <- mc_info_states(agg_data)
    expect_equal(colnames(states), c("locality_id", "logger_name", "sensor_name",
                                     "tag", "start", "end", "value"))
    expect_equal(nrow(states), 9)
    expect_true(all(is.na(states$logger_name)))
})

test_that("mc_info_range", {
    data <- mc_read_data("../data/TOMST/files_table.csv", silent=TRUE)
    range_table <- mc_info_range(data)
    expect_equal(colnames(range_table), c("sensor_name", "min_value", "max_value", "positive_jump", "negative_jump"))
    expect_equal(range_table$min_value[range_table$sensor_name == "TMS_T1"], -40)
    expect_equal(range_table$max_value[range_table$sensor_name == "TMS_moist"], 4000)
    expect_true(all(is.na(range_table$max_positive_jump)))
    expect_true(all(is.na(range_table$max_negative_jump)))
    expect_warning(agg_data <- mc_agg(data, list(TMS_T2=c("mean", "count"), TMS_moist="max"), "30 min"))
    range_table <- mc_info_range(agg_data)
    expect_equal(range_table$min_value[range_table$sensor_name == "TMS_T2_mean"], -40)
    expect_equal(range_table$max_value[range_table$sensor_name == "TMS_moist_max"], 4000)
})
