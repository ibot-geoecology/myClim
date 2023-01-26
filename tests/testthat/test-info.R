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
    cleaned_data <- mc_read_files("../data/clean-datetime_step", "TOMST", silent=T)
    info_table <- mc_info_clean(cleaned_data)
    expect_equal(colnames(info_table), c("locality_id", "serial_number", "start_date", "end_date", "step", "count_duplicities", "count_missing", "count_disordered", "rounded"))
})

test_that("mc_info", {
    data <- mc_read_files("../data/clean-datetime_step", "TOMST", clean=FALSE)
    info_data <- mc_info(data)
    expect_equal(colnames(info_data), c("locality_id", "serial_number", "sensor_id", "sensor_name", "start_date", "end_date", "step", "period", "min_value", "max_value", "count_values", "count_na"))
    expect_equal(nrow(info_data), 17)
    cleaned_data <- mc_prep_clean(data, silent=T)
    info_cleaned_data <- mc_info(cleaned_data)
    expect_equal(nrow(info_cleaned_data), 17)
    expect_warning(agg_data <- mc_agg(cleaned_data, list(TMS_T1=c("min", "max"), TMS_TMSmoisture="mean"), "hour"))
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

