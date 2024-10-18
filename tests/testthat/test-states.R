source("libtest.R")

test_that("mc_states_insert", {
    data <- mc_read_data("../data/TOMST/files_table.csv", silent=TRUE, clean=FALSE)
    states <- data.frame()
    expect_error(mc_states_insert(data, states))
    data <- mc_prep_clean(data, silent=TRUE)
    expect_error(mc_states_insert(data, states))
    states <- as.data.frame(tibble::tribble(
        ~locality_id, ~logger_index, ~sensor_name,    ~tag, ~start, ~end,        ~value,
        NA,             1,   NA        ,      NA,     NA,   NA,            NA,
    ))
    expect_error(mc_states_insert(data, states))
    states <- as.data.frame(tibble::tribble(
        ~locality_id, ~logger_index, ~sensor_name,    ~tag, ~start, ~end,        ~value,
        "A1E05"     ,             1,   NA        ,      NA,     NA,   NA,            NA,
    ))
    expect_error(mc_states_insert(data, states))
    states <- as.data.frame(tibble::tribble(
        ~locality_id, ~logger_index, ~sensor_name,    ~tag, ~start, ~end,        ~value,
        "A1E05"     ,             1,   NA        , "error",     NA,   NA,            NA,
    ))
    expect_error(mc_states_insert(data, states))
    states <- as.data.frame(tibble::tribble(
        ~locality_id, ~logger_index, ~sensor_name,    ~tag,
        ~start,                                 ~end,        ~value,
        "A1E05"    ,              1,   "Thermo_T", "error",
        lubridate::ymd_hm("2020-10-28 9:30"), lubridate::ymd_hm("2020-10-28 9:00"), NA_character_,
    ))
    expect_error(states_data <- mc_states_insert(data, states))
    states <- as.data.frame(tibble::tribble(
        ~locality_id, ~logger_index, ~sensor_name,    ~tag,
        ~start,                                 ~end,        ~value,
        "A1E05"    ,             NA,   "Thermo_T", "error",
        lubridate::ymd_hm("2020-10-28 9:00"), lubridate::ymd_hm("2020-10-28 9:30"), NA_character_,
    ))
    expect_error(mc_states_insert(data, states))
    states <- as.data.frame(tibble::tribble(
        ~locality_id, ~logger_index, ~sensor_name,    ~tag,
        ~start,                                 ~end,        ~value,
        "AA1E05"    ,              1,   "Thermo_T", "error",
        lubridate::ymd_hm("2020-10-28 9:00"), lubridate::ymd_hm("2020-10-28 9:30"), NA_character_,
    ))
    expect_warning(states_data <- mc_states_insert(data, states), "Locality AA1E05 does not exist in the data.")
    states <- as.data.frame(tibble::tribble(
        ~locality_id, ~logger_index, ~sensor_name,    ~tag,
        ~start,                                 ~end,        ~value,
        "A1E05"    ,              2,   "Thermo_T", "error",
        lubridate::ymd_hm("2020-10-28 9:00"), lubridate::ymd_hm("2020-10-28 9:30"), NA_character_,
    ))
    expect_warning(states_data <- mc_states_insert(data, states), "Locality A1E05 does not contain logger with index 2.")
    states <- as.data.frame(tibble::tribble(
        ~locality_id, ~logger_index, ~sensor_name,    ~tag,
        ~start,                                 ~end,        ~value,
        "A1E05"    ,              1,     "TMS_T1", "error",
        lubridate::ymd_hm("2020-10-28 9:00"), lubridate::ymd_hm("2020-10-28 9:30"), NA_character_,
    ))
    expect_warning(states_data <- mc_states_insert(data, states), "Logger 1 in locality A1E05 does not contain sensor TMS_T1.")
    states <- as.data.frame(tibble::tribble(
        ~locality_id, ~logger_index, ~sensor_name,    ~tag,
        ~start,                                 ~end,        ~value,
        "A1E05"    ,              1,   "Thermo_T", "error",
        lubridate::ymd_hm("2020-10-28 9:00"), lubridate::ymd_hm("2020-10-28 9:30"), NA_character_,
    ))
    states_data <- mc_states_insert(data, states)
    test_raw_data_format(states_data)
    states_table <- states_data$localities$A1E05$loggers[[1]]$sensors$Thermo_T$states
    expect_equal(nrow(states_table), 2)
    expect_equal(states_table$tag[[2]], "error")
    expect_equal(states_table$start[[2]], lubridate::ymd_hm("2020-10-28 9:00"))
    expect_equal(states_table$end[[2]], lubridate::ymd_hm("2020-10-28 9:30"))
    expect_true(is.na(states_table$value[[2]]))
    expect_equal(nrow(mc_info_states(data)) + 1, nrow(mc_info_states(states_data)))
    states <- as.data.frame(tibble::tribble(
        ~locality_id, ~logger_index,    ~tag,
        ~start,                                 ~end,
        "A2E32"     ,             1, "error",
        lubridate::ymd_hm("2020-10-16 9:00"), lubridate::ymd_hm("2020-10-16 9:30"),
    ))
    states_data <- mc_states_insert(data, states)
    expect_equal(nrow(mc_info_states(data)) + 4, nrow(mc_info_states(states_data)))
    states <- as.data.frame(tibble::tribble(
        ~locality_id, ~logger_index,      ~tag,                               ~start,                                 ~end,
        "A1E05"    ,              1,      "ok", lubridate::ymd_hm("2020-01-02 0:00"), lubridate::ymd_hm("2020-12-31 23:59"),
        "A1E05"    ,              1,   "error", lubridate::ymd_hm("2020-01-01 0:00"), lubridate::ymd_hm("2020-01-01 23:59"),
        "A1E05"    ,              1,    "test", lubridate::ymd_hm("2020-10-28 9:03"), lubridate::ymd_hm("2020-10-28 10:14"),
    ))
    states_data <- mc_states_insert(data, states)
    test_raw_data_format(states_data)
    states_table <- states_data$localities$A1E05$loggers[[1]]$sensors$Thermo_T$states
    expect_equal(nrow(states_table), 3)
    expect_equal(nrow(mc_info_states(data)) + 2, nrow(mc_info_states(states_data)))
    expect_equal(states_table$start[[2]], lubridate::ymd_hm("2020-10-28 8:45"))
    expect_equal(states_table$end[[2]], lubridate::ymd_hm("2020-10-28 11:15"))
    expect_equal(states_table$start[[3]], lubridate::ymd_hm("2020-10-28 9:00"))
    expect_equal(states_table$end[[3]], lubridate::ymd_hm("2020-10-28 10:00"))
    agg_data <- mc_agg(data)
    states$logger_index <- NULL
    states_data <- mc_states_insert(agg_data, states)
    test_agg_data_format(states_data)
    expect_equal(nrow(states_table), 3)
    states_table <- states_data$localities$A1E05$sensors$Thermo_T$states
    expect_equal(nrow(mc_info_states(agg_data)) + 2, nrow(mc_info_states(states_data)))
    expect_equal(states_table$start[[2]], lubridate::ymd_hm("2020-10-28 8:45"))
    expect_equal(states_table$end[[2]], lubridate::ymd_hm("2020-10-28 11:15"))
    expect_equal(states_table$start[[3]], lubridate::ymd_hm("2020-10-28 9:00"))
    expect_equal(states_table$end[[3]], lubridate::ymd_hm("2020-10-28 10:00"))
    states <- as.data.frame(tibble::tribble(
        ~locality_id, ~sensor_name,    ~tag, ~start, ~end, ~value,
        "A1E05"     ,     "TMS_T1", "error",
        lubridate::ymd_hm("2020-10-16 9:00"), lubridate::ymd_hm("2020-10-16 9:30"), NA,
    ))
    expect_warning(states_data <- mc_states_insert(agg_data, states), "Locality A1E05 does not contain sensor TMS_T1.")
})

test_that("mc_states_insert multiple loggers", {
    data <- mc_read_files("../data/join", "TOMST", silent=TRUE)
    states <- as.data.frame(tibble::tribble(
        ~locality_id,  ~logger_index,      ~tag,                               ~start,                                 ~end,
        "91184101"  ,              1,      "a", lubridate::ymd_hm("2020-10-28 9:15"), lubridate::ymd_hm("2020-10-28 10:00"),
        "91184101"  ,              2,      "a", lubridate::ymd_hm("2020-10-28 9:15"), lubridate::ymd_hm("2020-10-28 10:00"),
        "91184101"  ,              3,      "a", lubridate::ymd_hm("2020-10-28 9:15"), lubridate::ymd_hm("2020-10-28 10:00"),
    ))
    states_data <- mc_states_insert(data, states)
    test_raw_data_format(states_data)
    thermo_states1 <- states_data$localities$`91184101`$loggers[[1]]$sensors$Thermo_T$states
    expect_equal(thermo_states1$start[[2]], lubridate::ymd_hm("2020-10-28 9:15"))
    expect_equal(thermo_states1$end[[2]], lubridate::ymd_hm("2020-10-28 9:45"))
    thermo_states2 <- states_data$localities$`91184101`$loggers[[2]]$sensors$Thermo_T$states
    expect_equal(thermo_states2$start[[2]], lubridate::ymd_hm("2020-10-28 9:15"))
    expect_equal(thermo_states2$end[[2]], lubridate::ymd_hm("2020-10-28 9:15"))
    expect_equal(nrow(states_data$localities$`91184101`$loggers[[3]]$sensors$Thermo_T$states), 1)
})

test_that("mc_states_update", {
    data <- mc_read_data("../data/TOMST/files_table.csv", silent=TRUE)
    states <- mc_info_states(data)
    states_data <- mc_states_update(data, states)
    test_raw_data_format(states_data)
    new_states <- mc_info_states(states_data)
    expect_true(all.equal(states, new_states))
    agg_data <- mc_agg(data)
    states <- mc_info_states(agg_data)
    states_data <- mc_states_update(agg_data, states)
    test_agg_data_format(states_data)
    new_states <- mc_info_states(states_data)
    expect_true(all.equal(states, new_states))
})

test_that("mc_states_ update/delete", {
    data <- mc_read_files("../data/join", "TOMST", silent=TRUE)
    states <- mc_info_states(data)
    states_data <- mc_states_update(data, states)
    test_raw_data_format(states_data)
    new_states <- mc_info_states(states_data)
    expect_true(all.equal(states, new_states))
    states_data <- mc_states_delete(data)
    expect_true(all.equal(states, new_states))
    new_states <- mc_info_states(states_data)
    expect_equal(nrow(new_states), 0)
    new_states <- states
    new_states <- dplyr::filter(new_states, .data$sensor_name != mc_const_SENSOR_Thermo_T)
    new_states$tag <- "my_source"
    states_data <- mc_states_update(states_data, new_states)
    expect_equal(nrow(states_data$localities$`91184101`$loggers[[1]]$sensors$Thermo_T$states), 0)
    expect_true(all(states_data$localities$`94184102`$loggers[[1]]$sensors$TMS_T1$states$tag == "my_source"))
})

test_that("mc_states_delete", {
    data <- mc_read_files("../data/join", "TOMST", silent=TRUE)
    states <- mc_info_states(data)
    states$tag[c(1, 7)] <- "my_source"
    states_data <- mc_states_update(data, states)
    states_data <- mc_states_delete(states_data, localities="94184103")
    new_states <- mc_info_states(states_data)
    expect_equal(nrow(states), nrow(new_states) + 8)
    states_data <- mc_states_delete(states_data, sensors=mc_const_SENSOR_Thermo_T, tags="my_source")
    new_states <- mc_info_states(states_data)
    expect_equal(nrow(states), nrow(new_states) + 9)
    states_data <- mc_states_delete(states_data, tags="my_source")
    new_states <- mc_info_states(states_data)
    expect_equal(nrow(states), nrow(new_states) + 10)
    states_data <- mc_states_delete(states_data, sensors=mc_const_SENSOR_Thermo_T)
    new_states <- mc_info_states(states_data)
    expect_equal(nrow(states), nrow(new_states) + 12)
    states_data <- mc_states_delete(states_data, localities="92192250", sensors=mc_const_SENSOR_Dendro_T, tags="source")
    new_states <- mc_info_states(states_data)
    expect_equal(nrow(states), nrow(new_states) + 13)
    test_raw_data_format(states_data)
})

test_that("mc_states_replace", {
    data <- mc_read_data("../data/TOMST/files_table.csv", silent=TRUE, clean=TRUE)
    states <- as.data.frame(tibble::tribble(
        ~locality_id, ~logger_index, ~sensor_name,    ~tag,
        ~start,                                 ~end,        ~value,
        "A1E05"    ,              1,   "Thermo_T", "error",
        lubridate::ymd_hm("2020-10-28 9:00"), lubridate::ymd_hm("2020-10-28 9:30"), NA_character_,
        "A1E05"    ,              1,   "Thermo_T", "error",
        lubridate::ymd_hm("2020-10-28 10:15"), lubridate::ymd_hm("2020-10-28 10:15"), NA_character_,
        "A2E32"    ,              1,     "TMS_T1", "error",
        lubridate::ymd_hm("2020-10-16 8:00"), lubridate::ymd_hm("2020-10-16 9:00"), NA_character_,
    ))
    states_data <- mc_states_insert(data, states)
    replaced_data <- mc_states_replace(states_data, "error")
    test_raw_data_format(replaced_data)
    expect_true(all(is.na(replaced_data$localities$A1E05$loggers[[1]]$sensors$Thermo_T$values[c(2:4, 7)])))
    expect_equal(replaced_data$localities$A1E05$loggers[[1]]$sensors$Thermo_T$values[c(1, 5, 6, 8:11)],
                 data$localities$A1E05$loggers[[1]]$sensors$Thermo_T$values[c(1, 5, 6, 8:11)])
    data_agg <- mc_agg(states_data)
    replaced_data_agg <- mc_states_replace(data_agg, "error", -200)
    test_agg_data_format(replaced_data_agg)
    expect_true(all(replaced_data_agg$localities$A2E32$sensors$TMS_T1$values[8:12] == -200))
    expect_equal(replaced_data_agg$localities$A2E32$sensors$TMS_T1$values[c(1:7, 13:75)],
                 data_agg$localities$A2E32$sensors$TMS_T1$values[c(1:7, 13:75)])
})

test_that("mc_states_from_sensor", {
    cleaned_data <- mc_read_files("../data/eco-snow", "TOMST", silent=T)
    # raw data
    snow_raw_data <- mc_calc_snow(cleaned_data, "TMS_T2", output_sensor="T2_snow")
    expect_error(data_with_states <- mc_states_from_sensor(snow_raw_data, "TMS_T2", "snow", "TMS_T2", "test value"))
    data_with_states <- mc_states_from_sensor(snow_raw_data, "T2_snow", "snow", c("TMS_T2", "TMS_moist"), "test value")
    test_raw_data_format(data_with_states)
    states_table <- mc_info_states(data_with_states)
    states_table <- dplyr::filter(states_table, .data$tag == "snow")
    expect_equal(nrow(states_table), 8)
    expect_true(all(states_table$value == "test value"))
    # agg data
    snow_agg_data <- mc_agg(snow_raw_data, fun=list(TMS_T2="max", T2_snow="max"), period="day")
    agg_data_with_states <- mc_states_from_sensor(snow_agg_data, "T2_snow_max", "snow", "TMS_T2_max", inverse = 2)
    test_agg_data_format(agg_data_with_states)
    states_table <- mc_info_states(agg_data_with_states)
    states_table <- dplyr::filter(states_table, .data$tag == "snow")
    expect_equal(nrow(states_table), 3)
    expect_equal(states_table$start[[1]], agg_data_with_states$localities[["94184102"]]$datetime[[1]])
    snow_agg_data$localities[["94184102"]]$sensors$T2_snow_max$values[10:12] <- NA
    agg_data_with_states <- mc_states_from_sensor(snow_agg_data, "T2_snow_max", "snow", "TMS_T2_max")
    test_agg_data_format(agg_data_with_states)
    states_table <- mc_info_states(agg_data_with_states)
    states_table <- dplyr::filter(states_table, .data$tag == "snow")
    expect_equal(nrow(states_table), 3)
})

test_that("mc_states_to_sensor", {
    data <- mc_read_data("../data/TOMST/files_table.csv", silent=TRUE)
    states <- as.data.frame(tibble::tribble(
        ~locality_id, ~logger_index, ~sensor_name,    ~tag,
        ~start,                                 ~end,
        "A2E32"     ,              1,    "TMS_T1", "error",
        lubridate::ymd_hm("2020-10-16 7:00"), lubridate::ymd_hm("2020-10-16 8:00"),
        "A2E32"     ,              1,    "TMS_T1", "error",
        lubridate::ymd_hm("2020-10-16 9:00"), lubridate::ymd_hm("2020-10-16 10:00"),
        "A2E32"     ,              1,    "TMS_T2", "error",
        lubridate::ymd_hm("2020-10-16 7:30"), lubridate::ymd_hm("2020-10-16 8:30"),
    ))
    states_data <- mc_states_insert(data, states)
    data_new_sensor <- mc_states_to_sensor(states_data, "error", "TMS_T1_error", "TMS_T1")
    test_raw_data_format(data_new_sensor)
    t1_error_expected_values <- c(rep(FALSE, 3), rep(TRUE, 5), rep(FALSE, 3), rep(TRUE, 5), rep(FALSE, 75-16))
    expect_equal(data_new_sensor$localities$A2E32$loggers[[1]]$sensors$TMS_T1_error$values, t1_error_expected_values)
    expect_error(data_new_sensor <- mc_states_to_sensor(states_data, "error", c("TMS_T1_error", "TMS_T2_error"), "TMS_T1"))
    data_new_sensor <- mc_states_to_sensor(states_data, "error", c("TMS_T1_error", "TMS_T2_error"), c("TMS_T1", "TMS_T2"))
    test_raw_data_format(data_new_sensor)
    t2_error_expected_values <- c(rep(FALSE, 5), rep(TRUE, 5), rep(FALSE, 75-10))
    expect_equal(data_new_sensor$localities$A2E32$loggers[[1]]$sensors$TMS_T1_error$values, t1_error_expected_values)
    expect_equal(data_new_sensor$localities$A2E32$loggers[[1]]$sensors$TMS_T2_error$values, t2_error_expected_values)
    data_new_sensor <- mc_states_to_sensor(states_data, "error", "TMS_error", inverse = TRUE)
    test_raw_data_format(data_new_sensor)
    expect_equal(data_new_sensor$localities$A2E32$loggers[[1]]$sensors$TMS_error$values, !(t1_error_expected_values | t2_error_expected_values))
    agg_states_data <- mc_agg(states_data)
    agg_data_new_sensor <- mc_states_to_sensor(agg_states_data, "error", "TMS_T1_error", "TMS_T1")
    test_agg_data_format(agg_data_new_sensor)
    expect_equal(agg_data_new_sensor$localities$A2E32$sensors$TMS_T1_error$values, t1_error_expected_values)
})

test_that("mc_states_outlier", {
    data <- mc_read_files("../data/TMSoffsoil/data_93142760_201904.csv", "TOMST", clean=F)
    range_table <- mc_info_range(data)
    range_table$min_value <- -3.5
    expect_error(states_data <- mc_states_outlier(data, range_table))
    data <- mc_prep_clean(data, silent=TRUE)
    range_table$negative_jump[range_table$sensor_name == "TMS_moist"] <- -500
    expect_error(states_data <- mc_states_outlier(data, range_table))
    range_table$negative_jump[range_table$sensor_name == "TMS_moist"] <- 500
    states_data <- mc_states_outlier(data, range_table)
    test_raw_data_format(states_data)
    states_table <- mc_info_states(states_data)
    states_table <- dplyr::filter(states_table, .data$tag != "source")
    expect_equal(nrow(states_table), 3)
    expect_true("range" %in% states_table$tag)
    expect_true("jump" %in% states_table$tag)
    range_table$negative_jump[range_table$sensor_name == "TMS_moist"] <- 2000
    range_table$positive_jump[range_table$sensor_name == "TMS_moist"] <- 1200
    states_data <- mc_states_outlier(data, range_table, period="1 hour", range_tag="too_cold", jump_tag="my_jump")
    test_raw_data_format(states_data)
    states_table <- mc_info_states(states_data)
    states_table <- dplyr::filter(states_table, .data$tag != "source")
    expect_equal(nrow(states_table), 9)
    expect_equal(nrow(dplyr::filter(states_table, tag == "my_jump")), 7)
    expect_true("too_cold" %in% states_table$tag)
    range_table <- mc_info_range(data)
    range_table$positive_jump[range_table$sensor_name == "TMS_moist"] <- 300
    states_data <- mc_states_outlier(data, range_table, jump_tag="rainy")
    test_raw_data_format(states_data)
    states_table <- mc_info_states(states_data)
    states_table <- dplyr::filter(states_table, .data$tag != "source")
    expect_equal(nrow(states_table), 6)
    expect_true(all("rainy" == states_table$tag))
    agg_data <- mc_agg(data, fun="mean", period="1 hour")
    range_table <- mc_info_range(agg_data)
    range_table$min_value <- -3.5
    range_table$negative_jump[range_table$sensor_name == "TMS_moist_mean"] <- 500
    states_agg_data <- mc_states_outlier(agg_data, range_table)
    test_agg_data_format(states_agg_data)
    states_agg_table <- mc_info_states(states_agg_data)
    states_agg_table <- dplyr::filter(states_agg_table, .data$tag != "source")
    expect_equal(nrow(states_agg_table), 3)
    expect_true("range" %in% states_agg_table$tag)
    expect_true("jump" %in% states_agg_table$tag)
})
