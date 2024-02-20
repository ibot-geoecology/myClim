source("libtest.R")

test_that("mc_states_insert", {
    data <- mc_read_data("../data/TOMST/files_table.csv", silent=TRUE, clean=FALSE)
    states <- as.data.frame(tibble::tribble(
        ~locality_id, ~logger_index, ~sensor_name,    ~tag, ~start, ~end,        ~value,
        "AA1E05"    ,            NA,   NA        ,      NA,     NA,   NA,            NA,
    ))
    expect_error(mc_states_insert(data, states))
    data <- mc_prep_clean(data, silent=TRUE)
    expect_warning(states_data <- mc_states_insert(data, states), "Locality AA1E05 does not exist in the data.")
    states <- as.data.frame(tibble::tribble(
        ~locality_id, ~logger_index, ~sensor_name,    ~tag, ~start, ~end,        ~value,
        "A1E05"    ,              2,   NA        ,      NA,     NA,   NA,            NA,
    ))
    expect_warning(states_data <- mc_states_insert(data, states), "Locality A1E05 does not contain logger with index 2.")
    states <- as.data.frame(tibble::tribble(
        ~locality_id, ~logger_index, ~sensor_name,    ~tag, ~start, ~end,        ~value,
        "A1E05"    ,              1,     "TMS_T1",      NA,     NA,   NA,            NA,
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
        ~locality_id,  ~sensor_name,      ~tag,                               ~start,                                 ~end,
        "A1E05"    ,     "Thermo_T",      "ok", lubridate::ymd_hm("2020-01-02 0:00"), lubridate::ymd_hm("2020-12-31 23:59"),
        "A1E05"    ,     "Thermo_T",   "error", lubridate::ymd_hm("2020-01-01 0:00"), lubridate::ymd_hm("2020-01-01 23:59"),
        "A1E05"    ,     "Thermo_T",    "test", lubridate::ymd_hm("2020-10-28 9:03"), lubridate::ymd_hm("2020-10-28 10:14"),
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
        ~locality_id, ~sensor_name,    ~tag, ~start, ~end,        ~value,
        "A1E05"     ,     "TMS_T1",      NA,     NA,   NA,            NA,
    ))
    expect_warning(states_data <- mc_states_insert(agg_data, states), "Locality A1E05 does not contain sensor TMS_T1.")
})

test_that("mc_states_insert multiple loggers", {
    data <- mc_read_files("../data/join", "TOMST", silent=TRUE)
    states <- as.data.frame(tibble::tribble(
        ~locality_id,  ~sensor_name,      ~tag,                               ~start,                                 ~end,
        "91184101"  ,     "Thermo_T",      "a", lubridate::ymd_hm("2020-10-28 9:15"), lubridate::ymd_hm("2020-10-28 10:00"),
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
