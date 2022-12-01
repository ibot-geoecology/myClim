source("libtest.R")

test_that(".common_sensor_values_as_tibble", {
    data <- mc_read_files("../data/TOMST/data_94184102_0.csv", "TOMST", clean=FALSE)
    table <- .common_sensor_values_as_tibble(data$localities[[1]]$loggers[[1]])
    expect_equal(colnames(table), c("datetime", "TMS_T1", "TMS_T2", "TMS_T3", "TMS_TMSmoisture"))
})

test_that(".common_crop_states_table", {
    expect_warning(data <- mc_read_files("../data/TOMST-error", "TOMST", clean=FALSE))
    test_function <- if(exists(".common_crop_states_table")) .common_crop_states_table else .common_crop_states_table
    sensor <- data$localities$data_93142777$loggers[[1]]$sensors$TMS_T2
    out_sensor <- sensor
    out_sensor$states <- test_function(sensor$states, lubridate::interval(lubridate::ymd_hm("2022-02-24 07:15"), lubridate::ymd_hm("2022-02-24 16:00")))
    expect_equal(out_sensor$states$tag, c(myClim:::.model_const_SENSOR_STATE_SOURCE,
                                          rep(myClim:::.model_const_SENSOR_STATE_ERROR, 5)))
    expect_equal(out_sensor$states$start, c(lubridate::ymd_hm("2022-02-24 07:15"),
                                            lubridate::ymd_hm("2022-02-24 07:15"),
                                            lubridate::ymd_hm("2022-02-24 09:00"),
                                            lubridate::ymd_hm("2022-02-24 09:45"),
                                            lubridate::ymd_hm("2022-02-24 10:15"),
                                            lubridate::ymd_hm("2022-02-24 11:15")))
    expect_equal(out_sensor$states$end, c(lubridate::ymd_hm("2022-02-24 16:00"),
                                          lubridate::ymd_hm("2022-02-24 08:30"),
                                          lubridate::ymd_hm("2022-02-24 09:15"),
                                          lubridate::ymd_hm("2022-02-24 09:45"),
                                          lubridate::ymd_hm("2022-02-24 10:45"),
                                          lubridate::ymd_hm("2022-02-24 16:00")))
    out_sensor$states <- test_function(sensor$states, c(lubridate::interval(lubridate::ymd_hm("2022-02-24 07:30"), lubridate::ymd_hm("2022-02-24 9:15")),
                                          lubridate::interval(lubridate::ymd_hm("2022-02-24 12:00"), lubridate::ymd_hm("2022-02-24 16:00"))))
    expect_equal(out_sensor$states$tag, c(rep(myClim:::.model_const_SENSOR_STATE_SOURCE, 2),
                                          rep(myClim:::.model_const_SENSOR_STATE_ERROR, 3)))
    expect_equal(out_sensor$states$start, c(lubridate::ymd_hm("2022-02-24 07:30"),
                                            lubridate::ymd_hm("2022-02-24 12:00"),
                                            lubridate::ymd_hm("2022-02-24 07:30"),
                                            lubridate::ymd_hm("2022-02-24 09:00"),
                                            lubridate::ymd_hm("2022-02-24 12:00")))
    expect_equal(out_sensor$states$end, c(lubridate::ymd_hm("2022-02-24 09:15"),
                                          lubridate::ymd_hm("2022-02-24 16:00"),
                                          lubridate::ymd_hm("2022-02-24 08:30"),
                                          lubridate::ymd_hm("2022-02-24 09:15"),
                                          lubridate::ymd_hm("2022-02-24 16:00")))
})
