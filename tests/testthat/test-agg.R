source("libtest.R")

test_that("mc_agg UTC", {
    data <- mc_read_files("../data/clean-datetime_step", "TOMST", clean=FALSE)
    expect_error(hour_data <- mc_agg(data, "percentile", "hour", use_utc = TRUE, percentiles = c(10, 50, 90), min_coverage = 0))
    expect_warning(cleaned_data <- mc_prep_clean(data, silent=T))
    hour_data <- mc_agg(cleaned_data, "percentile", "hour", use_utc = TRUE, percentiles = c(10, 50, 90), min_coverage = 0)
    test_agg_data_format(hour_data)
    expect_equal(length(hour_data$localities[["94184102"]]$sensors), 12)
    expect_equal(hour_data$metadata@step, 60*60)
    expect_equal(hour_data$metadata@period, "hour")
    expect_equal(hour_data$localities$`91184133`$sensors$Thermo_T_percentile10$states$start, lubridate::ymd_h("2018-09-12 10"))
    expect_equal(hour_data$localities$`91184133`$sensors$Thermo_T_percentile10$states$end, lubridate::ymd_h("2018-09-15 13"))
    agg_data <- mc_agg(cleaned_data)
    test_agg_data_format(agg_data)
    expect_equal(length(agg_data$localities[["94184102"]]$sensors), 4)
    hour2_data <- mc_agg(agg_data, "percentile", "2 hours", use_utc = TRUE, percentiles = c(10, 50, 90), min_coverage = 0)
    test_agg_data_format(hour2_data)
    expect_equal(length(hour2_data$localities[["94184102"]]$sensors), 12)
    expect_equal(hour2_data$metadata@step, 2*60*60)
    expect_equal(hour2_data$metadata@period, "2 hours")
})

test_that("mc_agg day functions", {
    data <- mc_read_files("../data/agg", "TOMST", silent=T)
    data <- mc_prep_meta_locality(data, list(`91184101`=60), "tz_offset")
    agg_data <- mc_agg(data, c("min", "max", "mean", "percentile", "sum", "range", "count", "coverage"), "day", percentiles=50, use_utc=FALSE, min_coverage=1)
    test_agg_data_format(agg_data)
    expect_equal(agg_data$localities$`91184101`$sensors$Thermo_T_min$values, c(NA, 4.125), tolerance = 1e-3)
    expect_equal(agg_data$localities$`91184101`$sensors$Thermo_T_max$values, c(NA, 11.3125), tolerance = 1e-3)
    expect_equal(agg_data$localities$`91184101`$sensors$Thermo_T_mean$values, c(NA, 8.434896), tolerance = 1e-3)
    expect_equal(agg_data$localities$`91184101`$sensors$Thermo_T_percentile50$values, c(NA, 8.4375), tolerance = 1e-3)
    expect_equal(agg_data$localities$`91184101`$sensors$Thermo_T_sum$values, c(NA, 809.75), tolerance = 1e-3)
    expect_equal(agg_data$localities$`91184101`$sensors$Thermo_T_range$values, c(NA, 11.3125 - 4.125), tolerance = 1e-3)
    expect_equal(agg_data$localities$`91184101`$sensors$Thermo_T_count$values, c(87, 96))
    expect_equal(agg_data$localities$`91184101`$sensors$Thermo_T_coverage$values, c(87/96, 1), tolerance = 1e-3)
    agg_data <- mc_agg(data, c(Thermo_T=c("min", "max")), "day", use_utc=FALSE, min_coverage = 0)
    expect_equal(length(agg_data$localities$`91184101`$sensors), 2)
    test_agg_data_format(agg_data)
    mc_agg(agg_data, "mean", "week", use_utc=FALSE)
})

test_that("mc_agg empty data", {
    data <- get_empty_raw_data()
    expect_error(expect_warning(agg_data <- mc_agg(data)))
    data <- mc_read_data("../data/TOMST/files_table.csv", "../data/TOMST/localities_table.csv", silent=T)
    expect_error(expect_warning(agg_data <- mc_agg(data, "min", "day", use_utc = TRUE, min_coverage=0)))
    data <- mc_load("../data/agg/without_data.RDS")
    expect_warning(agg_data <- mc_agg(data))
    expect_warning(agg_data <- mc_agg(data, "min", "hour", use_utc = TRUE, min_coverage=0))
})

test_that("mc_agg 90s step", {
    table <- read.csv("../data/agg-short-step/step_90s_long.csv", stringsAsFactors = FALSE)
    table$datetime <- lubridate::ymd_hms(table$datetime)
    table$locality_id <- as.character(table$locality_id)
    data <- mc_read_long(table, sensor_ids = list("Temp" = mc_const_SENSOR_HOBO_T,
                                                  "RH" = mc_const_SENSOR_HOBO_RH,
                                                  "Wind" = mc_const_SENSOR_wind_speed),
                         clean = TRUE, silent = TRUE)
    test_raw_data_format(data)
    expect_equal(data$localities$`172`$loggers[[1]]$clean_info@step, 90)
    agg_data <- mc_agg(data, period = "2 min", fun = "mean")
    agg_data <- mc_agg(data, period = "all", fun = "mean")
    expect_equal(agg_data$localities$`172`$sensors$Temp_mean$values, mean(data$localities$`172`$loggers[[1]]$sensors$Temp$values))
})

test_that("mc_agg 10s step", {
    table <- read.csv("../data/agg-short-step/step_10s_long.csv", stringsAsFactors = FALSE)
    table$datetime <- lubridate::ymd_hms(table$datetime)
    table$locality_id <- as.character(table$locality_id)
    data <- mc_read_long(table, sensor_ids = list("Temp" = mc_const_SENSOR_HOBO_T,
                                                  "RH" = mc_const_SENSOR_HOBO_RH,
                                                  "Wind" = mc_const_SENSOR_wind_speed),
                         clean = TRUE, silent = TRUE)
    expect_equal(data$localities$`172`$loggers[[1]]$clean_info@step, 10)
    expect_equal(data$localities$`172`$loggers[[1]]$clean_info@count_duplicities, 0)
    expect_equal(data$localities$`172`$loggers[[1]]$clean_info@count_missing, 0)
    expect_equal(data$localities$`172`$loggers[[1]]$clean_info@count_disordered, 0)
    agg_data <- mc_agg(data, period = "1 min", fun = "mean")
    all_data <- mc_agg(data, period = "all", fun = "mean")
})

test_that("mc_agg solar aggregation", {
    data <- mc_read_data("../data/solar_agg/files_table.csv", "../data/solar_agg/localities_table.csv", silent=T)
    data <- mc_prep_solar_tz(data)
    expect_error(agg_data <- mc_agg(data, c("min", "max"), "hour", use_utc=FALSE, na.rm=FALSE))
    agg_data <- mc_agg(data, c("min", "max"), "day", use_utc=FALSE)
    test_agg_data_format(agg_data)
    expect_equal(length(agg_data$localities$A1E05$sensors), 2)
    test_agg_data_format(agg_data)
})

test_that("mc_agg UTC many NA", {
    cleaned_data <- mc_read_files("../data/clean-datetime_step/data_94184165_0.csv", "TOMST", silent=T)
    agg_data <- mc_agg(cleaned_data, c("min", "max", "mean", "percentile", "sum", "count", "coverage"), "hour", percentiles=50, min_coverage=0)
    expect_true(is.na(agg_data$localities[["94184165"]]$sensors$TMS_T1_min$values[[2]]))
    expect_true(is.na(agg_data$localities[["94184165"]]$sensors$TMS_T1_max$values[[2]]))
    expect_true(is.na(agg_data$localities[["94184165"]]$sensors$TMS_T1_mean$values[[2]]))
    expect_true(is.na(agg_data$localities[["94184165"]]$sensors$TMS_T1_percentile50$values[[2]]))
    expect_true(is.na(agg_data$localities[["94184165"]]$sensors$TMS_T1_sum$values[[2]]))
})

test_that("mc_agg long period", {
    data <- mc_read_files("../data/agg-month", "TOMST", silent=T)
    data <- mc_prep_meta_locality(data, list(`91184101`=60), "tz_offset")
    agg_data <- mc_agg(data, "mean", "week", use_utc=FALSE)
    test_agg_data_format(agg_data)
    expect_equal(agg_data$metadata@period, "week")
    expect_equal(agg_data$localities$`91184101`$datetime[[1]], lubridate::ymd_h("2020-10-26 00"))
    expect_false(is.na(agg_data$metadata@step))
    expect_true(any(is.na(agg_data$localities[["91184101"]]$sensors$Thermo_T_mean$values)))
    agg_data <- mc_agg(data, "mean", "month", use_utc=FALSE)
    test_agg_data_format(agg_data)
    expect_equal(agg_data$metadata@period, "month")
    expect_true(is.na(agg_data$metadata@step))
})

test_that("mc_agg all period", {
    data <- mc_read_files("../data/eco-snow", "TOMST", silent=T)
    all_data <- mc_agg(data, "mean", "all")
    test_agg_data_format(all_data)
    expect_equal(all_data$metadata@period, "all")
    expect_equal(all_data$metadata@intervals_start, lubridate::ymd_h("2021-01-01 00"))
    expect_equal(all_data$metadata@intervals_end, lubridate::ymd_hms("2021-01-31 23:59:59"))
    expect_equal(length(all_data$localities$`94184102`$datetime), 1)
    expect_equal(length(all_data$localities$`94184103`$datetime), 1)
    expect_false(is.na(all_data$localities$`94184102`$sensors$TMS_T1_mean$values[[1]]))
    expect_true(is.na(all_data$localities$`94184103`$sensors$TMS_T1_mean$values[[1]]))
    expect_equal(all_data$localities$`94184102`$sensors$TMS_T1_mean$states$start, lubridate::ymd_h("2021-01-01 00"))
    expect_equal(all_data$localities$`94184102`$sensors$TMS_T1_mean$states$end, lubridate::ymd_h("2021-01-01 00"))
    all_data <- mc_agg(data, "mean", "all", min_coverage=0)
    expect_false(is.na(all_data$localities$`94184102`$sensors$TMS_T1_mean$values[[1]]))
    expect_false(is.na(all_data$localities$`94184103`$sensors$TMS_T1_mean$values[[1]]))
})

test_that("mc_agg agregate from longer to shorter period", {
    data <- mc_read_files("../data/eco-snow", "TOMST", silent=T)
    agg_data <- mc_agg(data, "min", "day", min_coverage=0)
    expect_error(mc_agg(agg_data, "min", "hour", min_coverage=0))
})

test_that("mc_agg logical sensor", {
    data <- mc_read_files("../data/eco-snow/data_94184102_0.csv", "TOMST", silent=T)
    agg_data <- mc_agg(data)
    agg_data <- mc_calc_snow(agg_data, "TMS_T3", tmax=0.5)
    week_agg_data <- mc_agg(agg_data, list(snow=c("min", "max", "mean", "percentile", "sum", "count", "coverage")), "week", percentiles = 20)
    expect_equal(week_agg_data$localities$`94184102`$sensors$snow_min$values, c(NA, F, F, F, F))
    expect_equal(week_agg_data$localities$`94184102`$sensors$snow_max$values, c(NA, T, F, F, F))
    expect_equal(week_agg_data$localities$`94184102`$sensors$snow_mean$values, c(NA, T, F, F, F))
    expect_equal(week_agg_data$localities$`94184102`$sensors$snow_percentile20$values, c(NA, F, F, F, F))
    expect_equal(week_agg_data$localities$`94184102`$sensors$snow_sum$metadata@sensor_id, mc_const_SENSOR_integer)
    expect_equal(week_agg_data$localities$`94184102`$sensors$snow_sum$values, c(NA, 378, 0, 0, 0))
    expect_equal(week_agg_data$localities$`94184102`$sensors$snow_count$values, c(288, 672, 672, 672, 672))
    expect_equal(week_agg_data$localities$`94184102`$sensors$snow_count$metadata@sensor_id, "count")
    expect_equal(week_agg_data$localities$`94184102`$sensors$snow_coverage$values, c(288/672, 1, 1, 1, 1))
    expect_equal(week_agg_data$localities$`94184102`$sensors$snow_coverage$metadata@sensor_id, "coverage")
})

test_that("mc_agg integer sensor", {
    data <- mc_read_files("../data/TOMST/data_94184102_0.csv", "TOMST", silent=T)
    agg_data <- mc_agg(data, list(TMS_moist=c("min", "max", "mean", "percentile", "sum", "count", "coverage")), "hour", percentiles = 10)
    expect_equal(agg_data$localities$`94184102`$sensors$TMS_moist_min$values[[1]], 1551)
    expect_equal(agg_data$localities$`94184102`$sensors$TMS_moist_max$values[[1]], 1551)
    expect_equal(agg_data$localities$`94184102`$sensors$TMS_moist_mean$values[[2]], 1552)
    expect_equal(agg_data$localities$`94184102`$sensors$TMS_moist_percentile10$values[[8]], 1549)
    expect_equal(agg_data$localities$`94184102`$sensors$TMS_moist_sum$values[[1]], 6204)
    expect_equal(agg_data$localities$`94184102`$sensors$TMS_moist_count$values[[1]], 4)
    expect_equal(agg_data$localities$`94184102`$sensors$TMS_moist_count$metadata@sensor_id, "count")
    expect_equal(agg_data$localities$`94184102`$sensors$TMS_moist_coverage$values[[1]], 1)
    expect_equal(agg_data$localities$`94184102`$sensors$TMS_moist_coverage$metadata@sensor_id, "coverage")
})

test_that("mc_agg reaggregate", {
    not_applicable_format_warning(data <- mc_read_files("../data/TOMST/", "TOMST", silent=T)) %>%
        not_applicable_format_warning() %>%
        not_applicable_format_warning()
    agg_data <- mc_agg(data)
    agg_all <- mc_agg(agg_data, period = "all", fun = "mean")
    test_agg_data_format(agg_all)
})

test_that("mc_agg merging loggers", {
    files_table <- read.table("../data/TOMST/files_table.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)
    files_table$locality_id <- "A6W79"
    files_table$path <- purrr::map(files_table$path, ~ file.path("../data/TOMST", .x))
    data <- mc_read_data(files_table, silent=T)
    expect_warning(agg_data <- mc_agg(data), "sensor TMS_T1 is renamed to TMS_T1_1") %>%
        expect_warning("sensor TMS_T2 is renamed to TMS_T2_1") %>%
        expect_warning("sensor TMS_T3 is renamed to TMS_T3_1") %>%
        expect_warning("sensor TMS_moist is renamed to TMS_moist_1")
    test_agg_data_format(agg_data)
    expect_equal(length(agg_data$localities$A6W79$sensors), 9)
})

test_that(".agg_get_custom_intervals", {
    test_function_parse <- if(exists(".agg_parse_custom_dates")) .agg_parse_custom_dates else .agg_parse_custom_dates
    test_function <- if(exists(".agg_get_custom_intervals")) .agg_get_custom_intervals else .agg_get_custom_intervals
    custom_dates <- test_function_parse("04-01", NULL)
    intervals <- test_function(lubridate::interval(lubridate::ymd(20220501), lubridate::ymd(20230104)), custom_dates)
    expect_equal(intervals[[1]], lubridate::interval(lubridate::ymd(20220401), lubridate::ymd_hms("2023-03-31 23:59:59")))
    intervals <- test_function(lubridate::interval(lubridate::ymd(20210301), lubridate::ymd(20230104)), custom_dates)
    expect_equal(intervals, c(lubridate::interval(lubridate::ymd(20200401), lubridate::ymd_hms("2021-03-31 23:59:59")),
                              lubridate::interval(lubridate::ymd(20210401), lubridate::ymd_hms("2022-03-31 23:59:59")),
                              lubridate::interval(lubridate::ymd(20220401), lubridate::ymd_hms("2023-03-31 23:59:59"))))
    custom_dates <- test_function_parse("03-01", NULL)
    intervals <- test_function(lubridate::interval(lubridate::ymd(20190501), lubridate::ymd(20200404)), custom_dates)
    expect_equal(intervals, c(lubridate::interval(lubridate::ymd(20190301), lubridate::ymd_hms("2020-02-29 23:59:59")),
                              lubridate::interval(lubridate::ymd(20200301), lubridate::ymd_hms("2021-02-28 23:59:59"))))
    custom_dates <- test_function_parse("03-10 12:00", "09-06 8:00")
    intervals <- test_function(lubridate::interval(lubridate::ymd(20200101), lubridate::ymd(20230101)), custom_dates)
})

test_that("mc_agg custom", {
    table <- readRDS("../data/agg-custom/air_humidity.rds")
    data <- mc_read_wide(table, sensor_id = "RH", "humidity", silent=T)
    expect_error(agg_data <- mc_agg(data, "mean", period = "custom"))
    agg_data <- mc_agg(data, "mean", period = "custom", custom_start = "11-01")
    test_agg_data_format(agg_data)
    expect_equal(agg_data$localities$B1BYSH01$datetime, c(lubridate::ymd_h("2017-11-01 00"), lubridate::ymd_h("2018-11-01 00"), lubridate::ymd_h("2019-11-01 00"), lubridate::ymd_h("2020-11-01 00")))
    expect_equal(agg_data$metadata@period, "custom")
    expect_equal(agg_data$metadata@intervals_start, c(lubridate::ymd_h("2017-11-01 00"), lubridate::ymd_h("2018-11-01 00"),
                                                      lubridate::ymd_h("2019-11-01 00"), lubridate::ymd_h("2020-11-01 00")))
    expect_equal(agg_data$metadata@intervals_end, c(lubridate::ymd_hms("2018-10-31 23:59:59"), lubridate::ymd_hms("2019-10-31 23:59:59"),
                                                    lubridate::ymd_hms("2020-10-31 23:59:59"), lubridate::ymd_hms("2021-10-31 23:59:59")))
    expect_true(is.na(agg_data$metadata@step))
    agg_data <- mc_agg(data, "mean", period = "custom", custom_start = "05-01", custom_end = "10-01")
    test_agg_data_format(agg_data)
    expect_equal(agg_data$localities$B1BYSH01$datetime, c(lubridate::ymd_h("2018-05-01 00"), lubridate::ymd_h("2019-05-01 00"), lubridate::ymd_h("2020-05-01 00")))
    expect_equal(agg_data$metadata@period, "custom")
    expect_equal(agg_data$metadata@intervals_start, c(lubridate::ymd_h("2018-05-01 00"),
                                                      lubridate::ymd_h("2019-05-01 00"), lubridate::ymd_h("2020-05-01 00")))
    expect_equal(agg_data$metadata@intervals_end, c(lubridate::ymd_hms("2018-09-30 23:59:59"),
                                                    lubridate::ymd_hms("2019-09-30 23:59:59"), lubridate::ymd_hms("2020-09-30 23:59:59")))
    expect_false(is.na(agg_data$metadata@step))
    agg_data <- mc_agg(data, "mean", period = "custom", custom_start = "12-01", custom_end = "03-01")
    test_agg_data_format(agg_data)
    expect_equal(agg_data$localities$B1BYSH01$datetime, c(lubridate::ymd_h("2017-12-01 00"), lubridate::ymd_h("2018-12-01 00"), lubridate::ymd_h("2019-12-01 00"), lubridate::ymd_h("2020-12-01 00")))
    expect_equal(agg_data$metadata@period, "custom")
    expect_equal(agg_data$metadata@intervals_start, c(lubridate::ymd_h("2017-12-01 00"), lubridate::ymd_h("2018-12-01 00"),
                                                      lubridate::ymd_h("2019-12-01 00"), lubridate::ymd_h("2020-12-01 00")))
    expect_equal(agg_data$metadata@intervals_end, c(lubridate::ymd_hms("2018-02-28 23:59:59"), lubridate::ymd_hms("2019-02-28 23:59:59"),
                                                    lubridate::ymd_hms("2020-02-29 23:59:59"), lubridate::ymd_hms("2021-02-28 23:59:59")))
    expect_true(is.na(agg_data$metadata@step))
})

test_that("mc_agg shifted series", {
    files_table <- as.data.frame(tibble::tribble(
        ~path, ~locality_id, ~data_format, ~serial_number,
        "../data/clean-rounding/CZ2_HRADEC_TS.csv", "CZ2_HRADEC", "TOMST_join", NA_character_,
        "../data/clean-rounding/CZ2_HRADEC_TMS.csv", "CZ2_HRADEC", "TOMST_join", NA_character_,
    ))
    data <- mc_read_data(files_table, clean=FALSE, silent=TRUE)
    cleaned_data <- mc_prep_clean(data, silent=TRUE)
    expect_equal(cleaned_data$localities$CZ2_HRADEC$loggers[[1]]$clean_info@step, cleaned_data$localities$CZ2_HRADEC$loggers[[2]]$clean_info@step)
    expect_error(agg_data <- mc_agg(cleaned_data))
    agg_data <- mc_agg(cleaned_data, "mean", "2 hours")
    expect_false(all(is.na(agg_data$localities$CZ2_HRADEC$sensors$Thermo_T_mean$values)))
})

test_that("mc_agg custom functions", {
    data <- mc_read_files("../data/agg", "TOMST", silent=T)
    custom_functions <- list(frost_days=function(values){min(values) < 5}, first=function(values) {values[[1]]})
    agg_data <- mc_agg(data, c("min", "frost_days", "first"), "hour", custom_functions=custom_functions)
    test_agg_data_format(agg_data)
    expect_equal(agg_data$localities$`91184101`$sensors$Thermo_T_min$values < 5,
                 agg_data$localities$`91184101`$sensors$Thermo_T_frost_days$values)
    expect_equal(agg_data$localities$`91184101`$sensors$Thermo_T_frost_days$metadata@sensor_id, mc_const_SENSOR_logical)
    expect_equal(agg_data$localities$`91184101`$sensors$Thermo_T_first$metadata@sensor_id, mc_const_SENSOR_Thermo_T)
})

test_that("mc_agg min_coverage", {
    data <- mc_read_files("../data/agg", "TOMST", silent=T)
    data <- mc_prep_meta_locality(data, list(`91184101`="ABC"), "locality_id")
    agg_data <- mc_agg(data, c("min", "coverage"), "12 hours", min_coverage = 0.9)
    test_agg_data_format(agg_data)
    expect_true(is.na(agg_data$localities$ABC$sensors$Thermo_T_min$values[[1]]))
    expect_true(is.na(agg_data$localities$ABC$sensors$Thermo_T_min$values[[2]]))
    expect_false(is.na(agg_data$localities$ABC$sensors$Thermo_T_min$values[[3]]))
    expect_false(is.na(agg_data$localities$ABC$sensors$Thermo_T_min$values[[5]]))
    agg_data <- mc_agg(data, c("min", "coverage"), "12 hours", min_coverage = 0.5)
    expect_true(is.na(agg_data$localities$ABC$sensors$Thermo_T_min$values[[1]]))
    expect_false(is.na(agg_data$localities$ABC$sensors$Thermo_T_min$values[[2]]))
    expect_false(is.na(agg_data$localities$ABC$sensors$Thermo_T_min$values[[5]]))
    agg_data <- mc_agg(data, c("min", "coverage"), "12 hours", min_coverage = 0)
    expect_false(is.na(agg_data$localities$ABC$sensors$Thermo_T_min$values[[1]]))
})

test_that("mc_agg custom - states", {
    table <- readRDS("../data/agg-custom/air_humidity.rds")
    data <- mc_read_wide(table, sensor_id = "RH", "humidity", silent=T)
    states <- as.data.frame(tibble::tribble(
        ~locality_id, ~logger_index, ~sensor_name,    ~tag,
        ~start, ~end,
        "B1BOJK01"  ,             1,           NA,  "test",
        lubridate::ymd_hm("2019-06-01 0:00"), lubridate::ymd_hm("2020-06-01 0:00"),
        "B1BOJK01"  ,             1,           NA,  "test",
        lubridate::ymd_hm("2019-01-01 0:00"), lubridate::ymd_hm("2019-06-01 0:00"),
        "B1BOJK01"  ,             1,           NA,  "test",
        lubridate::ymd_hm("2019-01-01 0:00"), lubridate::ymd_hm("2019-02-01 0:00"),
    ))
    data <- mc_states_insert(data, states)
    agg_data <- mc_agg(data, "mean", period = "custom", custom_start = "05-01", custom_end = "10-01")
    test_agg_data_format(agg_data)
    expect_equal(nrow(agg_data$localities$B1BOJK01$sensors$humidity_mean$states), 2)
    expect_equal(agg_data$localities$B1BOJK01$sensors$humidity_mean$states$start,
                 c(lubridate::ymd_h("2019-05-01 0"), lubridate::ymd_h("2019-05-01 0")))
    expect_equal(agg_data$localities$B1BOJK01$sensors$humidity_mean$states$end,
                 c(lubridate::ymd_h("2020-05-01 0"), lubridate::ymd_h("2019-05-01 0")))
})

