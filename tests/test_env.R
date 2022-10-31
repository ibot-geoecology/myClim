library(testthat)

source("test.R")

test_that("mc_env_temp", {
    cleaned_data <- mc_read_files("data/agg-month", dataformat_name="TOMST", silent=T)
    cleaned_data <- mc_prep_crop(cleaned_data, lubridate::ymd_h("2020-11-02 00"), lubridate::ymd_h("2021-02-15 00"), end_included = FALSE)
    env_temp_table <- mc_env_temp(cleaned_data, "week")
    expect_equal(colnames(env_temp_table), c("locality_id", "serial_number", "sensor_name", "height", "datetime", "time_to", "value"))
    expect_equal(sort(unique(env_temp_table$sensor_name)),
                 c("T.drange.air.200.cm", "T.FDD0.air.200.cm", "T.frostdays.air.200.cm",
                   "T.GDD5.air.200.cm", "T.max95p.air.200.cm", "T.mean.air.200.cm", "T.min5p.air.200.cm"))
    expect_equal(sort(unique(env_temp_table$height)), "air 200 cm")
})

test_that("mc_env_moist", {
    cleaned_data <- mc_read_data("data/TOMST/files_table.csv", silent = T)
    expect_error(env_table <- mc_env_moist(cleaned_data, "all"))
    expect_warning(raw_data <- mc_calc_vwc(cleaned_data))
    env_table <- mc_env_moist(raw_data, "all")
    expect_equal(sort(unique(env_table$sensor_name)),
                 c("VWC.5p.soil.0_14.cm", "VWC.95p.soil.0_14.cm", "VWC.mean.soil.0_14.cm",  "VWC.sd.soil.0_14.cm"))
    expect_equal(sort(unique(env_table$height)), "soil 0-14 cm")
})

test_that("mc_env_vpd", {
    data <- mc_read_files("data/env-VPD/20024338.txt", "HOBO", date_format = "%d.%m.%Y %H:%M:%S", silent=TRUE)
    data <- mc_prep_meta_locality(data, list(`20024338`="LOC"), param_name = "locality_id")
    expect_error(env_table <- mc_env_vpd(data, "all"))
    vpd_data <- mc_calc_vpd(data, myClim:::.model_const_SENSOR_HOBO_T_C, myClim:::.model_const_SENSOR_HOBO_RH)
    env_table <- mc_env_vpd(vpd_data, "all")
    expect_equal(sort(unique(env_table$sensor_name)), c("VPD.max95p.air.150.cm", "VPD.mean.air.150.cm"))
    expect_equal(sort(unique(env_table$height)), "air 150 cm")
})
