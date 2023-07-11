source("libtest.R")

test_that("mc_env_temp", {
    cleaned_data <- mc_read_files("../data/agg-month", dataformat_name="TOMST", silent=T)
    env_temp_table <- mc_env_temp(cleaned_data, "week", min_coverage=0.9)
    expect_equal(nrow(env_temp_table), 119)
    expect_true(is.na(env_temp_table$value[[1]]))
    expect_true(is.na(env_temp_table$value[[17]]))
    expect_equal(colnames(env_temp_table), c("locality_id", "serial_number", "sensor_name", "height", "datetime", "time_to", "value"))
    expect_equal(stringr::str_sort(unique(env_temp_table$sensor_name)),
                 c("T.air_200_cm.drange", "T.air_200_cm.FDD0", "T.air_200_cm.frostdays",
                   "T.air_200_cm.GDD5", "T.air_200_cm.max95p", "T.air_200_cm.mean", "T.air_200_cm.min5p"))
    expect_equal(stringr::str_sort(unique(env_temp_table$height)), "air 200 cm")
    env_temp_table <- mc_env_temp(cleaned_data, "week", min_coverage=0.5)
    expect_false(is.na(env_temp_table$value[[1]]))
})

test_that("mc_env_temp more sensors", {
    cleaned_data <- mc_read_files("../data/eco-snow", dataformat_name="TOMST", silent=T)
    env_temp_table <- mc_env_temp(cleaned_data, "week", min_coverage=0.9)
    expect_true("T.air_15_cm.drange" %in% env_temp_table$sensor_name)
    expect_true("T.air_2_cm.min5p" %in% env_temp_table$sensor_name)
    expect_true("T.soil_8_cm.frostdays" %in% env_temp_table$sensor_name)
})

test_that("mc_env_moist", {
    cleaned_data <- mc_read_data("../data/TOMST/files_table.csv", silent = T)
    expect_error(env_table <- mc_env_moist(cleaned_data, "all"))
    expect_warning(raw_data <- mc_calc_vwc(cleaned_data))
    raw_data <- mc_filter(raw_data, localities = c("A2E32", "A6W79"))
    env_table <- mc_env_moist(raw_data, "all", min_coverage=0.7)
    expect_equal(stringr::str_sort(unique(env_table$sensor_name)),
                 c("VWC.soil_0_15_cm.5p", "VWC.soil_0_15_cm.95p", "VWC.soil_0_15_cm.mean",  "VWC.soil_0_15_cm.sd"))
    expect_equal(stringr::str_sort(unique(env_table$height)), "soil 0-15 cm")
    expect_false(any(is.na(env_table$value[env_table$locality_id == "A2E32"])))
    expect_true(all(is.na(env_table$value[env_table$locality_id == "A6W79"])))
})

test_that("mc_env_vpd", {
    data <- mc_read_files("../data/env-VPD/20024338.txt", "HOBO", date_format = "%d.%m.%Y %H:%M:%S", silent=TRUE)
    data <- mc_prep_meta_locality(data, list(`20024338`="LOC"), param_name = "locality_id")
    expect_error(env_table <- mc_env_vpd(data, "all"))
    vpd_data <- mc_calc_vpd(data, mc_const_SENSOR_HOBO_T, mc_const_SENSOR_HOBO_RH)
    env_table <- mc_env_vpd(vpd_data, "all")
    expect_true(all(is.na(env_table$value)))
    env_table <- mc_env_vpd(vpd_data, "all", min_coverage=0.6)
    expect_false(any(is.na(env_table$value)))
    expect_equal(stringr::str_sort(unique(env_table$sensor_name)), c("VPD.air_150_cm.max95p", "VPD.air_150_cm.mean"))
    expect_equal(stringr::str_sort(unique(env_table$height)), "air 150 cm")
})
