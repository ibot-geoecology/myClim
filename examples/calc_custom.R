library(myClim)

mc_info_logger(mc_data_example_clean)

 # show data table ================================================================================
diff_func <- function(data_table) {
    print(data_table)
    return(NULL)
}
data_diff <- mc_calc_custom(mc_data_example_clean, diff_func,
                            output_sensor = "DIFF")
mc_info(data_diff)

# error calculation ================================================================================
diff_func <- function(data_table) {
    data_table$TMS_T3 - data_table$TMS_T2
}

data_diff <- mc_calc_custom(mc_data_example_clean, diff_func,
                            output_sensor = "DIFF")


# custom check ================================================================================

diff_func <- function(data_table) {
    if (!("TMS_T2" %in% colnames(data_table)) || !("TMS_T3" %in% colnames(data_table)))
       return(NULL)
    data_table$TMS_T3 - data_table$TMS_T2
}

data_diff <- mc_calc_custom(mc_data_example_clean, diff_func,
                            output_sensor = "DIFF")

mc_info(data_diff)

# buildin check ================================================================================

diff_func <- function(data_table) {
    data_table$TMS_T3 - data_table$TMS_T2
}

data_diff <- mc_calc_custom(mc_data_example_clean, diff_func,
                            logger_type = "TMS", output_sensor = "DIFF")
data_diff <- mc_calc_custom(mc_data_example_clean, diff_func,
                            sensors = c("TMS_T2", "TMS_T3"), output_sensor = "DIFF")

mc_info(data_diff)

# sensor_id defined ================================================================================

data_diff <- mc_calc_custom(mc_data_example_clean, diff_func,
                            logger_type = "TMS", output_sensor_id = mc_const_SENSOR_T_C,
                            output_sensor = "DIFF")

mc_info(data_diff)
df <- mc_reshape_wide(mc_filter(data_diff, localities = "A2E32", logger_type = "TMS"))
tibble::as_tibble(df)
