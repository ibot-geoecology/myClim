example_tms_data <- microclim::prepare.read_files_by_csv("examples/data/TMS/files_table.csv")
example_tms_t1_table <- microclim::read.get_sensor_values_from_localities(example_tms_data, "T1", c("LOC_1", "LOC_2"))
