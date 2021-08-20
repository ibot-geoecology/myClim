example_tms_data1 <- microclim::prepare.read_files_by_csv("examples/data/TMS/files_table.csv")
example_tms_data2 <- microclim::prepare.read_TMS_directory("examples/data/TMS/")
example_tms_t1_table <- microclim::read.get_sensor_values_from_localities(example_tms_data1, "T1", c("LOC_1", "LOC_2"))
