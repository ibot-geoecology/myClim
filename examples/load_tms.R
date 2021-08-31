# The table in file examples/data/TMS/files_table.csv is used for loading data.
example_tms_data1 <- microclim::prepare.read_files_by_csv("examples/data/TMS/files_table.csv")
# The function search csv files in examples/data/TMS/ directory and try parse as TMS data.
# File examples/data/TMS/files_table.csv doesn't contain TMS data. Warning is printed and the file is skipped.
example_tms_data2 <- microclim::prepare.read_TMS_directory("examples/data/TMS/")
# The function return table with values of T1 sensor in LOC_1 and LOC_2 localities.
example_tms_t1_table <- microclim::read.get_sensor_values_from_localities(example_tms_data1, "T1", c("LOC_1", "LOC_2"))
