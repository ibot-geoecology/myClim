# The table in the examples/data/TMS/files_table.csv file is used to load data.
example_tms_data1 <- microclim::mc_feed_from_csv("examples/data/TMS/files_table.csv")
# The function looks for csv files in the examples/data/TMS/ directory and tries to parse the TMS data.
# File examples/data/TMS/files_table.csv doesn't contain TMS data. A warning is printed and the file is skipped.
example_tms_data2 <- microclim::mc_feed_TMS_directory("examples/data/TMS/")
# The function returns a table with the values of the T1 sensor in the localities LOC_1 and LOC_2.
example_tms_t1_table <- microclim::mc_reshape_wideformat(example_tms_data1, "T1", c("LOC_1", "LOC_2"))
