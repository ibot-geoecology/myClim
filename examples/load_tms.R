# The table in the examples/data/TMS/files_table.csv file is used to load data.
example_tms_data1 <- microclim::mc_feed_from_csv("examples/data/TMS/files_table.csv")
# The function looks for csv files in the examples/data/TMS/ directory and tries to parse the TMS data.
# File examples/data/TMS/files_table.csv doesn't contain TMS data. A warning is printed and the file is skipped.
example_tms_data2 <- microclim::mc_feed_TMS_directory("examples/data/TMS/")
# The function returns a wideformat table with the values of the T1, and T2 sensors in the localities LOC_1 and LOC_2.
example_tms_wideformat_table <- microclim::mc_reshape_wideformat(example_tms_data1, c("LOC_1", "LOC_2"), c("T1", "T2"))
# The function returns a longformat table with the values of the T1, and T2 sensors in the localities LOC_1 and LOC_2.
example_tms_longformat_table <- microclim::mc_reshape_longformat(example_tms_data1, c("LOC_1", "LOC_2"), c("T1", "T2"))
