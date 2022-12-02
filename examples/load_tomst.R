# The table in the examples/data/TOMST/files_table.csv file is used to load data.
example_tomst_data1 <- myClim::mc_read_data("examples/data/TOMST/files_table.csv", "examples/data/TOMST/localities_table.csv", clean = F)
# The function looks for csv files in the examples/data/TOMST/ directory and tries to parse the TOMST data.
# File examples/data/TOMST/files_table.csv doesn't contain TOMST data. A warning is printed and the file is skipped.
example_tomst_data2 <- myClim::mc_read_files("examples/data/TOMST/", dataformat_name = "TOMST")
# cleaning datetime step
example_cleaned_tomst_data1 <- myClim::mc_prep_clean(example_tomst_data1)
# solar tz offset
example_cleaned_tomst_data1 <- myClim::mc_prep_solar_tz(example_cleaned_tomst_data1)
# The function returns a wideformat table with the values of the TMS_T1, and TMS_T2 sensors in the localities A6W79 and A2E32.
example_tms_wideformat_table <- myClim::mc_reshape_wide(example_cleaned_tomst_data1, c("A6W79", "A2E32"), c("TMS_T1", "TMS_T2"))
# The function returns a longformat table with the values of the TMS_T1, and TMS_T2 sensors in the localities A6W79 and A2E32.
example_tms_longformat_table <- myClim::mc_reshape_long(example_cleaned_tomst_data1, c("A6W79", "A2E32"), c("TMS_T1", "TMS_T2"))
