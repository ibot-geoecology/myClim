library(microclim)

# The table in the examples/data/TOMST/files_table.csv file is used to load data.
example_tomst_data1 <- mc_feed_from_csv("examples/data/TOMST/files_table.csv")
# The function looks for csv files in the examples/data/TOMST/ directory and tries to parse the TOMST data.
# File examples/data/TOMST/files_table.csv doesn't contain TOMST data. A warning is printed and the file is skipped.
example_tomst_data2 <- mc_feed_TOMST_directory("examples/data/TOMST/")
# cleaning datetime step
example_cleaned_tomst_data1 <- mc_clean_datetime_step(example_tomst_data1)
# solar tz offset
example_cleaned_tomst_data1 <- mc_clean_solar_tz(example_cleaned_tomst_data1)
# The function returns a wideformat table with the values of the TMS_T1, and TMS_T2 sensors in the localities A6W79 and A2E32.
example_tms_wideformat_table <- mc_reshape_wideformat(example_cleaned_tomst_data1, c("A6W79", "A2E32"), c("TMS_T1", "TMS_T2"))
# The function returns a longformat table with the values of the TMS_T1, and TMS_T2 sensors in the localities A6W79 and A2E32.
example_tms_longformat_table <- mc_reshape_longformat(example_cleaned_tomst_data1, c("A6W79", "A2E32"), c("TMS_T1", "TMS_T2"))
