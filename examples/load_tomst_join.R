library(microclim)

example_tomst_join_data <- mc_read_directory("examples/data/joined_TOMST", dataformat_name = "TOMST_join")
# cleaning datetime step
example_cleaned_tomst_join_data <- mc_prep_clean(example_tomst_join_data)
# flat data
example_calc_tomst_join_data <- mc_agg(example_cleaned_tomst_join_data)

