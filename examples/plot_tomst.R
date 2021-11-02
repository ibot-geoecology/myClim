library(microclim)

example_tomst_data <- mc_feed_from_csv("examples/data/TOMST/files_table.csv")
example_cleaned_tomst_data <- mc_clean_datetime_step(example_tomst_data)

mc_plot_image(example_cleaned_tomst_data, "plots/image.png", "T1 sensors", sensors="TMS_T1")
mc_plot_loggers(example_cleaned_tomst_data, "plots")
