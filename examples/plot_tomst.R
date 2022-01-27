library(myClim)

example_tomst_data <- mc_read_csv("examples/data/TOMST/files_table.csv")
example_cleaned_tomst_data <- mc_prep_clean(example_tomst_data)

mc_plot_image(example_cleaned_tomst_data, "plots/image.png", "T1 sensors", sensors="TMS_T1")
mc_plot_loggers(example_cleaned_tomst_data, "plots")

mc_plot_raster(example_cleaned_tomst_data, "plots/T1T2.pdf", sensors = c("TMS_T1", "TMS_T2"))
mc_plot_raster(example_cleaned_tomst_data, "plots/moisture.pdf", sensors = "TMS_TMSmoisture")
mc_plot_raster(example_cleaned_tomst_data, "plots/T1T2.png", sensors = c("TMS_T1", "TMS_T2"), png_height = 500)

example_hour_data <- mc_agg(example_cleaned_tomst_data, "mean", period="hour")

mc_plot_image(example_hour_data, "plots/image_hour.png", "T1 sensors", sensors="TMS_T1_mean")
