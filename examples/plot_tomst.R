library(myClim)

example_tomst_data <- mc_read_data("examples/data/TOMST/files_table.csv")

mc_plot_image(example_tomst_data, "plots/image.png", "T1 sensors", sensors="TMS_T1")
mc_plot_loggers(example_tomst_data, "plots")

mc_plot_raster(example_tomst_data, "plots/T1T2.pdf", sensors = c("TMS_T1", "TMS_T2"))
mc_plot_raster(example_tomst_data, "plots/moisture.pdf", sensors = "TMS_TMSmoisture",
               viridis_color_map = "B", start_crop = lubridate::ymd_h("2021-01-15 00"), end_crop = lubridate::ymd_h("2021-03-17 00"))
mc_plot_raster(example_tomst_data, "plots/T1T2.png", sensors = c("TMS_T1", "TMS_T2"), png_height = 500)

example_tomst_data <- mc_filter(example_tomst_data, sensors=c("TS_T", "TMS_T1", "TMS_T2", "TMS_T3", "TMS_TMSmoisture"))
line_plot <- mc_plot_line(example_tomst_data, "plots/lines.pdf")
line_plot <- line_plot + ggplot2::scale_color_manual(values=c("pink", "orange", "red", "blue", "red"))
print(line_plot)
mc_plot_line(example_tomst_data, "plots/lines.png")

example_hour_data <- mc_agg(example_tomst_data, "mean", period="hour")

mc_plot_image(example_hour_data, "plots/image_hour.png", "T1 sensors", sensors="TMS_T1_mean")
