library(myClim)

# simple + 1°C

data <- mc_read_files("examples/data/join/1deg", "TOMST")
cleaned_data <- mc_prep_clean(data)
joined_data <- mc_join(cleaned_data, comp_sensors=c("TMS_T1", "TMS_T2"))

# CS_112

files_table <- readRDS("examples/data/join/CS_112/files_table.rds")
data <- mc_read_data(files_table)
cleaned_data <- mc_prep_clean(data)
joined_data <- mc_join(cleaned_data, comp_sensors=c("TMS_T1", "TS_T"))
mc_plot_line(data, "plots/CS_112.pdf", sensors=c("TMS_T1", "TMS_T2", "TMS_T3", "TS_T"))
