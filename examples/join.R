library(myClim)

# simple + 1°C

data <- mc_read_files("examples/data/join/1deg", "TOMST")
joined_data <- mc_join(data, comp_sensors=c("TMS_T1", "TMS_T2"))

# CS_112

files_table <- readRDS("examples/data/join/CS_112/files_table.rds")
data <- mc_read_data(files_table)
joined_data <- mc_join(data, comp_sensors=c("TMS_T1", "Thermo_T"))
mc_plot_line(data, sensors=c("TMS_T1", "TMS_T2", "TMS_T3", "Thermo_T"))
