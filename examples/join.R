library(myClim)

data <- mc_read_files("examples/data/join", "TOMST")
cleaned_data <- mc_prep_clean(data)
joined_data <- mc_join(cleaned_data)
joined_data <- mc_join(cleaned_data, comp_sensors=c("TMS_T1", "TMS_T2"))
