source("R/model.R")

mc_data_physical <- new.env()

mc_data_physical[[.model_const_PHYSICAL_T_C]] <- new("mc_Physical")
mc_data_physical[[.model_const_PHYSICAL_T_C]]@name <- .model_const_PHYSICAL_T_C
mc_data_physical[[.model_const_PHYSICAL_T_C]]@description <- "Temperature °C"
mc_data_physical[[.model_const_PHYSICAL_T_C]]@units <- "°C"
mc_data_physical[[.model_const_PHYSICAL_T_C]]@viridis_color_map <- "C"
mc_data_physical[[.model_const_PHYSICAL_T_C]]@scale_coeff <- 1/30

mc_data_physical[[.model_const_PHYSICAL_T_F]] <- new("mc_Physical")
mc_data_physical[[.model_const_PHYSICAL_T_F]]@name <- .model_const_PHYSICAL_T_F
mc_data_physical[[.model_const_PHYSICAL_T_F]]@description <- "Temperature °F"
mc_data_physical[[.model_const_PHYSICAL_T_F]]@units <- "°F"
mc_data_physical[[.model_const_PHYSICAL_T_F]]@viridis_color_map <- "C"
mc_data_physical[[.model_const_PHYSICAL_T_F]]@scale_coeff <- 1/54

mc_data_physical[[.model_const_PHYSICAL_TMSmoisture]] <- new("mc_Physical")
mc_data_physical[[.model_const_PHYSICAL_TMSmoisture]]@name <- .model_const_PHYSICAL_TMSmoisture
mc_data_physical[[.model_const_PHYSICAL_TMSmoisture]]@description <- "Soil moisture (TDT signal)"
mc_data_physical[[.model_const_PHYSICAL_TMSmoisture]]@units <- "TDT signal"
mc_data_physical[[.model_const_PHYSICAL_TMSmoisture]]@viridis_color_map <- "D"
mc_data_physical[[.model_const_PHYSICAL_TMSmoisture]]@scale_coeff <- 1/3000

mc_data_physical[[.model_const_PHYSICAL_moisture]] <- new("mc_Physical")
mc_data_physical[[.model_const_PHYSICAL_moisture]]@name <- .model_const_PHYSICAL_moisture
mc_data_physical[[.model_const_PHYSICAL_moisture]]@description <- "Volum. soil moisture"
mc_data_physical[[.model_const_PHYSICAL_moisture]]@units <- "ratio"
mc_data_physical[[.model_const_PHYSICAL_moisture]]@viridis_color_map <- "D"
mc_data_physical[[.model_const_PHYSICAL_moisture]]@scale_coeff <- 1

mc_data_physical[[.model_const_PHYSICAL_RH_perc]] <- new("mc_Physical")
mc_data_physical[[.model_const_PHYSICAL_RH_perc]]@name <- .model_const_PHYSICAL_RH_perc
mc_data_physical[[.model_const_PHYSICAL_RH_perc]]@description <- "Relative humidity %"
mc_data_physical[[.model_const_PHYSICAL_RH_perc]]@units <- "%"
mc_data_physical[[.model_const_PHYSICAL_RH_perc]]@viridis_color_map <- "D"
mc_data_physical[[.model_const_PHYSICAL_RH_perc]]@scale_coeff <- 1

mc_data_physical[[.model_const_PHYSICAL_l_cm]] <- new("mc_Physical")
mc_data_physical[[.model_const_PHYSICAL_l_cm]]@name <- .model_const_PHYSICAL_l_cm
mc_data_physical[[.model_const_PHYSICAL_l_cm]]@description <- "Length cm"
mc_data_physical[[.model_const_PHYSICAL_l_cm]]@units <- "cm"
mc_data_physical[[.model_const_PHYSICAL_l_cm]]@viridis_color_map <- "D"
mc_data_physical[[.model_const_PHYSICAL_l_cm]]@scale_coeff <- 1/10

mc_data_physical[[.model_const_PHYSICAL_l_mm]] <- new("mc_Physical")
mc_data_physical[[.model_const_PHYSICAL_l_mm]]@name <- .model_const_PHYSICAL_l_mm
mc_data_physical[[.model_const_PHYSICAL_l_mm]]@description <- "Length mm"
mc_data_physical[[.model_const_PHYSICAL_l_mm]]@units <- "mm"
mc_data_physical[[.model_const_PHYSICAL_l_mm]]@viridis_color_map <- "D"
mc_data_physical[[.model_const_PHYSICAL_l_mm]]@scale_coeff <- 1/10

mc_data_physical[[.model_const_PHYSICAL_l_um]] <- new("mc_Physical")
mc_data_physical[[.model_const_PHYSICAL_l_um]]@name <- .model_const_PHYSICAL_l_um
mc_data_physical[[.model_const_PHYSICAL_l_um]]@description <- "Length μm"
mc_data_physical[[.model_const_PHYSICAL_l_um]]@units <- "μm"
mc_data_physical[[.model_const_PHYSICAL_l_um]]@viridis_color_map <- "D"
mc_data_physical[[.model_const_PHYSICAL_l_um]]@scale_coeff <- 1/1000

mc_data_physical[[.model_const_PHYSICAL_v]] <- new("mc_Physical")
mc_data_physical[[.model_const_PHYSICAL_v]]@name <- .model_const_PHYSICAL_v
mc_data_physical[[.model_const_PHYSICAL_v]]@description <- "Speed m/s"
mc_data_physical[[.model_const_PHYSICAL_v]]@units <- "m/s"
mc_data_physical[[.model_const_PHYSICAL_v]]@viridis_color_map <- "D"
mc_data_physical[[.model_const_PHYSICAL_v]]@scale_coeff <- 1/10

mc_data_physical[[.model_const_PHYSICAL_t_h]] <- new("mc_Physical")
mc_data_physical[[.model_const_PHYSICAL_t_h]]@name <- .model_const_PHYSICAL_t_h
mc_data_physical[[.model_const_PHYSICAL_t_h]]@description <- "Time hour"
mc_data_physical[[.model_const_PHYSICAL_t_h]]@units <- "hour"
mc_data_physical[[.model_const_PHYSICAL_t_h]]@viridis_color_map <- "D"
mc_data_physical[[.model_const_PHYSICAL_t_h]]@scale_coeff <- 1/24

mc_data_physical[[.model_const_PHYSICAL_TOMST_r_delta]] <- new("mc_Physical")
mc_data_physical[[.model_const_PHYSICAL_TOMST_r_delta]]@name <- .model_const_PHYSICAL_TOMST_r_delta
mc_data_physical[[.model_const_PHYSICAL_TOMST_r_delta]]@description <- "Radius difference raw units"
mc_data_physical[[.model_const_PHYSICAL_TOMST_r_delta]]@units <- "raw units"
mc_data_physical[[.model_const_PHYSICAL_TOMST_r_delta]]@viridis_color_map <- "D"
mc_data_physical[[.model_const_PHYSICAL_TOMST_r_delta]]@scale_coeff <- 1/10000

usethis::use_data(mc_data_physical, overwrite = TRUE)
