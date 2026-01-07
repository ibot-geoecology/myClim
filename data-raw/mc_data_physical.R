source("R/model.R")

mc_data_physical <- new.env()

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

mc_data_physical[[.model_const_PHYSICAL_VWC]] <- new("mc_Physical")
mc_data_physical[[.model_const_PHYSICAL_VWC]]@name <- .model_const_PHYSICAL_VWC
mc_data_physical[[.model_const_PHYSICAL_VWC]]@description <- "Volum. soil moisture"
mc_data_physical[[.model_const_PHYSICAL_VWC]]@units <- "m³/m³"
mc_data_physical[[.model_const_PHYSICAL_VWC]]@viridis_color_map <- "D"
mc_data_physical[[.model_const_PHYSICAL_VWC]]@scale_coeff <- 1

mc_data_physical[[.model_const_PHYSICAL_p_kPa]] <- new("mc_Physical")
mc_data_physical[[.model_const_PHYSICAL_p_kPa]]@name <- .model_const_PHYSICAL_p_kPa
mc_data_physical[[.model_const_PHYSICAL_p_kPa]]@description <- "Presure kPa"
mc_data_physical[[.model_const_PHYSICAL_p_kPa]]@units <- "kPa"
mc_data_physical[[.model_const_PHYSICAL_p_kPa]]@viridis_color_map <- "D"
mc_data_physical[[.model_const_PHYSICAL_p_kPa]]@scale_coeff <- 1/100

mc_data_physical[[.model_const_PHYSICAL_RH]] <- new("mc_Physical")
mc_data_physical[[.model_const_PHYSICAL_RH]]@name <- .model_const_PHYSICAL_RH
mc_data_physical[[.model_const_PHYSICAL_RH]]@description <- "Relative humidity %"
mc_data_physical[[.model_const_PHYSICAL_RH]]@units <- "%"
mc_data_physical[[.model_const_PHYSICAL_RH]]@viridis_color_map <- "D"
mc_data_physical[[.model_const_PHYSICAL_RH]]@scale_coeff <- 1/100

mc_data_physical[[.model_const_PHYSICAL_T_C]] <- new("mc_Physical")
mc_data_physical[[.model_const_PHYSICAL_T_C]]@name <- .model_const_PHYSICAL_T_C
mc_data_physical[[.model_const_PHYSICAL_T_C]]@description <- "Temperature °C"
mc_data_physical[[.model_const_PHYSICAL_T_C]]@units <- "°C"
mc_data_physical[[.model_const_PHYSICAL_T_C]]@viridis_color_map <- "C"
mc_data_physical[[.model_const_PHYSICAL_T_C]]@scale_coeff <- 1/30

mc_data_physical[[.model_const_PHYSICAL_t_h]] <- new("mc_Physical")
mc_data_physical[[.model_const_PHYSICAL_t_h]]@name <- .model_const_PHYSICAL_t_h
mc_data_physical[[.model_const_PHYSICAL_t_h]]@description <- "Time hour"
mc_data_physical[[.model_const_PHYSICAL_t_h]]@units <- "hour"
mc_data_physical[[.model_const_PHYSICAL_t_h]]@viridis_color_map <- "D"
mc_data_physical[[.model_const_PHYSICAL_t_h]]@scale_coeff <- 1/24

mc_data_physical[[.model_const_PHYSICAL_moisture_raw]] <- new("mc_Physical")
mc_data_physical[[.model_const_PHYSICAL_moisture_raw]]@name <- .model_const_PHYSICAL_moisture_raw
mc_data_physical[[.model_const_PHYSICAL_moisture_raw]]@description <- "Moisture signal"
mc_data_physical[[.model_const_PHYSICAL_moisture_raw]]@units <- "raw units"
mc_data_physical[[.model_const_PHYSICAL_moisture_raw]]@viridis_color_map <- "D"
mc_data_physical[[.model_const_PHYSICAL_moisture_raw]]@scale_coeff <- 1/3000

mc_data_physical[[.model_const_PHYSICAL_radius_raw]] <- new("mc_Physical")
mc_data_physical[[.model_const_PHYSICAL_radius_raw]]@name <- .model_const_PHYSICAL_radius_raw
mc_data_physical[[.model_const_PHYSICAL_radius_raw]]@description <- "Dendrometer signal"
mc_data_physical[[.model_const_PHYSICAL_radius_raw]]@units <- "raw units"
mc_data_physical[[.model_const_PHYSICAL_radius_raw]]@viridis_color_map <- "D"
mc_data_physical[[.model_const_PHYSICAL_radius_raw]]@scale_coeff <- 1/10000

mc_data_physical[[.model_const_PHYSICAL_v]] <- new("mc_Physical")
mc_data_physical[[.model_const_PHYSICAL_v]]@name <- .model_const_PHYSICAL_v
mc_data_physical[[.model_const_PHYSICAL_v]]@description <- "Speed m/s"
mc_data_physical[[.model_const_PHYSICAL_v]]@units <- "m/s"
mc_data_physical[[.model_const_PHYSICAL_v]]@viridis_color_map <- "D"
mc_data_physical[[.model_const_PHYSICAL_v]]@scale_coeff <- 1/10

mc_data_physical[[.model_const_PHYSICAL_SWP_neg_bar]] <- new("mc_Physical")
mc_data_physical[[.model_const_PHYSICAL_SWP_neg_bar]]@name <- .model_const_PHYSICAL_SWP_neg_bar
mc_data_physical[[.model_const_PHYSICAL_SWP_neg_bar]]@description <- "SWP -bar"
mc_data_physical[[.model_const_PHYSICAL_SWP_neg_bar]]@units <- "-bar"
mc_data_physical[[.model_const_PHYSICAL_SWP_neg_bar]]@viridis_color_map <- "D"
mc_data_physical[[.model_const_PHYSICAL_SWP_neg_bar]]@scale_coeff <- 1

mc_data_physical[[.model_const_PHYSICAL_PPFD]] <- new("mc_Physical")
mc_data_physical[[.model_const_PHYSICAL_PPFD]]@name <- .model_const_PHYSICAL_PPFD
mc_data_physical[[.model_const_PHYSICAL_PPFD]]@description <- "PPFD μmol/m²/s"
mc_data_physical[[.model_const_PHYSICAL_PPFD]]@units <- "μmol/m²/s"
mc_data_physical[[.model_const_PHYSICAL_PPFD]]@viridis_color_map <- "D"
mc_data_physical[[.model_const_PHYSICAL_PPFD]]@scale_coeff <- 1

usethis::use_data(mc_data_physical, overwrite = TRUE)
