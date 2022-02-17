source("R/model.R")

T <- new("mc_Physical")
T@name <- .model_const_PHYSICAL_T
T@description <- "Temperature °C"
T@units <- "°C"
T@viridis_color_map <- "C"
T@scale_coeff <- 1/30

TMSmoisture <- new("mc_Physical")
TMSmoisture@name <- "TMSmoisture"
TMSmoisture@description <- "Soil moisture (TDT signal)"
TMSmoisture@units <- "TDT signal"
TMSmoisture@viridis_color_map <- "D"
TMSmoisture@scale_coeff <- 1/3000

moisture <- new("mc_Physical")
moisture@name <- "moisture"
moisture@description <- "Volum. soil moisture"
moisture@units <- "ratio"
moisture@viridis_color_map <- "D"
moisture@scale_coeff <- 1

mc_data_physical <- list(
    T = T,
    TMSmoisture = TMSmoisture,
    moisture = moisture
)

usethis::use_data(mc_data_physical, overwrite = TRUE)
