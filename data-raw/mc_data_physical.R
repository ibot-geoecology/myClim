source("R/model.R")

T <- new("mc_Physical")
T@name <- "T"
T@description <- "Temperature °C"
T@units <- "°C"
T@viridis_color_map <- "C"

TMSmoisture <- new("mc_Physical")
TMSmoisture@name <- "TMSmoisture"
TMSmoisture@description <- "Soil moisture (TDT signal)"
TMSmoisture@units <- "TDT signal"
TMSmoisture@viridis_color_map <- "D"

moisture <- new("mc_Physical")
moisture@name <- "moisture"
moisture@description <- "Volum. soil moisture"
moisture@units <- "ratio"
moisture@viridis_color_map <- "D"

mc_data_physical <- list(
    T = T,
    TMSmoisture = TMSmoisture,
    moisture = moisture
)

usethis::use_data(mc_data_physical, overwrite = TRUE)
