source("R/model.R")

mc_data_physical <- list(
    T = mc_Physical(
       name = "T",
       description = "Temperature °C",
       units = "°C",
       viridis_color_map = "C"
    ),
    TMSmoisture = mc_Physical(
       name = "TMSmoisture",
       description = "Soil moisture (TDR counts)",
       units = "TDR counts",
       viridis_color_map = "D"
    ),
    moisture = mc_Physical(
       name = "moisture",
       description = "Volum. soil moisture",
       units = "ratio",
       viridis_color_map = "D"
    )
)

usethis::use_data(mc_data_physical, overwrite = TRUE)
