source("R/model.R")

mc_data_physical <- list(
    T = mc_Physical(
       name = "T",
       description = "Temperature",
       units = "°C"
    ),
    TMSmoisture = mc_Physical(
       name = "TMSmoisture",
       description = "Soil moisture (TDR counts)",
       units = "TDR counts"
    ),
    moisture = mc_Physical(
       name = "moisture",
       description = "Volum. soil moisture",
       units = "%"
    )
)

usethis::use_data(mc_data_physical, overwrite = TRUE)
