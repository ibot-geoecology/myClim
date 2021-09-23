source("R/model.R")

mc_data_physical <- list(
    T = mc_Physical(
       name = "T",
       description = "Temperature",
       units = "°C"
    ),
    TMS_moisture = mc_Physical(
       name = "TMS_moisture",
       description = "TMS moisture",
       units = "raw count"
    )
)

usethis::use_data(mc_data_physical, overwrite = TRUE)
