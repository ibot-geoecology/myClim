source("R/model.R")

mc_data_sensors <- list(
    TMS_T1 = mc_Sensor(
       name = "T1",
       logger = "TMS",
       physical = "T",
       default_height = -0.08,
       min_value = -40,
       max_value = 60,
       plot_color = "#2000EEB0"
    ),
    TMS_T2 = mc_Sensor(
       name = "T2",
       logger = "TMS",
       physical = "T",
       default_height = 0,
       min_value = -50,
       max_value = 60,
       plot_color = "green4"
    ),
    TMS_T3 = mc_Sensor(
       name = "T3",
       logger = "TMS",
       physical = "T",
       default_height = 0.15,
       min_value = -50,
       max_value = 60,
       plot_color = "red3"
    ),
    TMS_moisture = mc_Sensor(
       name = "moisture",
       logger = "TMS",
       physical = "TMS_moisture",
       default_height = -0.07,
       min_value = 0,
       max_value = 4000,
       plot_color = "steelblue"
    )
)

usethis::use_data(mc_data_sensors, overwrite = TRUE)
