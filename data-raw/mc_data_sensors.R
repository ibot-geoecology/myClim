source("R/model.R")

TMS_T1 <- new("mc_Sensor")
TMS_T1@sensor_id <- "TMS_T1"
TMS_T1@logger <- "TMS"
TMS_T1@physical <- "T"
TMS_T1@default_height <- -0.08
TMS_T1@min_value <- -40
TMS_T1@max_value <- 60
TMS_T1@plot_color <- "#2000EEB0"
TMS_T1@plot_line_width <- 2

TMS_T2 <- new("mc_Sensor")
TMS_T2@sensor_id <- "TMS_T2"
TMS_T2@logger <- "TMS"
TMS_T2@physical <- "T"
TMS_T2@default_height <- 0
TMS_T2@min_value <- -50
TMS_T2@max_value <- 60
TMS_T2@plot_color <- "green4"

TMS_T3 <- new("mc_Sensor")
TMS_T3@sensor_id <- "TMS_T3"
TMS_T3@logger <- "TMS"
TMS_T3@physical <- "T"
TMS_T3@default_height <- 0.15
TMS_T3@min_value <- -50
TMS_T3@max_value <- 60
TMS_T3@plot_color <- "red3"

TMS_TMSmoisture <- new("mc_Sensor")
TMS_TMSmoisture@sensor_id <- "TMS_TMSmoisture"
TMS_TMSmoisture@logger <- "TMS"
TMS_TMSmoisture@physical <- "TMSmoisture"
TMS_TMSmoisture@default_height <- -0.07
TMS_TMSmoisture@min_value <- 0
TMS_TMSmoisture@max_value <- 4000
TMS_TMSmoisture@plot_color <- "steelblue"
TMS_TMSmoisture@plot_line_width <- 2

TMS_moisture <- new("mc_Sensor")
TMS_moisture@sensor_id <- "TMS_moisture"
TMS_moisture@logger <- "TMS"
TMS_moisture@physical <- "moisture"
TMS_moisture@default_height <- -0.07
TMS_moisture@min_value <- 0
TMS_moisture@max_value <- 1
TMS_moisture@plot_color <- "steelblue"
TMS_moisture@plot_line_width <- 2

TM_T <- new("mc_Sensor")
TM_T@sensor_id <- "TM_T"
TM_T@logger <- "ThermoDatalogger"
TM_T@physical <- "T"
TM_T@default_height <- -0.08
TM_T@min_value <- -40
TM_T@max_value <- 60
TM_T@plot_color <- "#2000EEB0"
TM_T@plot_line_width <- 2

mc_data_sensors <- list(
    TMS_T1 = TMS_T1,
    TMS_T2 = TMS_T2,
    TMS_T3 = TMS_T3,
    TMS_TMSmoisture = TMS_TMSmoisture,
    TMS_moisture = TMS_moisture,
    TM_T = TM_T
)

usethis::use_data(mc_data_sensors, overwrite = TRUE)
