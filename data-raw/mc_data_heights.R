source("R/model.R")

mc_data_heights <- as.data.frame(tibble::tribble(
                                  ~logger_type,                         ~sensor_name,         ~height, ~suffix,
    .model_const_LOGGER_TOMST_THERMODATALOGGER,             .model_const_SENSOR_TS_T,    "air 200 cm",      NA,
                 .model_const_LOGGER_TOMST_TMS,           .model_const_SENSOR_TMS_T1,     "soil 8 cm",      NA,
                 .model_const_LOGGER_TOMST_TMS,           .model_const_SENSOR_TMS_T2,      "air 2 cm",      NA,
                 .model_const_LOGGER_TOMST_TMS,           .model_const_SENSOR_TMS_T3,     "air 15 cm",      NA,
                 .model_const_LOGGER_TOMST_TMS,  .model_const_SENSOR_TMS_TMSmoisture,  "soil 0-15 cm",      NA,
         .model_const_LOGGER_TOMST_DENDROMETER,           .model_const_SENSOR_DEND_T,    "air 130 cm",      NA,
         .model_const_LOGGER_TOMST_DENDROMETER, .model_const_SENSOR_DEND_TOMSTdendro,    "air 130 cm",      NA,
                      .model_const_LOGGER_HOBO,         .model_const_SENSOR_HOBO_T_C,    "air 150 cm",      NA,
                      .model_const_LOGGER_HOBO,          .model_const_SENSOR_HOBO_RH,    "air 150 cm",      NA,
             .model_const_LOGGER_TOMST_TMS_L45,           .model_const_SENSOR_TMS_T1,    "soil 40 cm",  "_L45",
             .model_const_LOGGER_TOMST_TMS_L45,           .model_const_SENSOR_TMS_T2,    "soil 30 cm",  "_L45",
             .model_const_LOGGER_TOMST_TMS_L45,           .model_const_SENSOR_TMS_T3,     "air 15 cm",  "_L45",
             .model_const_LOGGER_TOMST_TMS_L45,  .model_const_SENSOR_TMS_TMSmoisture, "soil 30-44 cm",  "_L45",
))

usethis::use_data(mc_data_heights, overwrite = TRUE)
