source("R/model.R")

mc_data_heights <- as.data.frame(tibble::tribble(
                              ~logger_type,               ~sensor_name,         ~height, ~suffix,
.model_const_LOGGER_TOMST_THERMODATALOGGER,   mc_const_SENSOR_Thermo_T,    "air 200 cm",      NA,
             .model_const_LOGGER_TOMST_TMS,     mc_const_SENSOR_TMS_T1,     "soil 8 cm",      NA,
             .model_const_LOGGER_TOMST_TMS,     mc_const_SENSOR_TMS_T2,      "air 2 cm",      NA,
             .model_const_LOGGER_TOMST_TMS,     mc_const_SENSOR_TMS_T3,     "air 15 cm",      NA,
             .model_const_LOGGER_TOMST_TMS,  mc_const_SENSOR_TMS_moist,  "soil 0-15 cm",      NA,
     .model_const_LOGGER_TOMST_DENDROMETER,   mc_const_SENSOR_Dendro_T,    "air 130 cm",      NA,
     .model_const_LOGGER_TOMST_DENDROMETER, mc_const_SENSOR_Dendro_raw,    "air 130 cm",      NA,
         .model_const_LOGGER_HOBO_U23_001A,     mc_const_SENSOR_HOBO_T,    "air 150 cm",      NA,
         .model_const_LOGGER_HOBO_U23_001A,    mc_const_SENSOR_HOBO_RH,    "air 150 cm",      NA,
          .model_const_LOGGER_HOBO_U23_004,     mc_const_SENSOR_HOBO_T,      "air 2 cm",      NA,
          .model_const_LOGGER_HOBO_U23_004,  mc_const_SENSOR_HOBO_EXTT,     "soil 8 cm",      NA,
         .model_const_LOGGER_TOMST_TMS_L45,     mc_const_SENSOR_TMS_T1,    "soil 40 cm",  "_L45",
         .model_const_LOGGER_TOMST_TMS_L45,     mc_const_SENSOR_TMS_T2,    "soil 30 cm",  "_L45",
         .model_const_LOGGER_TOMST_TMS_L45,     mc_const_SENSOR_TMS_T3,     "air 15 cm",  "_L45",
         .model_const_LOGGER_TOMST_TMS_L45,  mc_const_SENSOR_TMS_moist, "soil 30-44 cm",  "_L45",
       .model_const_LOGGER_EMS_MINIKIN_SP1,    mc_const_SENSOR_SP1_SWP,    "soil 10 cm",      NA,
       .model_const_LOGGER_EMS_MINIKIN_SP1,      mc_const_SENSOR_SP1_T,     "soil 0 cm",      NA,
       .model_const_LOGGER_EMS_MINIKIN_QTI,   mc_const_SENSOR_QTi_PPFD,    "air 100 cm",      NA,
       .model_const_LOGGER_EMS_MINIKIN_QTI,      mc_const_SENSOR_QTi_T,    "air 100 cm",      NA,
       .model_const_LOGGER_EMS_MINIKIN_TH2,      mc_const_SENSOR_TH2_T,    "air 150 cm",      NA,
       .model_const_LOGGER_EMS_MINIKIN_TH2,     mc_const_SENSOR_TH2_RH,    "air 150 cm",      NA
))

usethis::use_data(mc_data_heights, overwrite = TRUE)
