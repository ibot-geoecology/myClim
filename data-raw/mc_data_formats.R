source("R/model.R")

mc_data_formats <- new.env()

mc_data_formats[[.model_const_DATA_FORMAT_TOMST]] <- new("mc_TOMSTDataFormat")
mc_data_formats[[.model_const_DATA_FORMAT_TOMST]]@has_header <- FALSE
mc_data_formats[[.model_const_DATA_FORMAT_TOMST]]@separator <- ";"
mc_data_formats[[.model_const_DATA_FORMAT_TOMST]]@date_column <- 2
mc_data_formats[[.model_const_DATA_FORMAT_TOMST]]@na_strings <- "-200"
mc_data_formats[[.model_const_DATA_FORMAT_TOMST]]@filename_serial_number_pattern <- "data_(\\d+)_\\d+\\.csv$"
#                                                                           ;datetime ;    ;T1             ;T2             ;T3             ;mois;    ;
mc_data_formats[[.model_const_DATA_FORMAT_TOMST]]@data_row_pattern <- "^\\d+;[\\d.: ]+;\\d+;-?\\d+[.,]?\\d*;-?\\d+[.,]?\\d*;-?\\d+[.,]?\\d*;\\d+;\\d+;\\d+.*$"

mc_data_formats[[.model_const_DATA_FORMAT_TOMST_join]] <- new("mc_TOMSTJoinDataFormat")
mc_data_formats[[.model_const_DATA_FORMAT_TOMST_join]]@has_header <- FALSE
mc_data_formats[[.model_const_DATA_FORMAT_TOMST_join]]@separator <- ";"
mc_data_formats[[.model_const_DATA_FORMAT_TOMST_join]]@date_column <- 4
mc_data_formats[[.model_const_DATA_FORMAT_TOMST_join]]@date_format <- "%d.%m.%Y %H:%M"
mc_data_formats[[.model_const_DATA_FORMAT_TOMST_join]]@na_strings <- "NA"
mc_data_formats[[.model_const_DATA_FORMAT_TOMST_join]]@filename_serial_number_pattern <- "(.+)\\.csv$"
#                                                                                ;  ;  ;datetime ;T1            ;T2            ;T3            ;mois;mois        ;
mc_data_formats[[.model_const_DATA_FORMAT_TOMST_join]]@data_row_pattern <- "^\\d+;.+;.+;[\\d.: ]+;-?\\d*\\.?\\d*;-?\\d*\\.?\\d*;-?\\d*\\.?\\d*;\\d*;\\d*\\.?\\d*;.*$"

mc_data_formats[[.model_const_DATA_FORMAT_HOBO]] <- new("mc_HOBODataFormat")
mc_data_formats[[.model_const_DATA_FORMAT_HOBO]]@has_header <- TRUE

usethis::use_data(mc_data_formats, overwrite = TRUE)
