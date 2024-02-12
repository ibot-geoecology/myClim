devtools::load_all()

mc_data_formats <- new.env()

mc_data_formats[[.model_const_DATA_FORMAT_TOMST]] <- new("mc_TOMSTDataFormat")
mc_data_formats[[.model_const_DATA_FORMAT_TOMST]]@skip <- 0
mc_data_formats[[.model_const_DATA_FORMAT_TOMST]]@separator <- ";"
mc_data_formats[[.model_const_DATA_FORMAT_TOMST]]@date_column <- 2
mc_data_formats[[.model_const_DATA_FORMAT_TOMST]]@date_format <- c("%Y.%m.%d %H:%M", "%d.%m.%Y %H:%M")
mc_data_formats[[.model_const_DATA_FORMAT_TOMST]]@na_strings <- c("-200", "")
mc_data_formats[[.model_const_DATA_FORMAT_TOMST]]@error_value <- -100
mc_data_formats[[.model_const_DATA_FORMAT_TOMST]]@filename_serial_number_pattern <- "data_(\\d+)_(?:\\d{4}_\\d{2}_\\d{2}_)?\\d+\\.csv$"
#                                                                           ;datetime   ;    ;T1             ;T2             ;T3             ;mois;    ;
mc_data_formats[[.model_const_DATA_FORMAT_TOMST]]@data_row_pattern <- "^\\d+;[\\d.: -/]+;\\d+;-?\\d+[.,]?\\d*;-?\\d+[.,]?\\d*;-?\\d+[.,]?\\d*;\\d+;\\d+;\\d+.*$"
mc_data_formats[[.model_const_DATA_FORMAT_TOMST]]@col_types <- "iciccciin"
mc_data_formats[[.model_const_DATA_FORMAT_TOMST]]@tz_offset <- 0

mc_data_formats[[.model_const_DATA_FORMAT_TOMST_join]] <- new("mc_TOMSTJoinDataFormat")
mc_data_formats[[.model_const_DATA_FORMAT_TOMST_join]]@skip <- 0
mc_data_formats[[.model_const_DATA_FORMAT_TOMST_join]]@separator <- ";"
mc_data_formats[[.model_const_DATA_FORMAT_TOMST_join]]@date_column <- 4
mc_data_formats[[.model_const_DATA_FORMAT_TOMST_join]]@date_format <- c("%d.%m.%Y %H:%M", "%d.%m.%Y %H:%M:%S")
mc_data_formats[[.model_const_DATA_FORMAT_TOMST_join]]@na_strings <- c("NA", "")
mc_data_formats[[.model_const_DATA_FORMAT_TOMST_join]]@col_types <- "icccdddic"
mc_data_formats[[.model_const_DATA_FORMAT_TOMST_join]]@filename_serial_number_pattern <- "(.+)\\.csv$"
#                                                                                ;  ;  ;datetime ;T1                   ;T2                   ;T3                   ;mois       ;
mc_data_formats[[.model_const_DATA_FORMAT_TOMST_join]]@data_row_pattern <- "^\\d+;.+;.+;[\\d.: ]+;(?:-?\\d*\\.?\\d*|NA);(?:-?\\d*\\.?\\d*|NA);(?:-?\\d*\\.?\\d*|NA);(?:\\d*|NA);.*$"
mc_data_formats[[.model_const_DATA_FORMAT_TOMST_join]]@tz_offset <- 0

mc_data_formats[[.model_const_DATA_FORMAT_HOBO]] <- new("mc_HOBODataFormat")
mc_data_formats[[.model_const_DATA_FORMAT_HOBO]]@skip <- 0
mc_data_formats[[.model_const_DATA_FORMAT_HOBO]]@logger_type <- .model_const_LOGGER_HOBO_U23_001A
mc_data_formats[[.model_const_DATA_FORMAT_HOBO]]@filename_serial_number_pattern <- "(.+)\\.(?:csv|txt)$"
mc_data_formats[[.model_const_DATA_FORMAT_HOBO]]@na_strings <- ""

usethis::use_data(mc_data_formats, overwrite = TRUE)
