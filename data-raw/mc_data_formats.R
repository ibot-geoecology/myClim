source("R/model.R")

TOMST <- new("mc_TOMSTDataFormat")
TOMST@has_header <- FALSE
TOMST@separator <- ";"
TOMST@date_column <- 2
TOMST@na_strings <- "-200"
TOMST@filename_serial_number_pattern <- "data_(\\d+)_\\d+\\.csv$"
#                               ;datetime ;    ;T1             ;T2             ;T3             ;mois;    ;
TOMST@data_row_pattern <- "^\\d+;[\\d.: ]+;\\d+;-?\\d+[.,]?\\d*;-?\\d+[.,]?\\d*;-?\\d+[.,]?\\d*;\\d+;\\d+;\\d+.*$"

TOMST_join <- new("mc_TOMSTJoinDataFormat")
TOMST_join@has_header <- FALSE
TOMST_join@separator <- ";"
TOMST_join@date_column <- 4
TOMST_join@date_format <- "%d.%m.%Y %H:%M"
TOMST_join@na_strings <- "NA"
TOMST_join@filename_serial_number_pattern <- "(.+)\\.csv$"
#                                    ;  ;  ;datetime ;T1            ;T2            ;T3            ;mois;mois        ;
TOMST_join@data_row_pattern <- "^\\d+;.+;.+;[\\d.: ]+;-?\\d*\\.?\\d*;-?\\d*\\.?\\d*;-?\\d*\\.?\\d*;\\d*;\\d*\\.?\\d*;.*$"

mc_data_formats <- list(
    TOMST = TOMST,
    TOMST_join = TOMST_join
)

usethis::use_data(mc_data_formats, overwrite = TRUE)
