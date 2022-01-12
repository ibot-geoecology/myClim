source("R/model.R")

## code to prepare `source_data_formats` dataset goes here
mc_data_formats <- list(
    HOBO_RH = mc_DataFormat(
               has_header = TRUE,
               separator = ";",
               date_column = 1
    ),
    TOMST = mc_TOMSTDataFormat(
      has_header = FALSE,
      separator = ";",
      date_column = 2,
      na_strings = "-200",
      filename_serial_number_pattern = "data_(\\d+)_\\d+\\.csv$",
      data_row_pattern = "^\\d+;[\\d.: ]+;\\d+;-?\\d+[.,]?\\d*;-?\\d+[.,]?\\d*;-?\\d+[.,]?\\d*;\\d+;\\d+;\\d+.*$"
    ),
    TOMST_join = mc_TOMSTJoinDataFormat(
        has_header = FALSE,
        separator = ";",
        date_column = 4,
        date_format = "%d.%m.%Y %H:%M",
        na_strings = c("NA"),
        filename_serial_number_pattern = "(.+)\\.csv$",
        #                        ;  ;  ;datetime ;T1            ;T2            ;T3            ;mois;mois        ;
        data_row_pattern = "^\\d+;.+;.+;[\\d.: ]+;-?\\d+\\.?\\d*;-?\\d*\\.?\\d*;-?\\d*\\.?\\d*;\\d+;\\d+\\.?\\d*;.*$"
    )
)

usethis::use_data(mc_data_formats, overwrite = TRUE)
