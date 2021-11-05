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
      na_strings = c("NA", " NA"),
      filename_serial_number_pattern = "data_(\\d+)_\\d+\\.csv$",
      data_row_pattern = "^\\d+;[^;]+;\\d+;-?\\d+\\.?\\d*;-?\\d+\\.?\\d*;-?\\d+\\.?\\d*;\\d+;\\d+;\\d+;?$"
    )
)

usethis::use_data(mc_data_formats, overwrite = TRUE)
