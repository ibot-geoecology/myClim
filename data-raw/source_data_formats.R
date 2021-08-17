source("R/model.R")

## code to prepare `source_data_formats` dataset goes here
data.source_data_formats <- list(
    HOBO_RH = new("model.DataFormat",
               has_header = TRUE,
               separator = ";",
               date_column = 1
    ),
    TMS = new("model.TMS3DataFormat",
               has_header = FALSE,
               separator = ";",
               date_column = 2,
               na_strings = c("NA", " NA"),
               filename_serial_number_pattern = "data_(\\d+)_\\d+\\.csv"
    )
)

usethis::use_data(data.source_data_formats, overwrite = TRUE)
