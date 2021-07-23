source("R/model.R")

## code to prepare `source_data_formats` dataset goes here
data.source_data_formats <- list(
    HOBO_RH = new("model.DataFormat",
               has_header = TRUE,
               separator = ";",
               date_column = 1
    ),
    TMS3 = new("model.TMS3DataFormat",
               has_header = TRUE,
               separator = ";",
               date_column = 2,
               na_strings = c("NA", " NA"),
               columns = list(
                   T1 = 4, T2 = 5, T3 = 6,
                   moisture = 7)
    )
)

usethis::use_data(data.source_data_formats, overwrite = TRUE)
