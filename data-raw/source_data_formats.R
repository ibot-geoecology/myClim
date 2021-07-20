## code to prepare `source_data_formats` dataset goes here
source("R/model.R")

data.source_data_formats <- list(
  TMS4 = new("DataFormat",
             header = FALSE,
             separator = ";",
             date_column = 1,
             date_format = "%Y.%m.%d %H:%M")
)

usethis::use_data(data.source_data_formats, overwrite = TRUE)
