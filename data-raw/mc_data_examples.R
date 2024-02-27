devtools::load_all()

files_table <- as.data.frame(tibble::tribble(
                                    ~path, ~locality_id, ~data_format, ~serial_number,        ~date_format,
"examples/data/TOMST/data_94184102_0.csv",      "A6W79",      "TOMST",             NA,                  NA,
"examples/data/TOMST/data_94184103_0.csv",      "A2E32",      "TOMST",             NA,                  NA,
        "examples/data/HOBO/20024338.txt",      "A2E32",       "HOBO",             NA, "%d.%m.%Y %H:%M:%S",
"examples/data/TOMST/data_91184101_0.csv",      "A1E05",      "TOMST",             NA,                  NA,
"examples/data/TOMST/data_92201058_0.csv",      "A1E05",      "TOMST",             NA,                  NA,
))


mc_data_example_raw <- mc_read_data(files_table, "examples/data/TOMST/localities_table.csv", clean = FALSE)

sensor_function <- function(sensor) {
    paths <- as.data.frame(stringr::str_match(sensor$states$value, ".+(\\/myClim\\/examples\\/data\\/.+)"))$V2
    sensor$states$value <- paste0(".", paths)
    return(sensor)
}
logger_function <- function(logger) {
    logger$sensors <- purrr::map(logger$sensors, sensor_function)
    return(logger)
}
locality_function <- function(locality) {
    locality$loggers <- purrr::map(locality$loggers, logger_function)
    return(locality)
}
mc_data_example_raw$localities <- purrr::map(mc_data_example_raw$localities, locality_function)

mc_data_example_raw <- mc_prep_crop(mc_data_example_raw, end = lubridate::ymd("2021-02-01"))

mc_data_example_clean <- mc_prep_clean(mc_data_example_raw, silent = TRUE)

mc_data_example_agg <- mc_agg(mc_data_example_clean)

usethis::use_data(mc_data_example_raw, overwrite = TRUE)
usethis::use_data(mc_data_example_clean, overwrite = TRUE)
usethis::use_data(mc_data_example_agg, overwrite = TRUE)
