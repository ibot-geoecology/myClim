devtools::load_all()

files_table <- as.data.frame(tibble::tribble(
                                    ~path, ~locality_id, ~data_format, ~serial_number,        ~date_format,
"examples/data/TOMST/data_94184102_0.csv",      "A6W79",      "TOMST",             NA,                  NA,
"examples/data/TOMST/data_94184103_0.csv",      "A2E32",      "TOMST",             NA,                  NA,
        "examples/data/HOBO/20024338.txt",      "A2E32",       "HOBO",             NA, "%d.%m.%Y %H:%M:%S",
"examples/data/TOMST/data_91184101_0.csv",      "A1E05",      "TOMST",             NA,                  NA,
"examples/data/TOMST/data_92201058_0.csv",      "A1E05",      "TOMST",             NA,                  NA,
))


mc_data_example_source <- mc_read_data(files_table, "examples/data/TOMST/localities_table.csv", clean = FALSE)
mc_data_example_source <- mc_prep_crop(mc_data_example_source, end = lubridate::ymd("2021-02-01"))

mc_data_example_clean <- mc_prep_clean(mc_data_example_source, silent = TRUE)

mc_data_example_calc <- mc_agg(mc_data_example_clean)

usethis::use_data(mc_data_example_source, overwrite = TRUE)
usethis::use_data(mc_data_example_clean, overwrite = TRUE)
usethis::use_data(mc_data_example_calc, overwrite = TRUE)
