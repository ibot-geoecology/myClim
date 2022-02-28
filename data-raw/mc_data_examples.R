library(myClim)

mc_data_example_source <- mc_read_data("examples/data/TOMST/files_table.csv", "examples/data/TOMST/localities_table.csv")
mc_data_example_source <- mc_prep_crop(mc_data_example_source, end = lubridate::ymd("2021-02-01"))

mc_data_example_clean <- mc_prep_clean(mc_data_example_source, silent = TRUE)

mc_data_example_calc <- mc_agg(mc_data_example_clean)

usethis::use_data(mc_data_example_source, overwrite = TRUE)
usethis::use_data(mc_data_example_clean, overwrite = TRUE)
usethis::use_data(mc_data_example_calc, overwrite = TRUE)
