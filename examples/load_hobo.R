library(myClim)

data1 <- mc_read_files("examples/data/HOBO/20024354.txt", "HOBO", date_format = "%d.%m.%Y %H:%M:%S")

files_table <- as.data.frame(tibble::tribble(
    ~path,                                       ~locality_id, ~data_format, ~serial_number, ~date_format,
    "examples/data/HOBO/20024354.txt",           "AAA",          "HOBO",       NA_character_,  "%d.%m.%Y %H:%M:%S",
    "examples/data/HOBO/20024354_comma.csv",     "BBB",          "HOBO",       NA_character_,  "%y.%m.%d %H:%M:%S",
    "examples/data/HOBO/20024354_semicolon.txt", "CCC",          "HOBO",       NA_character_,  "%y.%m.%d %H:%M:%S",
    "examples/data/HOBO/20024354_tab.txt",       "DDD",          "HOBO",       NA_character_,  "%y.%m.%d %H:%M:%S",
))

data2 <- mc_read_data(files_table)