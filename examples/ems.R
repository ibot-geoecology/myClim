library(myClim)

files_table <- as.data.frame(tibble::tribble(
    ~path,                                 ~locality_id, ~data_format, ~tz_offset,
    "examples/data/EMS/00_2025_11_05.dcv",        "AAA",        "EMS",         60,
    "examples/data/EMS/01_2025_12_04.dcv",        "BBB",        "EMS",         60,
    "examples/data/EMS/18_2025_11_03.dcv",        "AAA",        "EMS",         60,
    "examples/data/EMS/B4_2025_12_15.dcv",        "AAA",        "EMS",         60,
))

data <- mc_read_data(files_table)

myClimGui::mcg_run(data)
