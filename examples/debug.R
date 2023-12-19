detach("package:myClim", unload=TRUE)
setwd("tests/testthat")
devtools::load_all()

# debug code
lt <- read.table("localities_table.csv", sep=",", header = T)

tms.m <- mc_read_data(files_table = "files_table.csv",
                      localities_table = lt,
                      silent = T)
