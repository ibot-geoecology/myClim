path_prefix <- "../"

files_sources <- list.files(stringr::str_glue("{path_prefix}R"), full.names = T)
purrr::walk(files_sources, source)
files_data <- list.files(stringr::str_glue("{path_prefix}data"), full.names = T)
for(data_file in files_data){
  load(data_file)
}
