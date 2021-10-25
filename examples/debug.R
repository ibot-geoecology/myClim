files.sources = list.files("../R", full.names = T)
purrr::walk(files.sources, source)
