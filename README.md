# Install

While installing from GitLab it is necessary to manually install dependencies first. 
```R
requiered_packages <- c("stringr", "lubridate", "tibble", "dplyr", "purrr", "ggplot2", "ggforce", "viridis", "runner", "rmarkdown", "knitr")
missing_packages <- requiered_packages[!(requiered_packages %in% installed.packages()[,"Package"])]
if(length(missing_packages)) install.packages(missing_packages)
```

Following script will download myClim installation files and install the myClim library.  
It is necessary to **set the directory where to unzip installation files**. Is important only for installation. After installation the files are not needed any more. During installation also vignettes will be built. Depending on your environment may need additional libraries to build vignettes. Read warnings carefully.  

```R
# directory to unzip source files
setwd("C:/path/to/directory") # !!!! EDIT HERE !!!!
zip_file <- "myClim.zip"
dir_name <- "myClim"
download.file("https://git.sorbus.ibot.cas.cz/api/v4/projects/microclimate_r%2Fmicroclim/repository/archive.zip?ref=HEAD&private_token=2fmZB-Qg-fbiVvzz2-Lh", destfile=zip_file, mode="wb")
subdir <- unzip(zip_file, list=TRUE)$Name[1]
unzip(zip_file)
file.remove(zip_file)
unlink(dir_name, recursive=TRUE)
file.rename(subdir, dir_name)
setwd(dir_name)
pkg_file <- devtools::build(".")
install.packages(pkg_file, repos = NULL)
```
# Documentation & user manual
Functions documentation is available in Reference section [online here](http://tubedb.ibot.cas.cz:8000/index.html).   
In Articles section there is also vignette = user manual with running examples. 

# Examples
In [examples](https://git.sorbus.ibot.cas.cz/microclimate_r/microclim/-/tree/main/examples) folder there are example script with example data e.g. 

* [examples/load_tomst.R](examples/load_tomst.R) is the tutorial how to read TMS logger data. 
* [examples/plot_tomst.R](examples/plot_tomst.R) plotting tutorial
* [examples/calc.R](examples/calc.R) example of using `mc_calc` functions
* [examples/load_chmi.R](examples/load_chmi.R) example of reading data from wide data.frame

