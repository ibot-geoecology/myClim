# Install

It is necessary to manually install dependencies. 
```R
requiered_packages <- c("stringr", "lubridate", "tibble", "dplyr", "purrr",
                        "ggplot2", "ggforce", "viridis", "runner",
                        "rmarkdown", "knitr", "kableExtra", "tidyr", "plotly", "zoo")
missing_packages <- requiered_packages[!(requiered_packages %in% installed.packages()[,"Package"])]
if(length(missing_packages)) install.packages(missing_packages)
# optional package rTubeDB
remotes::install_github('environmentalinformatics-marburg/tubedb/rTubeDB')
```

Installation of myClim package:
```R
install.packages("http://labgis.ibot.cas.cz/myclim/myClim_latest.tar.gz", repos=NULL, build_vignettes=TRUE)
```
# Documentation & user manual
* [functions documentation](http://labgis.ibot.cas.cz/myclim/reference/index.html)   
* [vignette: user manual](http://labgis.ibot.cas.cz/myclim/articles/myclim-demo.html)

# Source code of package
* [GitHub repository](https://github.com/ibot-geoecology/myClim)   
