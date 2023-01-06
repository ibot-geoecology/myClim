# Install

It is necessary to manually install dependencies. 
```R
requiered_packages <- c("stringr", "lubridate", "tibble", "dplyr", "purrr",
                        "ggplot2", "ggforce", "viridis", "runner",
                        "rmarkdown", "knitr", "kableExtra", "tidyr", "plotly", "zoo")
missing_packages <- requiered_packages[!(requiered_packages %in% installed.packages()[,"Package"])]
if(length(missing_packages)) install.packages(missing_packages)
```

Installation of myClim package:
```R
install.packages("http://labgis.ibot.cas.cz/myclim/myClim_latest.tar.gz", repos=NULL, build_vignettes=TRUE)
```
# Documentation & user manual
* [functions documentation](http://labgis.ibot.cas.cz/myclim/index.html)   
* [vignette: user manual](http://labgis.ibot.cas.cz/myclim/articles/myclim-demo.html)

