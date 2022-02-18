# Instalace

Ruční instalace nenainstaluje automaticky závislosti. Můžete nainstalovat pomocí:

```R
requiered_packages <- c("stringr", "lubridate", "tibble", "dplyr", "purrr", "ggplot2", "ggforce", "viridis", "runner")
missing_packages <- requiered_packages[!(requiered_packages %in% installed.packages()[,"Package"])]
if(length(missing_packages)) install.packages(missing_packages)
```

Pomocí následujícího kódu je možné stáhnout a nainstalovat knihovnu včetně ukázkových příkladů a dat. Na začátku je potřeba nastavit adresář,
do kterého se rozbalí zdrojové soubory knihovny.

```R
# directory to unzip source files
setwd("C:/path/to/directory") # !EDIT IT!
zip_file <- "myClim.zip"
dir_name <- "myClim"
download.file("https://git.sorbus.ibot.cas.cz/api/v4/projects/microclimate_r%2Fmicroclim/repository/archive.zip?ref=HEAD&private_token=2fmZB-Qg-fbiVvzz2-Lh", destfile=zip_file, mode="wb")
subdir <- unzip(zip_file, list=TRUE)$Name[1]
unzip(zip_file)
file.remove(zip_file)
unlink(dir_name, recursive=TRUE)
file.rename(subdir, dir_name)
install.packages(dir_name, repos=NULL, type="source")
setwd(dir_name)
```

V RStudiu je možné aktualizovat balíček v menu Build -> Install and restart.

# Ukázka

V adresáři [examples](https://git.sorbus.ibot.cas.cz/microclimate_r/microclim/-/tree/main/examples)
se nachází soubory a skripty pro ukázku funkcionality.
V souboru [examples/load_tomst.R](examples/load_tomst.R) je ukázka načtení dat z TOMST čidel.
Stručný popis se nachází v komentářích.
V souboru [examples/plot_tomst.R](examples/plot_tomst.R) je ukázka generování grafů.
V souboru [examples/calc.R](examples/calc.R) je ukázka použití funkcí z modulu `mc_calc`.

Spuštění příkladů je možné následujícím kódem:

```R
source("examples/load_tomst.R")
source("examples/plot_tomst.R")
source("examples/calc.R")
```

# Testy

Program je pokryt unit testy, které je možné spustit příkazem:

```R
testthat::test_dir("tests")
```

# Dokumentace
[Dokumentace balíčku](Reference_Manual_myClim.md)
