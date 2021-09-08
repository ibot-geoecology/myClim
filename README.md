# Instalace

Pomocí následujícího kódu je možné stáhnout a nainstalovat knihovnu včetně ukázkových příkladů a dat. Na začátku je potřeba nastavit adresář,
do kterého se rozbalí zdrojové soubory knihovny.

```R
# directory to unzip source files
setwd("C:/path/to/directory") # !EDIT IT!
zip_file <- "microclim.zip"
download.file("https://git.sorbus.ibot.cas.cz/api/v4/projects/microclimate_r%2Fmicroclim/repository/archive.zip?ref=HEAD&private_token=2fmZB-Qg-fbiVvzz2-Lh", destfile=zip_file, mode="wb")
subdir <- unzip(zip_file, list=TRUE)$Name[1]
unzip(zip_file)
file.remove(zip_file)
for(item in list.files(subdir, all.files = FALSE, include.dirs = TRUE)){
    file.copy(file.path(subdir, item), "./", recursive=TRUE)
}
unlink(subdir, recursive=TRUE)
install.packages(".", repos=NULL, type="source")
```

V RStudiu je možné aktualizovat balíček v menu Build -> Install and restart.

# Ukázka

V adresáři [examples](https://git.sorbus.ibot.cas.cz/microclimate_r/microclim/-/tree/main/examples)
se nachází soubory a skripty pro ukázku funkcionality.
V souboru [examples/load_tms.R](examples/load_tms.R) je ukázka načtení dat z TMS čidel.
Stručný popis se nachází v komentářích.

Spuštění příkladu je možné následujícím kódem:

```R
library(microclim)
source("examples/load_tms.R")
```

# Testy

Program je pokryt unit testy, které je možné spustit příkazem:

```R
testthat::test_dir("tests")
```

# Formát načtených dat

Zde je jednotná struktura, ve kterých si balíček drží načtená data.

* seznam lokalit - jméno položky je id lokality; každá lokalita je seznam o dvou položkách
    * `metadata` - instance třídy `mc_LocalityMetadata`
        * `id`
        * `altitude`
        * `lat_wgs84`
        * `lon_wgs84`
    * `loggers` - seznam loggerů v lokalitě bez jmen; logger je tvořen seznamem o třech položkách
        * `metadata` - instance třídy `mc_LoggerMetadata`
        * `datetime` - vector data a času ve fromátu POSIXct
        * `sensors` - seznam senzorů patřících pod logger; položky jsou pojmenované; každý senzor je seznam o třech položkách`
            * `metadata` - instance třídy `mc_SensorMetadata`
                * `sensor` - název senzoru 
                * `height` - výška senzoru 
                * `calibrated` - logický příznak, jestli je kalibrovaný 
            * `states` - instance třídy `mc_SensorState`
                * `tag` - příznak 
                * `start` - začátek stavu 
                * `end` - konec stavu
            * `values` - vektor hodnot, který má stejnou délku jako `datetime` 

# Dokumentace
[Dokumentace balíčku](Reference_Manual_microclim.md)
