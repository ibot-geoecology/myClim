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
    * `metadata` - instance třídy `model.LocalityMetadata`
        * `id`
        * `altitude`
        * `lat_wgs84`
        * `lon_wgs84`
    * `loggers` - seznam loggerů v lokalitě bez jmen; logger je tvořen seznamem o třech položkách
        * `metadata` - instance třídy `model.LoggerMetadata`
        * `datetime` - vector data a času ve fromátu POSIXct
        * `sensors_data` - seznam senzorů patřících pod logger; položky jsou pojmenované; každý senzor je instance třídy `model.SensorData`
            * `sensor` - název senzoru 
            * `height` - výška senzoru 
            * `calibrated` - logický příznak, jestli je kalibrovaný 
            * `values` - vektor hodnot, který má stejnou délku jako `datetime` 

# API
## model

Soubor [R/model.R](R/model.R) obsahuje definice tříd.

### model.Sensor

Třida pro definici sensorů a jejich parametrů. Dávalo by mi smysl ji mít oddělenou od dat z čidel.

### model.LocalityMetadata

Třída pro uložení informací o lokalitě.

### model.LoggerMetadata

Třída pro uložení informací o loggeru.

### model.SensorData

Třída pro uložení informací o sensoru včetně dat.

### model.DataFormat

Třída pro dafinici formátu dat, které lezou z konkrétního loggeru.

### model.TMSDataFormat

Zděděná třída rozšiřující `model.DataFormat` kvůli detekci formátu data a sériového čísla.

## prepare

Soubor [R/prepare.R](R/prepare.R) obsahuje funkce pro parsování dat z loggerů.

### prepare.read\_directory(directory, logger_type, recursive=TRUE)

Funkce hledá csv soubory v zadaném adresáři a parsuje je jako data pro určitý logger. V případě, že jsou data ve špatném formátu,
tak soubor přeskočí a vypíše varování. Funkce vloží všechny loggery do lokality None, protože nemá informace o lokalitách.

### prepare.read\_TMS\_directory(directory, recursive=TRUE)

Funkce volá `prepare.read_directory` se specifikovaným TMS loggerem.

### prepare.read\_files(files, logger_type)

Funkce načítá zadané soubory a parsuje je jako data pro určitý logger. V případě, že jsou data ve špatném formátu,
tak soubor přeskočí. Funkce vloží všechny loggery do lokality None, protože nemá informace o lokalitách.

### prepare.read\_TMS\_files(files)

Funkce volá `prepare.read_files` se specifikovaným TMS loggerem.

### prepare.read\_files\_by\_table(files_table)

Funkce načítá soubory podle data.frame tabulky. Tabulka obsahuje následující sloupce:

* `path` - cesta k souboru
* `locality_id` - id lokality do který patří logger
* `logger` - typ loggeru
* `serial_number` - sériové číslo loggeru; Nemusí být vyplněno. Když je prázdné, tak se ho funkce snaží načíst z názvu souboru.

### prepare.read\_files\_by\_csv(csv_with_files_table)

Funkce volá `prepare.read_files_by_table` s tabulkou, kterou načte z csv souboru.

## read

Soubor [R/read.R](R/read.R) obsahuje funkce pro načítání informací z originálních dat.

### read.get\_sensor\_values\_from\_localities(data, sensor, localities)

Funkce vytvoří tabulku pro jeden `sensor`. Tabulka obsahuje sloupec `datetime` a pro každý logger z `localities` jeden sloupec
pojmenovaný `{lacality_id}-{logger_serial_number}` s hodnotami senzoru.

## data

Soubory rda z adresáře data. O vytvoření dat se starají R soubory z adresáře data-raw.

### data.source\_data\_formats

Seznam instancí třídy `model.DataFormat`, které definují jednotlivé vstupní formáty.
