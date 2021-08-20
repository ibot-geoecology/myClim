# Instalace

## Přímá instalce z GitLab

V Linuxu je možné instalovat příkazem `install_gitlab` z balíčku `devtools`. Ve Windows se nechová správně. 

```R
devtools::install_gitlab("microclimate_r/microclim", host="git.sorbus.ibot.cas.cz", auth_token="2fmZB-Qg-fbiVvzz2-Lh")
```

Ve Windows lze instalovat následujícím kódem. 

```R
destfile <- tempfile(pattern = "microclim", tmpdir = tempdir(), fileext = ".zip")
download.file("https://git.sorbus.ibot.cas.cz/api/v4/projects/microclimate_r%2Fmicroclim/repository/archive.zip?ref=HEAD&private_token=2fmZB-Qg-fbiVvzz2-Lh", destfile=destfile)
install.packages(destfile, repos=NULL, type="source")
file.remove(destfile)
```

## Lokální instalace

Celý git repozitář je možné klonovat následujícími příkazy. V případě, že se chcete vyhnout příkazové řadce a git,
tak je možné celý obsah komprimovaný jako zip stáhnout přes tlačítko:

![Download](images/download_button.png)


```sh
git clone https://git.sorbus.ibot.cas.cz/microclimate_r/microclim.git
```

nebo

```sh
git clone git@git.sorbus.ibot.cas.cz:microclimate_r/microclim.git
```

Instalaci balíčku je možné provést následujícím příkazem
za předpokladu, že pracovní adresář je nastavený na adresář, ve kterém se nachází README.md.

```R

install.packages(".", repos = NULL, type="source")

```

Případně v RStudiu je možné instalovat v menu Build -> Install and restart.

# Formát načtených dat

Zde je struktura

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

Soubor [R/model.R](R/model.R). Obsahuje definice tříd.

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

Soubor [R/prepare.R](R/prepare.R). Obsahuje funkce pro parsování dat z loggerů.

### prepare.read\_files\_by\_csv

Funkce pro načtení dat z více souborů. Funkci se předá csv soubor s tabulkou s popisem souborů.

Je možné otestovat následujícím způsobem:

```R
library(microclim)
localities_data <- microclim::prepare.read_files_by_csv("tests/data/files_table.csv")
```

### prepare.read\_directory

Funkce hledá csv soubory v zadaném adresáři a parsuje je jako data pro určitý logger. V případě, že jsou data ve špatném formátu,
tak soubor přeskočí. Funkce vloží všechny loggery do lokality None, protože nemá informace o lokalitách.

### prepare.read\_TMS\_directory

Funkce volá `prepare.read_directory` se specifikovaným TMS loggerem.

### prepare.read\_files

Funkce načítá zadané soubory a parsuje je jako data pro určitý logger. V případě, že jsou data ve špatném formátu,
tak soubor přeskočí. Funkce vloží všechny loggery do lokality None, protože nemá informace o lokalitách.

### prepare.read\_TMS\_files

Funkce volá `prepare.read_files` se specifikovaným TMS loggerem.

### prepare.read\_files\_by\_table

Funkce načítá soubory podle data.frame tabulky. Tabulka obsahuje následující sloupce:

* `path` - cesta k souboru
* `locality_id` - id lokality do který patří logger
* `logger` - typ loggeru
* `serial_number` - sériové číslo loggeru; Nemusí být vyplněno. Když je prázdné, tak se ho funkce snaží načíst z názvu souboru.

### prepare.read\_files\_by\_csv

Funkce volá `prepare.read_files_by_table` s tabulkou, kterou načte z csv souboru.

## data

Soubory rda z adresáře data. O vytvoření dat se starají R soubory z adresáře data-raw.

### data.source\_data\_formats

Seznam instancí třídy `model.DataFormat`, které definují jednotlivé vstupní formáty.

# Examples

V adresáři [examples](https://git.sorbus.ibot.cas.cz/microclimate_r/microclim/-/tree/main/examples)
jsou příklady volání funkcí i s daty. V případě naklonování git repozitáře je možné spustit přímo. 


```R
source("examples/load_tms.R")
```
