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
