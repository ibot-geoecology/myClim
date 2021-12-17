<!-- toc -->

prosince 17, 2021

# DESCRIPTION

```
Package: microclim
Type: Package
Title: What the Package Does (Title Case)
Version: 0.0.4
Author: Who wrote it
Maintainer: The package maintainer <yourself@somewhere.net>
Description: More about what it does (maybe more than one line)
    Use four spaces when indenting paragraphs within the Description.
License: What license is it under?
Encoding: UTF-8
LazyData: true
RoxygenNote: 7.1.1
Depends: 
    R (>= 3.0)
Imports:
    stringr,
    lubridate,
    tibble,
    dplyr,
    purrr,
    runner
Roxygen: list(markdown = TRUE)```


# `mc_calc_agg`

Agregate data by function


## Description

Function add agregated locality.


## Usage

```r
mc_calc_agg(
  data,
  fun,
  breaks,
  localities = NULL,
  sensors = NULL,
  use_utc = F,
  suffix = "_agg",
  ...
)
```


## Arguments

Argument      |Description
------------- |----------------
`data`     |     in format for calculation
`fun`     |     aggregation function
`breaks`     |     cut function parameter
`localities`     |     locality_ids for filtering data; if empty then all
`sensors`     |     sensor_ids for filtering data; if empty then all
`use_utc`     |     if set FALSE then datetime changed by locality tz_offset (default FALSE)
`suffix`     |     of new locality name
`...`     |     parameters for aggregation function


## Value

aggregated data in standard format


## Examples

```r
example_cleaned_tomst_data <- mc_calc_agg(example_cleaned_tomst_data, quantile, "hour", probs = 0.5, na.rm=TRUE)
```


# `mc_calc_snow_agg`

Snow detection summary


## Description

Function return summary info about snow detection


## Usage

```r
mc_calc_snow_agg(data, snow_sensor, localities = NULL, period = 3, use_utc = F)
```


## Arguments

Argument      |Description
------------- |----------------
`data`     |     in format for calculation
`snow_sensor`     |     name of snow sensor created by function mc_calc_snow
`localities`     |     names of localities; if empty then all
`period`     |     count days for continuous cover of snow (default 3)
`use_utc`     |     if set FALSE then datetime changed by locality tz_offset; default FALSE


## Details

If snow_sensor isn't in locality, then NA returned.


## Value

data.frame with columns locality, snow_days, first_day, last_day, first_day_period, last_day_period


## Examples

```r
snow_agg <- mc_calc_snow_agg(example_tomst_data1, "TMS_T3")
```


# `mc_calc_snow`

Snow detection


## Description

Function add sensor to locality with snow detection


## Usage

```r
mc_calc_snow(
  data,
  sensor,
  output_sensor = "snow",
  localities = NULL,
  dr = 2,
  tmax = 0.5
)
```


## Arguments

Argument      |Description
------------- |----------------
`data`     |     in format for calculation
`sensor`     |     name of temperature sensor
`output_sensor`     |     name of new snow sensor (default "snow")
`localities`     |     names for calculation; if empty then all
`dr`     |     delta range
`tmax`     |     maximal temperature


## Value

input data with added snow sensor


## Examples

```r
snow <- mc_calc_snow(example_tomst_data1, "TMS_T3")
```


# `mc_data_formats`

Formats of source data files


## Description

Formats of source data files


## Format

An object of class `list` of length 2.


## Usage

```r
mc_data_formats
```


# `mc_data_physical`

Physical quantities definition


## Description

Physical quantities definition


## Format

An object of class `list` of length 3.


## Usage

```r
mc_data_physical
```


# `mc_data_sensors`

Sensors definition


## Description

Sensors definition


## Format

An object of class `list` of length 6.


## Usage

```r
mc_data_sensors
```


# `mc_DataFormat-class`

Class for source file data format


## Description

Class for source file data format


# `mc_filter`

Filter data


## Description

This function filter data by localities and sensors


## Usage

```r
mc_filter(data, localities = NULL, sensors = NULL, reverse = FALSE)
```


## Arguments

Argument      |Description
------------- |----------------
`data`     |     in format for preparing or calculation
`localities`     |     locality_ids for filtering data; if NULL then do nothing
`sensors`     |     sensor_ids for filtering data; if NULL then do nothing
`reverse`     |      

*  if TRUE then filtered discard else keeped (default FALSE)


## Value

filtered data in same format as input


## Examples

```r
example_tomst_data1 <- mc_filter(example_tomst_data1, localities=c("A6W79", "A2E32"), sensors=c("TMS_T1", "TMS_T2"))
```


# `mc_info_clean`

Get all clean table


## Description

This function return dataframe with all clean info about loggers


## Usage

```r
mc_info_clean(data)
```


## Arguments

Argument      |Description
------------- |----------------
`data`     |     in format for preparing


## Value

dataframe with columns locality_id, serial_number, start_date, end_date, step, count_duplicits, count_missed, count_disordered


## Examples

```r
log_table <- mc_prep_logs(cleaned_example_tomst_data1)
```


# `mc_info_count`

count data


## Description

This function return count of localities, loggers and sensors


## Usage

```r
mc_info_count(data)
```


## Arguments

Argument      |Description
------------- |----------------
`data`     |     in format for preparing or calculation


## Value

data.frame with count localities, loggers and sensors


## Examples

```r
count_table <- mc_info_count(example_tomst_data1)
```


# `mc_LocalityMetadata-class`

Class for locality metadata


## Description

Class for locality metadata


# `mc_LoggerCleanInfo-class`

Class for logger clean info


## Description

Class for logger clean info


# `mc_LoggerMetadata-class`

Class for logger metadata


## Description

Class for logger metadata


# `mc_Physical-class`

Class for physical


## Description

Class for physical


# `mc_plot_image`

Plot data - image


## Description

Function plot data to file with image function


## Usage

```r
mc_plot_image(
  data,
  filename,
  title,
  localities = NULL,
  sensors = NULL,
  height = 1900,
  left_margin = 12
)
```


## Arguments

Argument      |Description
------------- |----------------
`data`     |     all data in standard format
`filename`     |     output filename
`title`     |     of plot
`localities`     |     names of localities; if empty then all
`sensors`     |     names of sensors; if empty then all
`height`     |     of image; default = 1900
`left_margin`     |     width of space for sensor_labels; default = 12


## Examples

```r
mc_plot_image(data, "T1_image.png", "T1 sensor", sensors="TMS_T1")
```


# `mc_plot_loggers`

Plot data from loggers


## Description

Function plot loggers to directory


## Usage

```r
mc_plot_loggers(
  data,
  directory,
  localities = c(),
  sensors = c(),
  crop = c(NA, NA)
)
```


## Arguments

Argument      |Description
------------- |----------------
`data`     |     all data in standard format
`directory`     |     output directory
`localities`     |     names of localities; if empty then all
`sensors`     |     names of sensors; if empty then all
`crop`     |     datetime range for plot, not cropping if NA (default c(NA, NA))


## Examples

```r
mc_plot_loggers(example_tomst_data1, "Figures")
```


# `mc_prep_clean`

Cleaning datetime series


## Description

This function change datetime and values series. Result series has constant
 step without duplicits and missed values are filled in as NA.


## Usage

```r
mc_prep_clean(data, silent = FALSE)
```


## Arguments

Argument      |Description
------------- |----------------
`data`     |     character data in standard format
`silent`     |     if true, then informations aren't printed (default FALSE)


## Value

cleaned data in standard format


## Examples

```r
cleaned_example_tomst_data1 <- mc_prep_clean(example_tomst_data1)
```


# `mc_prep_crop`

Crop datetime


## Description

This function crop data by datetime


## Usage

```r
mc_prep_crop(data, start = NULL, end = NULL)
```


## Arguments

Argument      |Description
------------- |----------------
`data`     |     in standard format
`start`     |     POSIXct datetime in UTC; is optional
`end`     |     POSIXct datetime in UTC; is optional


## Value

cropped data in standard format


## Examples

```r
cleaned_example_tomst_data1 <- mc_prep_crop(example_tomst_data1, end=as.POSIXct("2020-02-01"))
```


# `mc_prep_flat`

Flattening data


## Description

This function flatten data. Logger lever from data hierarchy is deleted.
 Sensors are moved to locality and datetimes are merged to one series.


## Usage

```r
mc_prep_flat(data)
```


## Arguments

Argument      |Description
------------- |----------------
`data`     |     in format for preparing


## Value

flattened data in format for calculation


## Examples

```r
calc_data <- mc_prep_flat(example_cleaned_tomst_data1)
```


# `mc_prep_rename_sensor`

Rename sensor


## Description

This function rename sensors. It is usefull for flatting data format.


## Usage

```r
mc_prep_rename_sensor(
  data,
  sensor_names,
  localities = NULL,
  serial_numbers = NULL
)
```


## Arguments

Argument      |Description
------------- |----------------
`data`     |     in format for preparing or calculation
`sensor_names`     |     list with new names of sensors; names of items are old ones
`localities`     |     vector of locality_ids; if NULL than all (default NULL)
`serial_numbers`     |     vector of serial_numbers; if NULL than all (default NULL); parameter is usefull only for preparing format of data


## Value

data with changed sensor names


## Examples

```r

```


# `mc_prep_solar_tz`

Solar TZ offset


## Description

This function compute TZ offset in localities by solar time


## Usage

```r
mc_prep_solar_tz(data)
```


## Arguments

Argument      |Description
------------- |----------------
`data`     |     in standard format


## Value

data with changed TZ offset in standard format


## Examples

```r
cleaned_example_tomst_data1 <- mc_prep_solar_tz(cleaned_example_tomst_data1)
```


# `mc_prep_user_tz`

Set user defined TZ offset


## Description

This function set user defined TZ offsets in localities


## Usage

```r
mc_prep_user_tz(data, tz_offsets)
```


## Arguments

Argument      |Description
------------- |----------------
`data`     |     in standard format
`tz_offsets`     |     named list (name: locality_id, item: tz_offset in rounded minutes)


## Value

data with changed TZ offset in standard format


## Examples

```r
example_tomst_data2 <- mc_prep_solar_tz(example_tomst_data2, list(`91184101`=60))
```


# `mc_read_csv`

Data files reading by CSV


## Description

This function read raw data from loggers by table saved in CSV file


## Usage

```r
mc_read_csv(csv_files_table, csv_localities_table = NULL)
```


## Arguments

Argument      |Description
------------- |----------------
`csv_files_table`     |     data.frame
`csv_localities_table`     |     data.frame


## Value

data in standard format


## Examples

```r
example_tomst_data <- microclim::mc_read_csv("examples/data/TOMST/files_table.csv")
```


# `mc_read_dataframe`

Data files reading


## Description

This function read raw data from loggers by data.frame with files description.


## Usage

```r
mc_read_dataframe(files_table, localities_table = NULL)
```


## Arguments

Argument      |Description
------------- |----------------
`files_table`     |     data.frame which describe data files Columns:  

*  path - path to file 

*  locality_id 

*  data_format 

*  serial_number - can be NA, than try detect
`localities_table`     |     data.frame which describe localities Columns:  

*  locality_id 

*  altitude 

*  lon_wgs84 

*  lat_wgs84 

*  tz_offset


## Value

data in standard format


# `mc_read_directory`

Reading files from directory


## Description

This function read csv data files from directory of one logger type.
 If csv file is not in correct format, is skipped. Locality is set to serial_number of logger.


## Usage

```r
mc_read_directory(directory, dataformat_name, recursive = TRUE)
```


## Arguments

Argument      |Description
------------- |----------------
`directory`     |     character
`dataformat_name`     |     character - data format of logger (TOMST)
`recursive`     |     logical - recursive search in subdirectories


## Value

data in standard format


## Examples

```r
example_tomst_data <- microclim::mc_read_directory("examples/data/TOMST/", "TOMST")
```


# `mc_read_files`

Reading files


## Description

This function read data files of one logger type. Locality is set to serial_number of logger.


## Usage

```r
mc_read_files(files, dataformat_name)
```


## Arguments

Argument      |Description
------------- |----------------
`files`     |     vector of character - files with data
`dataformat_name`     |     character - data format of logger (TOMST)


## Value

data in standard format


## Examples

```r
example_tomst_data <- microclim::mc_read_files(c("examples/data/TOMST/data_91184101_0.csv", "examples/data/TOMST/data_94184102_0.csv"), "TOMST")
```


# `mc_reshape_long`

Longformat of sensor values


## Description

This function create data.frame with values of sensor


## Usage

```r
mc_reshape_long(data, localities = NULL, sensors = NULL)
```


## Arguments

Argument      |Description
------------- |----------------
`data`     |     in format for preparing or calculation
`localities`     |     locality_ids; if NULL then all (default NULL)
`sensors`     |     names of sensors; if NULL then all (default NULL)


## Value

data.frame with columns locality_id, serial_number, sensor, datetime, value


## Examples

```r
example_tms_t1_table <- microclim::mc_reshape_long(example_tomst_data, c("A6W79", "A2E32"), c("TMS_T1", "TMS_T2"))
```


# `mc_reshape_wide`

Wideformat of sensor values


## Description

This function create data.frame with values of sensor in wide format.


## Usage

```r
mc_reshape_wide(data, localities = NULL, sensors = NULL)
```


## Arguments

Argument      |Description
------------- |----------------
`data`     |     in format for preparing or calculation
`localities`     |     names of localities; if NULL then all (default NULL)
`sensors`     |     names of sensors; if NULL then all (default NULL)


## Value

data.frame with datetime column and columns for every sensor


## Examples

```r
example_tms_wideformat <- mc_reshape_wide(example_tomst_data1, c("A6W79", "A2E32"), c("TMS_T1", "TMS_T2"))
```


# `mc_Sensor-class`

Class for sensor definition


## Description

Class for sensor definition


# `mc_SensorMetadata-class`

Class for sensor metadata


## Description

Class for sensor metadata


# `mc_SensorState-class`

Class for state of sensor


## Description

Class for state of sensor


# `mc_TOMSTDataFormat-class`

Class for source file data format for TOMST logger


## Description

Class for source file data format for TOMST logger


# `microclim-package`

microclim: What the Package Does (Title Case)


## Description

More about what it does (maybe more than one line)
 Use four spaces when indenting paragraphs within the Description.


