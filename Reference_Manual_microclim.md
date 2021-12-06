<!-- toc -->

prosince 06, 2021

# DESCRIPTION

```
Package: microclim
Type: Package
Title: What the Package Does (Title Case)
Version: 0.0.1
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
    runner
Roxygen: list(markdown = TRUE)```


# `mc_calc_agg_mean`

Agregate data by mean function


## Description

Function return aggregated data by mean


## Usage

```r
mc_calc_agg_mean(
  data,
  breaks,
  localities = NULL,
  sensors = NULL,
  use_utc = F,
  ...
)
```


## Arguments

Argument      |Description
------------- |----------------
`data`     |     all data in standard format
`breaks`     |     cut function parameter
`localities`     |     locality_ids for filtering data; if empty then all
`sensors`     |     sensor_ids for filtering data; if empty then all
`use_utc`     |     if set FALSE then datetime changed by locality tz_offset; default FALSE
`...`     |     parameters for mean function


## Value

aggregated data in standard format


## Examples

```r
example_cleaned_tomst_data <- mc_calc_agg_mean(example_cleaned_tomst_data, "hour", na.rm=TRUE)
```


# `mc_calc_agg_quantile`

Agregate data by quantile function


## Description

Function return aggregated data by quantile function


## Usage

```r
mc_calc_agg_quantile(
  data,
  breaks,
  probs,
  localities = NULL,
  sensors = NULL,
  use_utc = F,
  ...
)
```


## Arguments

Argument      |Description
------------- |----------------
`data`     |     all data in standard format
`breaks`     |     cut function parameter
`probs`     |     value 0-1
`localities`     |     locality_ids for filtering data; if empty then all
`sensors`     |     sensor_ids for filtering data; if empty then all
`use_utc`     |     if set FALSE then datetime changed by locality tz_offset; default FALSE
`...`     |     parameters for quantile function


## Value

aggregated data in standard format


## Examples

```r
example_cleaned_tomst_data <- mc_calc_agg_quantile(example_cleaned_tomst_data, "hour", 0.1, na.rm=TRUE)
```


# `mc_calc_agg`

Agregate data by function


## Description

Function return aggregated data by function


## Usage

```r
mc_calc_agg(
  data,
  fun,
  breaks,
  localities = NULL,
  sensors = NULL,
  use_utc = F,
  ...
)
```


## Arguments

Argument      |Description
------------- |----------------
`data`     |     all data in standard format
`fun`     |     aggregation function
`breaks`     |     cut function parameter
`localities`     |     locality_ids for filtering data; if empty then all
`sensors`     |     sensor_ids for filtering data; if empty then all
`use_utc`     |     if set FALSE then datetime changed by locality tz_offset; default FALSE
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
mc_calc_snow_agg(
  data,
  sensor,
  localities = c(),
  dr = 2,
  tmax = 0.5,
  period = 3
)
```


## Arguments

Argument      |Description
------------- |----------------
`data`     |     all data in standard format
`sensor`     |     name of temperature sensor
`localities`     |     names of localities; if empty then all
`dr`     |     delta range
`tmax`     |     maximal temperature
`period`     |     count days for continuous cover of snow (default 3)


## Value

data.frame with columns serial_number, snow_days, first_day, last_day, first_day_period, last_day_period


## Examples

```r
snow_agg <- mc_calc_snow_agg(example_tomst_data1, "TMS_T3")
```


# `mc_calc_snow`

Snow detection


## Description

Function detect snow based on detrended time series


## Usage

```r
mc_calc_snow(data, sensor, localities = c(), dr = 2, tmax = 0.5)
```


## Arguments

Argument      |Description
------------- |----------------
`data`     |     all data in standard format
`sensor`     |     name of temperature sensor
`localities`     |     names of localities; if empty then all
`dr`     |     delta range
`tmax`     |     maximal temperature


## Value

data.frame with datetime column and logical columns named by serial_number of loggers


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
mc_filter(data, localities = NULL, sensors = NULL)
```


## Arguments

Argument      |Description
------------- |----------------
`data`     |     in standard format
`localities`     |     locality_ids for filtering data; if empty then all
`sensors`     |     sensor_ids for filtering data; if empty then all


## Value

filtered data in standard format


## Examples

```r
example_tomst_data1 <- mc_filter(example_tomst_data1, localities=c("A6W79", "A2E32"), sensors=c("TMS_T1", "TMS_T2"))
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
`data`     |     in standard format


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


# `mc_prep_datetime_step`

Cleaning datetime series


## Description

This function change datetime and values series. Result series has constant
 step without duplicits and missed values are filled in as NA.


## Usage

```r
mc_prep_datetime_step(data)
```


## Arguments

Argument      |Description
------------- |----------------
`data`     |     character data in standard format


## Value

cleaned data in standard format


## Examples

```r
cleaned_example_tomst_data1 <- mc_prep_datetime_step(example_tomst_data1)
```


# `mc_prep_logs`

Get all clean log messages


## Description

This function return dataframe with all clean log messages


## Usage

```r
mc_prep_logs(data)
```


## Arguments

Argument      |Description
------------- |----------------
`data`     |     in standard format


## Value

dataframe with columns locality_id, serial_number, clean_type, message


## Examples

```r
log_table <- mc_prep_logs(cleaned_example_tomst_data1)
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


# `mc_read_from_csv`

Data files reading by CSV


## Description

This function read raw data from loggers by table saved in CSV file


## Usage

```r
mc_read_from_csv(csv_files_table, csv_localities_table = NULL)
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
example_tomst_data <- microclim::mc_read_from_csv("examples/data/TOMST/files_table.csv")
```


# `mc_read_from_df`

Data files reading


## Description

This function read raw data from loggers by data.frame with files description.


## Usage

```r
mc_read_from_df(files_table, localities_table = NULL)
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


# `mc_reshape_longformat`

Longformat of sensor values


## Description

This function create data.frame with values of sensor


## Usage

```r
mc_reshape_longformat(data, localities = c(), sensors = c())
```


## Arguments

Argument      |Description
------------- |----------------
`data`     |     all data in standard format
`localities`     |     names of localities; if empty then all
`sensors`     |     names of sensors; if empty then all


## Value

data.frame with columns location, serial_number, sensor, datetime, value


## Examples

```r
example_tms_t1_table <- microclim::mc_reshape_longformat(example_tomst_data, c("A6W79", "A2E32"), c("TMS_T1", "TMS_T2"))
```


# `mc_reshape_wideformat`

Wideformat of sensor values


## Description

This function create data.frame with values of sensor in wide format.


## Usage

```r
mc_reshape_wideformat(data, localities = c(), sensors = c())
```


## Arguments

Argument      |Description
------------- |----------------
`data`     |     all data in standard format
`localities`     |     names of localities; if empty then all
`sensors`     |     names of sensors; if empty then all


## Value

data in standard format


## Examples

```r
example_tms_wideformat <- mc_reshape_wideformat(example_tomst_data1, c("A6W79", "A2E32"), c("TMS_T1", "TMS_T2"))
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


