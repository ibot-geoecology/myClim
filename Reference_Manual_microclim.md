<!-- toc -->

září 30, 2021

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
    rlang,
    runner
Roxygen: list(markdown = TRUE)```


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

An object of class `list` of length 5.


## Usage

```r
mc_data_sensors
```


# `mc_DataFormat-class`

Class for source file data format


## Description

Class for source file data format


# `mc_eco_snow_agg`

Snow detection summary


## Description

Function return summary info about snow detection


## Usage

```r
mc_eco_snow_agg(snow_data, period = 3)
```


## Arguments

Argument      |Description
------------- |----------------
`snow_data`     |     data from function mc_eco_snow
`period`     |     count days for continuous cover of snow (default 3)


## Value

data.frame with columns serial_number, snow_days, first_day, last_day, first_day_period, last_day_period


## Examples

```r
data <- mc_eco_snow(example_tms_data1, "T3")
mc_eco_snow_agg(data)
```


# `mc_eco_snow`

Snow detection


## Description

Function detect snow based on detrended time series


## Usage

```r
mc_eco_snow(
  data,
  sensor,
  localities = c(),
  dr = 2,
  tmax = 0.5,
  interval_length = 15
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
`interval_length`     |     length of interval in minutes (default 15)


## Value

data.frame with datetime column and logical columns named by serial_number of loggers


## Examples

```r
mc_eco_snow(example_tms_data1, "T3")
```


# `mc_feed_directory`

Reading files from directory


## Description

This function read csv data files from directory of one logger type.
 If csv file is not in correct format, is skipped. Locality is set None.


## Usage

```r
mc_feed_directory(directory, logger_type, recursive = TRUE)
```


## Arguments

Argument      |Description
------------- |----------------
`directory`     |     character
`logger_type`     |     character - type of logger (TMS)
`recursive`     |     logical - recursive search in subdirectories


## Value

data in standard format


## Examples

```r
example_tms_data <- microclim::mc_feed_directory("examples/data/TMS/", "TMS")
```


# `mc_feed_files`

Reading files


## Description

This function read data files of one logger type. Locality is set None.


## Usage

```r
mc_feed_files(files, logger_type)
```


## Arguments

Argument      |Description
------------- |----------------
`files`     |     vector of character - files with data
`logger_type`     |     character - type of logger (TMS)


## Value

data in standard format


## Examples

```r
example_tms_data <- microclim::mc_feed_files(c("examples/data/TMS/data_91184101_0.csv", "examples/data/TMS/data_94184102_0.csv"), "TMS")
```


# `mc_feed_from_csv`

Data files reading by CSV


## Description

This function read raw data from loggers by table saved in CSV file


## Usage

```r
mc_feed_from_csv(csv_with_files_table)
```


## Arguments

Argument      |Description
------------- |----------------
`csv_with_files_table`     |     data.frame


## Value

data in standard format


## Examples

```r
example_tms_data <- microclim::mc_feed_from_csv("examples/data/TMS/files_table.csv")
```


# `mc_feed_from_df`

Data files reading


## Description

This function read raw data from loggers by data.frame with files description.
 Columns of data.frame:
  

*  path - path to file 

*  locality_id 

*  logger 

*  serial_number - can be NA, than try detect


## Usage

```r
mc_feed_from_df(files_table)
```


## Arguments

Argument      |Description
------------- |----------------
`files_table`     |     data.frame which describe data files


## Value

data in standard format


# `mc_feed_TMS_directory`

Reading TMS files from directory


## Description

This function read TMS data files from directory. Locality is set None.


## Usage

```r
mc_feed_TMS_directory(directory, recursive = TRUE)
```


## Arguments

Argument      |Description
------------- |----------------
`directory`     |     character
`recursive`     |     logical - recursive search in subdirectories


## Value

data in standard format


## Examples

```r
example_tms_data <- microclim::mc_feed_TMS_directory("examples/data/TMS/")
```


# `mc_feed_TMS_files`

Reading TMS files


## Description

This function read data files of TMS type. Locality is set None.


## Usage

```r
mc_feed_TMS_files(files)
```


## Arguments

Argument      |Description
------------- |----------------
`files`     |     vector of character - files with data


## Value

data in standard format


## Examples

```r
example_tms_data <- microclim::mc_feed_TMS_files(c("examples/data/TMS/data_91184101_0.csv", "examples/data/TMS/data_94184102_0.csv"))
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
mc_plot_image(data_table, filename, title)
```


## Arguments

Argument      |Description
------------- |----------------
`data_table`     |     data.frame in wideformat generated by mc_reshape_wideformat_interval
`filename`     |     output filename
`title`     |     of plot


## Examples

```r
data_table <- mc_reshape_wideformat_interval(example_tms_data1, sensors="T1")
mc_plot_image(data_table, "T1_image.png", "T1 sensor")
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
  crop = c(NA, NA),
  interval_length = 15
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
`interval_length`     |     length of interval in minutes (default 15)


## Examples

```r
mc_plot_loggers(example_tms_data1, "Figures")
```


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

data in standard format


## Examples

```r
example_tms_t1_table <- microclim::mc_reshape_longformat(example_tms_data, c("LOC_1", "LOC_2"), c("T1", "T2"))
```


# `mc_reshape_wideformat_interval`

Wideformat of sensor values by interval


## Description

This function create data.frame with values of sensor in wide format.
 Mean is computed from values in datetime interval.


## Usage

```r
mc_reshape_wideformat_interval(
  data,
  localities = c(),
  sensors = c(),
  interval_length = 15
)
```


## Arguments

Argument      |Description
------------- |----------------
`data`     |     all data in standard format
`localities`     |     names of localities; if empty then all
`sensors`     |     names of sensors; if empty then all
`interval_length`     |     in minutes (default 15)


## Value

data in standard format


## Examples

```r
example_tms_wideformat_interval <- microclim::mc_reshape_wideformat_interval(example_tms_data, c("LOC_1", "LOC_2"), c("T1", "T2"), 10)
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
example_tms_wideformat <- mc_reshape_wideformat(example_tms_data1, c("LOC_1", "LOC_2"), c("T1", "T2"))
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


# `mc_TMSDataFormat-class`

Class for source file data format for TMS logger


## Description

Class for source file data format for TMS logger


# `microclim-package`

microclim: What the Package Does (Title Case)


## Description

More about what it does (maybe more than one line)
 Use four spaces when indenting paragraphs within the Description.


