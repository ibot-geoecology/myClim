<!-- toc -->

září 16, 2021

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
    rlang
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


# `mc_DataFormat-class`

Class for source file data format


## Description

Class for source file data format


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


# `mc_reshape_wideformat`

Wideformat of sensor values


## Description

This function create data.frame with values of sensor


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
example_tms_t1_table <- microclim::mc_reshape_wideformat(example_tms_data, c("LOC_1", "LOC_2"), c("T1", "T2"))
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


