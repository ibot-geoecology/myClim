<!-- toc -->

února 21, 2022

# DESCRIPTION

```
Package: myClim
Type: Package
Title: R package for processing microclimatic data
Version: 0.0.21
Author: GISlab
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
    ggplot2,
    ggforce,
    viridis,
    runner
Roxygen: list(markdown = TRUE)```


# `mc_agg`

Agregate data by function


## Description

Function has two basic uses:
  

*  aggregate time step of microclimatic records from fine to coarser with specified function (e. g. daily mean from 15 min records); 

*  convert myClim object from preparation to calculation format without records modification, this behaviour appears wen fun=NULL, period=NULL. Any output of mc_agg is in calculation format. See Details.


## Usage

```r
mc_agg(
  data,
  fun = NULL,
  period = NULL,
  use_utc = TRUE,
  percentiles = NULL,
  na.rm = TRUE
)
```


## Arguments

Argument      |Description
------------- |----------------
`data`     |     cleaned myClim object: output of [`mc_prep_clean()`](#mcprepclean())
`fun`     |     aggregation function; one of ("min", "max", "mean", "percentile", "sum", "count", "coverage") See details. Can be single function name, character vector of function names or list of function names. if NULL records are not aggregated, but converted to calculation format. See details.
`period`     |     Time period for aggregation - same as breaks in cut.POSIXt, e.g. ("hour", "day", "month"); if NULL then no aggregation  There is special period "all" returning single value for each sensor based on function applied across all records within the sensor.  Start day of week is Monday.
`use_utc`     |     default TRUE, if set FALSE forced to use UTC time,instead possibly available time offset (in locality metadata: tz_offset) local or solar time see (e.g. `mc_prep_solar_tz` , `mc_prep_user_tz` ); Non-UTC time can by used only for period `day` and longer.
`percentiles`     |     vector of percentile numbers; numbers are from range 0-100; every number generate new sensor, see details
`na.rm`     |     parameter for aggregation function; Not used for count and coverage.


## Details

Function returns new myClim object.
 
 Any output of mc_agg is in computation format. That means the structure of myClim object is flattened. Hierarchical level of logger is removed (Locality<-Logger<-Sensor<-Record), and all microclimatic records within the sensors are joined directly to the level of locality (Locality<-Sensor<-Record). This is called computation format and is only acceptable format for `mc_calc` functions family.
 
 In case `mc_agg` is used only for conversion from preparation to computation format (fun=NULL, period=NULL) then microclimatic records are not modified.
 
 When fun and period is specified, microclimatic records are aggregated based on function into new period. Aggregated time step is marked by a first element of selected period i.e. day = c(2022-12-29 00:00, 2022-12-30 00:00...); week = c(2022-12-19 00:00, 2022-12-28 00:00...); month = c(2022-11-01 00:00, 2022-12-01 00:00...); year = c(2021-01-01 00:00, 2022-01-01 00:00...). When first or last period is incomplete in original data, the incomplete part is deleted, and a warning is shown (e.g. when original data starting on 2021-11-28 00:00 and period = ”month” then incomplete November is deleted and aggregation starts in December).
 
 Empty sensors with no records are excluded. Aggregation functions return NA for empty vector except from count which returns 0. Aggregation functions are applied to all sensors in provided myClim object. Aggregation function creates new sensors on localities with used aggregation function in its name (sensor_name)_(function) e.g. (TMS_T1_max), after aggregation sensors keep original sensor_id in sensor metadata (e.g. TMS_T1).
  

*  sensors created with functions `min` , `max` , `mean` , `percentile` , `sum` keeps identical sensor_id and value_type as original input sensors 

*  sensors created with functions `count` has sensor_id `count` and value_type `integer` , function `coverage` has sensor_id `coverage` and value_type `real`  

*  coverage returns the ratio of non NA records/all records


## Value

Returns new myClim object in calculation format ready for mc_calc functions family. When fun=NULL, period=NULL records are not modified but only converted to calc format. When fun and period provided then time step is aggregated based on function.


## Examples

```r
example_cleaned_tomst_data <- mc_agg(example_cleaned_tomst_data, c("min", "max", "percentile"), "hour", percentiles = 50, na.rm=TRUE)
```


# `mc_calc_fdd`

Freezing Degree Days


## Description

Function add new virtual sensor with values of FDD Freezing Degree Days.


## Usage

```r
mc_calc_fdd(data, sensor, output_prefix = "FDD", t_base = 0, localities = NULL)
```


## Arguments

Argument      |Description
------------- |----------------
`data`     |     in format for calculation
`sensor`     |     name of temperature sensor
`output_prefix`     |     name prefix of new FDD sensor (default "FDD")  name of output sensor consists of output_prefix and value t_base
`t_base`     |     threshold temperaturefor calculation FDD (default 0)
`localities`     |     list of locality_ids for calculation; if NULL then all (default NULL)


## Details

Maximal step length of data is day.


## Value

input data with added FDD sensor


# `mc_calc_gdd`

Growing Degree Days


## Description

Function add new virtual sensor with values of GDD Growing Degree Days.


## Usage

```r
mc_calc_gdd(data, sensor, output_prefix = "GDD", t_base = 5, localities = NULL)
```


## Arguments

Argument      |Description
------------- |----------------
`data`     |     in format for calculation
`sensor`     |     name of temperature sensor
`output_prefix`     |     name prefix of new GDD sensor (default "GDD")  name of output sensor consists of output_prefix and value t_base
`t_base`     |     threshold temperaturefor calculation GDD (default 5)
`localities`     |     list of locality_ids for calculation; if NULL then all (default NULL)


## Details

Maximal step length of data is day.


## Value

input data with added GDD sensor


# `mc_calc_snow_agg`

Summary of TRUE/FALSE snow sensor


## Description

Accept only myClim objects in calculation format. See [`mc_agg()`](#mcagg()) . `mc_calc_snow_agg` was primary designed to work with the virtual snow sensor of TRUE/FALSE which is the output of [`mc_calc_snow()`](#mccalcsnow()) . `mc_calc_snow_agg` returns the summary table of snow sensor (e.g number of days with snow cover, the longest continual snow period...). The snow summary is returned for whole date range provided.


## Usage

```r
mc_calc_snow_agg(
  data,
  snow_sensor = "snow",
  localities = NULL,
  period = 3,
  use_utc = F
)
```


## Arguments

Argument      |Description
------------- |----------------
`data`     |     myClim object in calculation format (see [`mc_agg()`](#mcagg()) ) with TRUE/FALSE snow sensor see [`mc_calc_snow()`](#mccalcsnow())
`snow_sensor`     |     name of snow sensor containing TRUE/FALS snow detection, suitable for virtual sensors created by function `mc_calc_snow` ; (default "snow")
`localities`     |     optional subset of localities where to run the function (list of locality_ids); if NULL then return all localities (default NULL)
`period`     |     number of days defining the continuous snow cover period of interest (default 3 days)
`use_utc`     |     if set FALSE then time is corrected based on offset provided in locality metadata `tz_offset` , see e. g. [`mc_prep_solar_tz()`](#mcprepsolartz()) , [`mc_prep_user_tz()`](#mcprepusertz()) ; (default FALSE)


## Details

Primary designed for virtual snow sensor created by [`mc_calc_snow()`](#mccalcsnow()) , but accepts any sensor with TRUE/FLAST snow event detection. If `snow_sensor` on the locality missing, then locality is skipped.


## Value

Returns data.frame with columns:
  

*  locality - locality name 

*  snow_days - number of days with snow cover 

*  first_day - first day with snow 

*  last_day - last day with snow 

*  first_day_period - first day of period with continual snow cover based on `period` parameter 

*  last_day_period - last day of period with continual snow cover based on `period` parameter


## Examples

```r
snow_agg <- mc_calc_snow_agg(example_tomst_data1, "TMS_T2_snow")
```


# `mc_calc_snow`

Snow detection from temperature


## Description

Function accept only myClim objects in calculation format. See [`mc_agg()`](#mcagg()) . Function `mc_calc_snow` creates new virtual sensor on locality within myClim data object. Function return TRUE/FALSE vector in original time step for Snow/non-snow  events.


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
`data`     |     myClim object in calculation format. See [`mc_agg()`](#mcagg())
`sensor`     |     name of temperature sensor used for snow estimation. (e.g. TMS_T2)
`output_sensor`     |     name of output snow sensor (default "snow")
`localities`     |     list of locality_ids where snow sill be calculated; if NULL then all (default NULL)
`dr`     |     delta range (maximal daily temperature range on sensor covered by snow)
`tmax`     |     maximal daily temperature on sensor covered by snow


## Details

Function was designed estimate to snow presence from temperature in situation when temperature sensor is covered by snow. Snow detection algorithm combines daily range `dr` of temperature with the maximal daily temperature `tmax` . I.e in default settings TRUE (snow presence) is returned when daily temperature range is lower than 2°C and daily maximal temperature is lower than 0.5 °C.
 
 TRUE/FALSE = Snow/non-snow information is returned in original time step (e.g. 15 min, 1 h...) despite function operate with daily temperature range and maximum. Because of dependency on daily temperatures, the longest time step for snow detection allowed is day.


## Value

The new myClim data object, identical as input with added snow sensor. Time step is not modified.


## Examples

```r
snow <- mc_calc_snow(example_tomst_data1, "TMS_T2", output_sensor="TMS_T2_snow")
```


# `mc_calc_vwc`

Converting soil moisture from TDT signal to volumetric water content


## Description

Function converting soil moisture from TDT signal to volumetric water content.


## Usage

```r
mc_calc_vwc(
  data,
  moist_sensor = "TMS_TMSmoisture",
  temp_sensor = "TMS_T1",
  output_sensor = "vwc_moisture",
  soiltype = "universal",
  localities = NULL,
  ref_t = myClim:::.calib_MOIST_REF_T,
  acor_t = myClim:::.calib_MOIST_ACOR_T,
  wcor_t = myClim:::.calib_MOIST_WCOR_T
)
```


## Arguments

Argument      |Description
------------- |----------------
`data`     |     in format for calculation
`moist_sensor`     |     name of soil moisture sensor (default "TMS_TMSmoisture")  Soil moisture sensor must be in TMSmoisture physical. Function use sensor$calibration table for calculation values of new sensor and copy table to new_sensor. Value sensor$metadata@calibrated is derived from calibration parameters.
`temp_sensor`     |     name of soil temperature sensor (default "TMS_T1")  Temperature sensor must be in T physical.
`output_sensor`     |     name of new snow sensor (default "vwc_moisture")
`soiltype`     |     value from mc_data_vwc_parameters in column soiltype (default "universal")  Parameters a, b and c are used in calculation.
`localities`     |     list of locality_ids for calculation; if NULL then all (default NULL)
`ref_t`     |     (default 24)
`acor_t`     |     (default 1.91132689118083)
`wcor_t`     |     (default 0.64108)


## Details




## Value

input data with added VWC moisture sensor


## Examples

```r
calc_data <- mc_calc_vwc(calc_data, soiltype="sand", localities="A2E32")
```


# `mc_calib_moisture`

Calculation slope and intercept parameters of TMSmoisture sensor


## Description

Function calculate slope and intercept parameters from measuring moisture in water and air.


## Usage

```r
mc_calib_moisture(
  raw_air,
  raw_water,
  t_air = 24,
  t_water = 24,
  ref_air = 114.534,
  ref_water = 3634.723,
  ref_t = .calib_MOIST_REF_T,
  acor_t = .calib_MOIST_ACOR_T,
  wcor_t = .calib_MOIST_WCOR_T
)
```


## Arguments

Argument      |Description
------------- |----------------
`raw_air`     |     TDT signal in air
`raw_water`     |     TDT signal in water
`t_air`     |     temperature of air (default 24)
`t_water`     |     temperature of air (default 24)
`ref_air`     |     (default 114.534)
`ref_water`     |     (default 3634.723)
`ref_t`     |     (default 24)
`acor_t`     |     (default 1.91132689118083)
`wcor_t`     |     (default 0.64108)


## Details




## Value

list with slope and intercept parameters


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

An object of class `list` of length 11.


## Usage

```r
mc_data_sensors
```


# `mc_data_vwc_parameters`

Volumetric water content parameters


## Description

data.frame with columns:
  

*  soiltype 

*  a 

*  b 

*  c 

*  rho 

*  clay 

*  silt 

*  sand 

*  ref


## Format

An object of class `data.frame` with 13 rows and 9 columns.


## Usage

```r
mc_data_vwc_parameters
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
mc_filter(
  data,
  localities = NULL,
  sensors = NULL,
  reverse = FALSE,
  stop_if_empty = TRUE
)
```


## Arguments

Argument      |Description
------------- |----------------
`data`     |     in format for preparing or calculation
`localities`     |     locality_ids for filtering data; if NULL then do nothing
`sensors`     |     sensor_ids for filtering data; if NULL then do nothing
`reverse`     |      

*  if TRUE then filtered discard else keeped (default FALSE)
`stop_if_empty`     |      

*  if TRUE then error for empty output (default TRUE)


## Value

filtered data in same format as input


## Examples

```r
example_tomst_data1 <- mc_filter(example_tomst_data1, localities=c("A6W79", "A2E32"), sensors=c("TMS_T1", "TMS_T2"))
```


# `mc_info_clean`

get clean info table


## Description

This function return dataframe with info about cleaning loggers


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


# `mc_info`

get sensors info table


## Description

This function return dataframe with info about sensors


## Usage

```r
mc_info(data)
```


## Arguments

Argument      |Description
------------- |----------------
`data`     |     in format for preparing or calculation


## Value

dataframe with columns locality_id, serial_number, sensor_id, sensor_name, start_date, end_date, step, step_text, min_value, max_value, count_values, count_na


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


# `mc_MainMetadata-class`

Class for main metadata in data format for calculation


## Description

Class for main metadata in data format for calculation


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
  title = "",
  localities = NULL,
  sensors = NULL,
  height = 1900,
  left_margin = 12
)
```


## Arguments

Argument      |Description
------------- |----------------
`data`     |     in format for preparing or calculation
`filename`     |     output filename
`title`     |     of plot; default is empty
`localities`     |     names of localities; if empty then all
`sensors`     |     names of sensors; if empty then all
`height`     |     of image; default = 1900
`left_margin`     |     width of space for sensor_labels; default = 12


## Examples

```r
mc_plot_image(data, "T1_image.png", "T1 sensor", sensors="TMS_T1")
```


# `mc_plot_line`

Plot data - ggplot2 geom_line


## Description

Function plot data to file with ggplot2 geom_line


## Usage

```r
mc_plot_line(
  data,
  filename,
  sensors = NULL,
  scale_coeff = NULL,
  png_width = 1900,
  png_height = 1900,
  start_crop = NULL,
  end_crop = NULL
)
```


## Arguments

Argument      |Description
------------- |----------------
`data`     |     in format for preparing or calculation
`filename`     |     output - supported formats are pdf and png
`sensors`     |     names of sensor
`scale_coeff`     |     scale coefficient for secondary axis (default NULL)  Values from secondary axis are scaled with calculation values * scale_coeff. If coefficient is NULL than function try detects scale coefficient from physical unit of sensors.
`png_width`     |     width for png output (default 1900)
`png_height`     |     height for png output (default 1900)
`start_crop`     |     POSIXct datetime for crop data (default NULL)
`end_crop`     |     POSIXct datetime for crop data (default NULL)


## Details

Maximal number of physical units of sensors is two. Main and secondary y axis.


# `mc_plot_loggers`

Plot data from loggers


## Description

Function plot loggers to directory


## Usage

```r
mc_plot_loggers(
  data,
  directory,
  localities = NULL,
  sensors = NULL,
  crop = c(NA, NA)
)
```


## Arguments

Argument      |Description
------------- |----------------
`data`     |     in format for preparing
`directory`     |     output directory
`localities`     |     names of localities; if NULL then all
`sensors`     |     names of sensors; if NULL then all
`crop`     |     datetime range for plot, not cropping if NA (default c(NA, NA))


## Examples

```r
mc_plot_loggers(example_tomst_data1, "Figures")
```


# `mc_plot_raster`

Plot data - ggplot2 geom_raster


## Description

Function plot data to file with ggplot2 geom_raster


## Usage

```r
mc_plot_raster(
  data,
  filename,
  sensors = NULL,
  by_hour = TRUE,
  png_width = 1900,
  png_height = 1900,
  viridis_color_map = NULL,
  start_crop = NULL,
  end_crop = NULL
)
```


## Arguments

Argument      |Description
------------- |----------------
`data`     |     in format for preparing or calculation
`filename`     |     output - supported formats are pdf and png
`sensors`     |     names of sensor; should have same unit
`by_hour`     |     if TRUE, then y axis is hour, alse time (default TRUE)
`png_width`     |     width for png output (default 1900)
`png_height`     |     height for png output (default 1900)
`viridis_color_map`     |     viridis color map option; if NULL, then used value from mc_data_physical  

*  "A" - magma 

*  "B" - inferno 

*  "C" - plasma 

*  "D" - viridis 

*  "E" - cividis 

*  "F" - rocket 

*  "G" - mako 

*  "H" - turbo
`start_crop`     |     POSIXct datetime for crop data (default NULL)
`end_crop`     |     POSIXct datetime for crop data (default NULL)


# `mc_prep_calib_load`

load calibration parameters


## Description

This function load calibration parameters from data.frame


## Usage

```r
mc_prep_calib_load(data, calib_table)
```


## Arguments

Argument      |Description
------------- |----------------
`data`     |     in format for preparing
`calib_table`     |     data.frame with columns (serial_number, sensor_id, datetime, slope, intercept)


## Details

It is not possible change calibration parameters in calibrated sensor.


## Value

data with loaded calibration informations.


# `mc_prep_calib`

Sensor calibration


## Description

This function calibrate values of sensor by sensor$calibration parameters. Values are changed
 and parameter sensor$metadata@calibrated is set to TRUE. It isn't possible calibrate calibrated sensor again.


## Usage

```r
mc_prep_calib(data, sensors, localities = NULL)
```


## Arguments

Argument      |Description
------------- |----------------
`data`     |     in format for preparing or calculation
`sensors`     |     vector of sensor names for calibration  It is not possible calibrate TMSmoisture sensor with this function. Calibration of TMSmoisture sensor is processed in mc_calc_vwc during conversion to volumetric water content. Only sensors with real value type can be calibrated.
`localities`     |     vector of locality_ids, if NULL, then calibrate in all localities (default NULL)


## Details




## Value

data with calibrated sensors.


# `mc_prep_clean`

Cleaning datetime series


## Description

This function guess time step from regular time series. After that produce perfectly regular time series based on guessed step together with the first and last record.
 Using clean time series, function check weather the original time series is continual without missing values, check duplicated and disordered records.
 Resulting myClim object time series has constant step without duplicated and disordered records. Resulting time series is nicely rounded. See details. Disordered records are reordered chronologically, missing values are filled with NAs.


## Usage

```r
mc_prep_clean(data, silent = FALSE)
```


## Arguments

Argument      |Description
------------- |----------------
`data`     |     myClim object in raw format (output of `mc_read` functions family) see e.g. [`mc_read_directory()`](#mcreaddirectory())
`silent`     |     if true, then cleaning log table is not printed in console (default FALSE), see [`mc_info_clean()`](#mcinfoclean())


## Details

`mc_prep_clean` is initial, mandatory step before further work with data in `myClim` library.
 
 This function guarantee time series with constant time step, without duplicated and disordered records which is crucial for next steps in data analysis.
 `mc_prep_clean` assume constant time step in microclimatic records. The step is guessed from input time series based on last 100 records. In case of microclimatic logger with irregular time series, function returns warning and skip logger.
 
 In case the time step is regular, but is not nicely rounded, function round the time series to the closest nice time and shift original data to nicely rounded time series. (e.g. original records in 10 min regular step c(11:58, 12:08, 12:18, 12:28) are shifted to newly generated nice sequence c(12:00, 12:10, 12:20, 12:30) microclimatic records are not modified but only shifted).


## Value

*  myClim object in clean format 

*  cleaning log is by default printed in console, and can be called ex post by [`mc_info_clean()`](#mcinfoclean())


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
mc_prep_crop(data, start = NULL, end = NULL, end_included = TRUE)
```


## Arguments

Argument      |Description
------------- |----------------
`data`     |     in format for preparing or calculation
`start`     |     POSIXct datetime in UTC; is optional; start datetime is included
`end`     |     POSIXct datetime in UTC; is optional
`end_included`     |     if TRUE then  end datetime is included (default TRUE)


## Value

cropped data in standard format


## Examples

```r
cleaned_example_tomst_data1 <- mc_prep_crop(example_tomst_data1, end=as.POSIXct("2020-02-01", tz="UTC"))
```


# `mc_prep_merge`

Merge data


## Description

This function merge two instances of data to one


## Usage

```r
mc_prep_merge(data_items)
```


## Arguments

Argument      |Description
------------- |----------------
`data_items`     |     list of data in format for preparing or calculation; Format of data must be same.


## Details

If data1 and data2 contains locality with same locality_id, than locality_id from data2 is renamed.


## Value

merged data


## Examples

```r
merged_tomst_data <- mc_prep_merge(list(example_tomst_data1, example_tomst_data2))
```


# `mc_prep_rename_locality`

rename locality_id


## Description

This function change locality_ids.


## Usage

```r
mc_prep_rename_locality(data, locality_ids)
```


## Arguments

Argument      |Description
------------- |----------------
`data`     |     in format for preparing or calculation
`locality_ids`     |     list with new locality_ids; names of items are old ones


## Value

data with changed locality_ids


## Examples

```r
data <- mc_prep_rename_locality(example_tomst_data1, list(A1E05="ABC05", A2E32="CDE32"))
```


# `mc_prep_rename_sensor`

Rename sensor


## Description

This function rename sensors.


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


# `mc_prep_solar_tz`

Set solar time offset against original time


## Description

This function calculates the offset against UTC on the locality  to get the solar time. This is based on coordinates. If coordinates not provided, then not working.


## Usage

```r
mc_prep_solar_tz(data)
```


## Arguments

Argument      |Description
------------- |----------------
`data`     |     myClim object in raw, clean, or calculation format


## Details

The function require at least longitude provided in locality metadata slot `lon_wgs84` . If longitude not provided, function not works. Coordinates of locality can be provided e. g. during data reading see [`mc_read_data_frame()`](#mcreaddataframe()) , [`mc_read_csv()`](#mcreadcsv()) 
 
 TZ offset in minutes is calculated as `longitude / 180 * 12 * 60` .


## Value

MyClim object in the same format as input, with `tz_offset` filled in locality metadata


## Examples

```r
cleaned_example_tomst_data1 <- mc_prep_solar_tz(cleaned_example_tomst_data1)
```


# `mc_prep_user_tz`

Set user defined offset against original time


## Description

This function allow user to set the offset in minutes against original time series of microclimatic records. `MyClim` generally assume the records are in UTM Time Zone. When offset is provided, myClim functions by default use the time corrected with this offset in calculations. See details.


## Usage

```r
mc_prep_user_tz(data, tz_offsets)
```


## Arguments

Argument      |Description
------------- |----------------
`data`     |     myClim object in raw, clean, or calculation format
`tz_offsets`     |     named list (name: `locality_id` , list item: `tz_offset` in rounded minutes)


## Details

For analysis of microclimatic data it is important to think of time zones because of diurnal or seasonal microclimatic rhythms. `mc_prep_user_tz` allow user to set time offset for individual localities to harmonize e.g.day/night cycles across vhole data set.
 
 This function can be used also inversely, for heterogeneous data sets containing loggers recording in local time and user wish to unify them by setting individual offest e.g. to UTM. This function is also useful for corrections of shifted loggers time series e.g. due to technical issue.
 
 In case user is sure, the loggers recorded in UTC and wants to harmonize data set to solar time, there is [`mc_prep_solar_tz()`](#mcprepsolartz()) calculating offset based on coordinates to harmonize the midday across vhole dataset.


## Value

MyClim object in the same format as input, with `tz_offset` filled in locality metadata


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
example_tomst_data <- myClim::mc_read_csv("examples/data/TOMST/files_table.csv")
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
example_tomst_data <- myClim::mc_read_directory("examples/data/TOMST/", "TOMST")
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
example_tomst_data <- myClim::mc_read_files(c("examples/data/TOMST/data_91184101_0.csv", "examples/data/TOMST/data_94184102_0.csv"), "TOMST")
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
example_tms_t1_table <- myClim::mc_reshape_long(example_tomst_data, c("A6W79", "A2E32"), c("TMS_T1", "TMS_T2"))
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

data.frame with datetime column and columns for every sensor; Name of column is in format
 locality_id list("serial_number") sensor_name for preparing format and
 locality_id _ sensor_name for calculation format.


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


# `mc_TOMSTDataFormat-class`

Class for source file data format for TOMST logger


## Description

Class for source file data format for TOMST logger


# `mc_TOMSTJoinDataFormat-class`

Class for source file data format for joined TOMST logger


## Description

Class for source file data format for joined TOMST logger


# `myClim-package`

myClim: R package for processing microclimatic data


## Description

Package myClim use data in two formats. Names of formats are prep-format and calc-format.
 Loaded data from source files by read functions are in prep-format. It Contains information about localities,
 loggers and sensors. Function mc_agg converts data to calc-format. Calc-format doesn't contain loggers metadata.
 Sensors are organized in localities. Calc-format is focused to analyzing data and calculation new virtual sensors.
 
 Prep-format
 list("\n", "+--------------------------------------------------------------------------+\n", "| locality[1]                                                              |\n", "+--------------------------------------------------------------------------+\n", "|           +-----------------------------+                                |\n", "| $metadata | mc_LocalityMetadata - class |                                |\n", "|           +-----------------------------+                                |\n", 
    "|           | @locality_id                |                                |\n", "|           | @altitude                   |                                |\n", "|           | @lat_wgs84                  |                                |\n", "|           | @lon_wgs84                  |                                |\n", "|           | @tz_offset                  |                                |\n", "|           | @tz_type                    |                                |\n", "|           | @user_data                  |                                |\n", 
    "|           +-----------------------------+                                |\n", "|           +-------------------------------------------------------------+|\n", "| $loggers  | logger[1]                                                   ||\n", "|           +-------------------------------------------------------------+|\n", "|           |             +---------------------------+                   ||\n", "|           | $metadata   | mc_LoggerMetadata - class |                   ||\n", "|           |             +---------------------------+                   ||\n", 
    "|           |             | @type                     |                   ||\n", "|           |             | @serial_number            |                   ||\n", "|           |             +---------------------------+                   ||\n", "|           |             +----------------------------+                  ||\n", "|           | $clean_info | mc_LoggerCleanInfo - class |                  ||\n", "|           |             +----------------------------+                  ||\n", "|           |             | @step                      |                  ||\n", 
    "|           |             | @count_duplicits           |                  ||\n", "|           |             | @count_missed              |                  ||\n", "|           |             | @count_disordered          |                  ||\n", "|           |             +----------------------------+                  ||\n", "|           | $datetime   POSIXct vector                                  ||\n", "|           |             +----------------------------------------------+||\n", "|           | $sensors    | sensor[1]                                    |||\n", 
    "|           |             +----------------------------------------------+||\n", "|           |             |              +---------------------------+   |||\n", "|           |             | $metadata    | mc_SensorMetadata - class |   |||\n", "|           |             |              +---------------------------+   |||\n", "|           |             |              | @sensor_id                |   |||\n", "|           |             |              | @name                     |   |||\n", "|           |             |              | @height                   |   |||\n", 
    "|           |             |              | @calibrated               |   |||\n", "|           |             |              +---------------------------+   |||\n", "|           |             |              +----------+-------+-----------+|||\n", "|           |             | $calibration | datetime | slope | intercept ||||\n", "|           |             |              | ...      | ...   | ...       ||||\n", "|           |             |              +----------+-------+-----------+|||\n", "|           |             | $values      numeric/logical vector          |||\n", 
    "|           |             |              +-----+-------+-----+           |||\n", "|           |             | $states      | tag | start | end |           |||\n", "|           |             |              | ... | ...   | ... |           |||\n", "|           |             |              +-----+-------+-----+           |||\n", "|           |             +----------------------------------------------+||\n", "|           |             +----------------------------------------------+||\n", "|           |             | sensor[2]                                    |||\n", 
    "|           |             +----------------------------------------------+||\n", "|           |             ...                                             ||\n", "|           |             +----------------------------------------------+||\n", "|           |             | sensor[n]                                    |||\n", "|           |             +----------------------------------------------+||\n", "|           +-------------------------------------------------------------+|\n", "|           +-------------------------------------------------------------+|\n", 
    "|           | logger[2]                                                   ||\n", "|           +-------------------------------------------------------------+|\n", "|           ...                                                            |\n", "|           +-------------------------------------------------------------+|\n", "|           | logger[n]                                                   ||\n", "|           +-------------------------------------------------------------+|\n", "+--------------------------------------------------------------------------+\n", 
    "+--------------------------------------------------------------------------+\n", "| locality[2]                                                              |\n", "+--------------------------------------------------------------------------+\n", "...\n", "+--------------------------------------------------------------------------+\n", "| locality[n]                                                              |\n", "+--------------------------------------------------------------------------+") 
 
 Calc-format
 list("\n", "            +-------------------------+\n", "$metadata   | mc_MainMetadata - class |\n", "            +-------------------------+\n", "            | @step                   |\n", "            | @step_text              |\n", "            +-------------------------+\n", "            +------------------------------------------------------------+\n", "$localities | locality[1]                                                |\n", "            +------------------------------------------------------------+\n", 
    "            |            +-----------------------------+                 |\n", "            | $metadata  | mc_LocalityMetadata - class |                 |\n", "            |            +-----------------------------+                 |\n", "            |            | @locality_id                |                 |\n", "            |            | @altitude                   |                 |\n", "            |            | @lat_wgs84                  |                 |\n", "            |            | @lon_wgs84                  |                 |\n", 
    "            |            | @tz_offset                  |                 |\n", "            |            | @tz_type                    |                 |\n", "            |            | @user_data                  |                 |\n", "            |            +-----------------------------+                 |\n", "            | $datetime  POSIXct vector                                  |\n", "            |            +----------------------------------------------+|\n", "            | $sensors   | sensor[1]                                    ||\n", 
    "            |            +----------------------------------------------+|\n", "            |            |              +---------------------------+   ||\n", "            |            | $metadata    | mc_SensorMetadata - class |   ||\n", "            |            |              +---------------------------+   ||\n", "            |            |              | @sensor_id                |   ||\n", "            |            |              | @name                     |   ||\n", "            |            |              | @height                   |   ||\n", 
    "            |            |              | @calibrated               |   ||\n", "            |            |              +---------------------------+   ||\n", "            |            |              +----------+-------+-----------+||\n", "            |            | $calibration | datetime | slope | intercept |||\n", "            |            |              | ...      | ...   | ...       |||\n", "            |            |              +----------+-------+-----------+||\n", "            |            | $values      numeric/logical vector          ||\n", 
    "            |            |              +-----+-------+-----+           ||\n", "            |            | $states      | tag | start | end |           ||\n", "            |            |              | ... | ...   | ... |           ||\n", "            |            |              +-----+-------+-----+           ||\n", "            |            +----------------------------------------------+|\n", "            |            +----------------------------------------------+|\n", "            |            | sensor[2]                                    ||\n", 
    "            |            +----------------------------------------------+|\n", "            |            ...                                             |\n", "            |            +----------------------------------------------+|\n", "            |            | sensor[n]                                    ||\n", "            |            +----------------------------------------------+|\n", "            +------------------------------------------------------------+\n", "            +------------------------------------------------------------+\n", 
    "            | locality[2]                                                |\n", "            +------------------------------------------------------------+\n", "            ...\n", "            +------------------------------------------------------------+\n", "            | locality[n]                                                |\n", "            +------------------------------------------------------------+")


