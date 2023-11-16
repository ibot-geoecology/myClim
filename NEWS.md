# myClim 1.0.12
* A new `facet` parameter has been added to the `mc_plot_line()` function to provide more flexibility 
  in data visualization.
* The y-axis labels for Boolean sensors in the `mc_plot_line()` function have been updated to `TRUE` and `FALSE`.
* The function `mc_calib_moisture()` now produces renamed results to enhance consistency.
* The manual and vignettes have been enhanced to provide more comprehensive and user-friendly documentation.

# myClim 1.0.11
* The reading of TOMST data with a comma decimal separator was incorrect in the 1.0.9 and 1.0.10 versions.
  This error has been fixed.

# myClim 1.0.10
* Dependency on the package `runner` is removed and `data.table` is added as a new dependency.
* In the functions `mc_read_files()` and `mc_read_data()`, a new parameter `user_data_formats` is added
  for custom source file data formats.
* A new vignette for user data formats is added.
* A check for different values in same time is added to the function `mc_prep_clean()`.
* A new function `mc_prep_TMSoffsoil()` is added for detecting when TMS is out of soil.

# myClim 1.0.9
* The citation of the package has been changed to the article **myClim: Microclimate data handling and standardised
  analyses in R**.
* The reading of CSV data files is now processed by the `vroom` package, which provides a faster and more efficient way
  to read CSV data.

# myClim 1.0.8
* The calibration constants for the `mc_calc_vwc()` function are now visible to the user under new names:
  `mc_const_CALIB_MOIST_REF_T`, `mc_const_CALIB_MOIST_ACOR_T`, and `mc_const_CALIB_MOIST_WCOR_T`.
* The sensor ID constants are now visible to the user under new names: `mc_const_SENSOR_*`.
* The `soiltype` parameter in the `mc_calc_vwc()` function can now be a list with user-defined parameters
  in the format `list(a=number1, b=number2, c=number3)`. 

# myClim 1.0.7
* The temperature correction is no longer applied in the `mc_calc_vwc()` function when the `temp_sensor` value is NA.

# myClim 1.0.6
* The `mc_prep_crop()` function now includes a new parameter `localities`, which allows for specific localities to be selected.
* The definitions of physicals, loggers, and sensors have been modified.
  * `TMS_TMSmoisture` -> `TMS_moist`
  * `TS_T` -> `Thermo_T`
  * `DEND_T` -> `Dendro_T`
  * `DEND_TOMSTdendro` -> `Dendro_raw`
  * `HOBO_T_C` -> `HOBO_T`
  * `moisture` -> `VWC`
  * `RH_perc` -> `RH`
  * `wind` -> `wind_speed`
  * deleted `HOBO_T_F`
* Data from HOBO loggers in °F are now automatically converted to °C.

# myClim 1.0.5
* Metadata of a locality can now be loaded from a table using the `mc_read_data()` function.
* The parameter `use_utc` has been added to the following functions: `mc_reshape_wide()`, `mc_reshape_long()`,
  `mc_plot_image()`, `mc_plot_line()` and `mc_plot_raster()`. This parameter allows for the use of local time.

# myClim 1.0.4
* Updated the `print(myClim_data)` function to display the metadata of the `myClim` object and the head of the table from `mc_info()`.
* Enabled the use of the extraction operator `[]` to extract specific localities from the `myClim` object.

# myClim 1.0.3
* Function `mc_plot_raster()` can plot bool type sensors, such as snow.

# myClim 1.0.2
* Enhanced help texts.
* Added a check in `mc_prep_calib_load()` to verify if the type of `calib_table$datetime` is `POSIXct`.

# myClim 1.0.1
* Updated help texts to comply with CRAN rules.
* Restored `options` and `par` using the `on.exit()` function.
* Replaced `print()` with `message()` in the code.
* Utilized temporary directories in tests, examples, and vignettes.
* Some `dontrun` examples have been modified to run.
* Detection of the installed plotly package using the `system.file()` function.
 
# myClim 1.0.0

* Improved the vignette.
* Added the ability to specify multiple TubeDB regions using the `region` parameter in the `mc_read_tubedb()` function,
  providing more flexibility in data retrieval.
* Automatically prepared the `sensor_ids` parameter in the `mc_read_tubedb()` function even if region is `NULL`.
* Fixed the issue with factors in the `localities_table` in the `mc_read_data()` function.
* Fixed the issue with factors in the `states` table in the sensor data list.
* Concluded beta testing.

# myClim 0.3.1

* Deletion of duplicated items in function `mc_prep_clean()` is more effective.
* The parameter `sensor_ids` in function `mc_read_tubedb()` is prepared automatically if names in TubeDB
  are identical with names in myClim.
* Conversion of datetimes in function `mc_read_tubedb()` is fixed. The problem is in function
  `rTubeDB::query_timeseries` if the parameter `datetimeFormat = "POSIXct"` is used.
* The issue with loading data from TubeDB when the elevation is missing has been fixed.

# myClim 0.3.0

* Behavior of `mc_filter(data, localities="locality_abc", sensors="sensor1", reverse=TRUE)` has changed. 
  Sensor `sensor1` is removed only from locality `locality_abc`. Other localities remain unchanged.
* Function `mc_join()` is fixed. Multiple sensors with the same height caused an error.