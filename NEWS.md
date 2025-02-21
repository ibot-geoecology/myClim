# myClim 1.4.0
* The save localities unit test has been fixed.
* CRAN version.

# myClim 1.3.11
* The numbers in the `TOMS_join` format can be in scientific notation.

# myClim 1.3.10
* The environment `mc_read_problems` with vroom parse problems has been added.

# myClim 1.3.9
* The `clean_conflict` states issue in the `mc_prep_clean` function has been fixed.
* The `NA` values in the `mc_prep_TMSoffsoil` function have been fixed.

# myClim 1.3.8
* The `tag` parameter has been added to the `mc_plot_line` function.

# myClim 1.3.7
* The uninitialsed date_format warning in the `mc_read_data` function has been fixed. 
* The `cropt_table` parameter has been added to the `mc_prep_crop` function.
* The `start` and `end` parameters in the `mc_prep_crop` function can be only single value.
* The `crop_margins_NA` parameter has been added to the `mc_states_replace` function.

# myClim 1.3.6
* A new function `mc_save_localities` has been added.
* The function `mc_load` can load multiple files.
* The function `mc_load` can load all .RDS files in a directory.
* The warnings about missing sensors have been removed.

# myClim 1.3.5
* The `logger_name` is used in the `mc_states_join` function instead of the `logger_index` as value in the states table.
* The result of `mc_prep_TMSoffsoil` function has been changed to `TRUE`/`FALSE` instead of `1`/`0`.

# myClim 1.3.4
* The `logger_name` is used in myClim object instead of the `logger_index`.
* The `logger_name` is visible in `mc_plot_line`.
* The `show_logger_name` parameter has been added to the `mc_reshape_wide` function.
* The `serias_name` label changed to `series` in `mc_plot_line`.
* The progress bar is added to `mc_prep_crop`.
* The `mc_prep_clean` step warning is fixed.

# myClim 1.3.3
* A performance issue in the `mc_prep_crop` function has been fixed.
* The parameters `tolerance` and `by_type` have been added to the `mc_join` function.
* The class `mc_LocalityMetadata` has been extended with the `join_serial` slot.
* The new function `mc_states_join` has been added.
* The function `mc_info_join` has been deleted.

# myClim 1.3.2
* The parameter `tolerance` has been added to the `mc_prep_clean` function.
* The result table of the `mc_info_clean` function has been extended with the `logger_index` column.
* The plot colors of sensors have been updated.
* The colors issue in the `mc_plot_line` function has been fixed.

# myClim 1.3.1
* The conflict state issue in the `mc_prep_clean` function has been fixed.

# myClim 1.3.0
* Published on CRAN.

# myClim 1.2.6
* New functions `mc_info_range` and `mc_states_outlier` have been added.

# myClim 1.2.5
* The issue with the HOBO `logger_type` in the `mc_read_data` function has been fixed.

# myClim 1.2.4
* The multiple custom date formats issue for HOBO logger in the `mc_read_data` function has been fixed.

# myClim 1.2.3
* It is possible to define multiple custom date formats in the `mc_read_data` function.

# myClim 1.2.2
* The issue in the `mc_join` function with the `use always newer logger` option has been fixed.

# myClim 1.2.1
* The new functions `mc_states_from_sensor` and `mc_states_to_sensor` have been added.

# myClim 1.2.0
* The color palette of `mc_plot_line` function has been extended.

# myClim 1.1.6
* New function `mc_states_replace` has been added.

# myClim 1.1.5
* Updated the color palette of `mc_plot_line` for greater differentiation between values.

# myClim 1.1.4
* Fixed the issue with the joined series of loggers in the `mc_plot_line` function.
* Added a new parameter `color_by_logger` to the `mc_plot_line` function.
* Added a new parameter `logger_types` to the `mc_filter` function.

# myClim 1.1.3
* Fixed reading of TOMST Thermologger data CSV. The data format has changed in the new version of Lolly.

# myClim 1.1.2
* The `mc_prep_clean` function now selects a conflicted value based on the best match with the new rounded date and time.
* A new parameter, `resolve_conflicts`, has been added to the `mc_prep_clean` function to allow checking uncleaned data.
* Fixed the round-off issue of datetimes in states when an unconventional step is used.
 
# myClim 1.1.1
* New functions `mc_info_states`, `mc_states_insert`, `mc_states_update`, and `mc_states_delete` have been added.
* The `mc_agg` function now rounds states by period.

# myClim 1.1.0
* Fixed an issue with the read progress bar when some files were skipped.
* Changed the logger type `HOBO` to `HOBO_U23-001A`.
* Added support for the logger type `HOBO_U23-004`.

# myClim 1.0.19
* Fixed an issue that affected the calibration table after calling the `mc_join()` function.
* Added a file counter to the `mc_plot_loggers()` function.

# myClim 1.0.18
* Fixed temperature drift correction in the `mc_calc_vwc()` function.
* Resolved the issue where the `vroom::problems()` function did not function properly when tidyverse was imported.
  This is now handled in the read functions.
* Disabled progress bar for the `vroom::vroom()` function.

# myClim 1.0.17
* Multiple custom date formats can be defined for TOMST data format.

# myClim 1.0.16
* A new function, `mc_info_join()`, has been added. This function attempts to join myClim objects
  and returns an overview of the operation.
* The skip option has been added to the `mc_join()` function.
* The `mc_read_data()` function can now read HOBO files which use a comma as the decimal separator.
* The logger index has now been added to the column name in the `mc_reshape_wide()` function.
* Progress bars have been added to the `mc_read_data()`, `mc_prep_clean()`, `mc_join()`, and `mc_agg()` functions.
* When new sensors are created from a custom function in the `mc_agg()` function, they now inherit the `sensor_id`
  from the parent sensors.
* If a wrong physical unit is input into the calc functions, it will now result in a warning instead of an error.

# myClim 1.0.15
* when reading `TOMST_join` format (internal TOMST data format used by IBOT researchers) the `mc_read_data()`
  function now uses the `Thermo` value of the `logger_type` parameter when reading TOMST Termo Logger
  and does not detect the type of the logger from the data.
* A new function, `mc_info_logger()`, has been added. This function returns an overview table of loggers in myClim Raw-format.
* The issue with NA values in the `mc_join()` function has been fixed.
* The issue with always choices in the `mc_join()` function has been fixed.
* The `TRUE` value in `TOMST_join` is now correctly detected.
* The `mc_join()` function does not fail if a wrong logger type is defined.

# myClim 1.0.14
* The function `mc_read_data()` now skips non-existent files.
* The `length()` function now returns the number of localities for `myClim` object.
* Detection of `TOMST_join` format is now fixed for negative values of moisture.

# myClim 1.0.13
* The issue of NA values at the beginning of TOMST_join format data files has been resolved.

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