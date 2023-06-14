# myClim 1.0.6
* The `mc_prep_crop()` function now includes a new parameter, `localities`, which allows for specific localities to be selected.
* The definitions of physicals, loggers, and sensors have been modified.
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