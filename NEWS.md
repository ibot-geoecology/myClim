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