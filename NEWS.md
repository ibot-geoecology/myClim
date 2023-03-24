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