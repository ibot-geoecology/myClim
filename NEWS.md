# myClim 0.3.0

* Behavior of `mc_filter(data, localities="locality_abc", sensors="sensor1", reverse=TRUE)` is changed. Sensor `sensor1`
  is removed only from locality `locality_abc`. Other localities are unchanged.
* Function `mc_join()` is fixed. Multiple sensors with the same height caused an error.