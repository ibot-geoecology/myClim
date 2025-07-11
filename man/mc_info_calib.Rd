% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/info.R
\name{mc_info_calib}
\alias{mc_info_calib}
\title{Get calibration info table}
\usage{
mc_info_calib(data)
}
\arguments{
\item{data}{myClim object see \link{myClim-package}}
}
\value{
data.frame with columns:
\itemize{
\item locality_id - when provided by user then locality ID, when not provided identical with serial number
\item logger_name - name of logger in myClim object at the locality (e.g., "Thermo_1", "TMS_2")
\item sensor_name - sensor name either original (e.g., TMS_T1, T_C), or calculated/renamed (e.g., "TMS_T1_max", "my_sensor01")
\item datetime - date and time of calibration
\item cor_factor - correction factor applied to the sensor values
\item cor_slope - the slope of calibration curve
}
}
\description{
This function return data.frame with calibration parameter of sensors loaded by \code{\link[=mc_prep_calib_load]{mc_prep_calib_load()}}.
}
\examples{
mc_info_calib(mc_data_example_clean)
}
