% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/prep.R
\name{mc_prep_calib_load}
\alias{mc_prep_calib_load}
\title{Load calibration to correct microclimatic records}
\usage{
mc_prep_calib_load(data, calib_table)
}
\arguments{
\item{data}{myClim object in Raw-format. see \link{myClim-package}}

\item{calib_table}{data.frame with columns (serial_number, sensor_id, datetime, slope, intercept)}
}
\value{
myClim object with loaded calibration information in metadata.
Microclimatic records are not calibrated, only ready for calibration.
To calibrate records run \code{\link[=mc_prep_calib]{mc_prep_calib()}}
}
\description{
This function loads calibration parameters from data.frame
and writes them into myClim object metadata. This function
does not calibrate data. For calibration itself run \code{\link[=mc_prep_calib]{mc_prep_calib()}}
}
\details{
This function allows user to provide calibration values either from DIY or
certified calibration procedure. Calibration data have by default the form
of linear function determined by the \code{cor_factor} and \code{cor_slope}:

\code{calibrated = original * (cor_slope + 1) + cor_factor}

This is useful in
case of multi-point calibration typically performed by certified calibration
labs. In case of one-point calibration typically DIY
calibrations only \code{cor_factor} is used and \code{cor_slope=0}. One point calibration
is thus only the addition of correction factor. This function loads sensor specific
calibration values from data frame and writs them into myClim Raw-format
object metadata. The structure of input data frame is as follows:
\itemize{
\item serial_number = unique identification of logger hosting the sensors e.g. 91184101
\item sensor_id = the name of sensor to calibrate e.g. TMS_T1
\item datetime = the date of the calibration
\item cor_factor = the correction factor, in case of multi-point calibration the intercept of calibration curve.
\item cor_slope = the slope of calibration curve (in case of one-point calibration slope = 0)
}

It is not possible to change calibration parameters for already calibrated sensor.
This prevents repeated calibrations. Once \code{\link[=mc_prep_calib]{mc_prep_calib()}} is called then
it is not allowed to provide new calibration data, neither run calibration again.
}
