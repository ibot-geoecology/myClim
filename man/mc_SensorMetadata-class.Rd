% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/model.R
\docType{class}
\name{mc_SensorMetadata-class}
\alias{mc_SensorMetadata-class}
\alias{mc_SensorMetadata}
\title{Class for sensor metadata}
\description{
Class for sensor metadata
}
\details{
\code{sensor_id} must be one of the defined id in myClim. see \link{mc_data_sensors}.
It is useful to select on of predefined, because it makes plotting and calculaton easier.
Through \code{sensor_id} myClim assign pre-deined physicyl units or plotting colors see \link{mc_Sensor}.
}
\section{Slots}{

\describe{
\item{\code{sensor_id}}{unique identifier of sensor (TMS_T1, TMS_T2, TMS_T3, TMS_moist, ...) \link{mc_data_sensors} e.g. TMS_T1, TMS_moist, snow_fresh...}

\item{\code{name}}{character, could be same as \code{sensor_id} but also defined by function or user.}

\item{\code{height}}{character}

\item{\code{calibrated}}{logical - detect if sensor is calibrated}
}}

\seealso{
\link{myClim-package}, \link{mc_LoggerMetadata}, \link{mc_data_sensors}
}
