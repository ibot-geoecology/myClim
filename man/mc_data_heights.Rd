% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{mc_data_heights}
\alias{mc_data_heights}
\title{Default heights of sensors}
\format{
An object of class \code{data.frame} with 13 rows and 4 columns.
}
\usage{
mc_data_heights
}
\description{
This table is used to set the default heights in metadata of sensors based on logger type.
The defaults were set based on the most common uses,
defaults can be overwrite be user. see \link{mc_prep_meta_sensor}
}
\details{
data.frame with columns:
\itemize{
\item logger_type
\item sensor_name
\item height - character representation of height
\item suffix - suffix for sensor_name. If suffix is NA, then sensor_name is not modified.
}

Default heights are:
\itemize{
\item TS_T = air 200 cm
\item TMS_T1 = soil 8 cm
\item TMS_T2 = air 2 cm
\item TMS_T3 = air 15 cm
\item TMS_TMSmoisture = soil 0-15 cm
\item DEND_T = 130 cm
\item DEND_TOMSTdendro = 130 cm
\item HOBO_T_C = air 150 cm
\item HOBO_RH = air 150 cm
\item TMS_T1 = soil 40 cm
\item TMS_T2 = soil 30 cm
\item TMS_T3 = air 15 cm
\item TMS_TMSmoisture = soil 30-44 cm
}
}
\seealso{
\code{\link[=mc_read_files]{mc_read_files()}}, \code{\link[=mc_read_data]{mc_read_data()}}
}
\keyword{datasets}
