% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/info.R
\name{mc_info_logger}
\alias{mc_info_logger}
\title{Get loggers info table}
\usage{
mc_info_logger(data)
}
\arguments{
\item{data}{myClim object in Raw-format. see \link{myClim-package}}
}
\value{
A data.frame with the following columns:
\itemize{
\item locality_id - If provided by the user, it represents the locality ID; if not provided, it is identical to the logger's serial number.
\item logger_name - Logger name.
\item serial_number - Serial number of the logger, either provided by the user or automatically detected from the file name or header.
\item logger_type - Logger type.
\item start_date - The oldest record on the logger.
\item end_date - The newest record on the logger.
\item step_seconds - Time step of the record series (in seconds).
}
}
\description{
This function returns a data.frame with information about loggers.
}
\details{
This function is designed to work only with
myClim objects in \strong{Raw-format}, where the loggers are organized at localities.
In \strong{Agg-format}, myClim objects do not support loggers; sensors are directly connected to the locality.
See \link{myClim-package}. \code{mc_info_logger} does not work in Agg-format.
}
\examples{
mc_info_logger(mc_data_example_raw)
}
