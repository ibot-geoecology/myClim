% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{mc_data_formats}
\alias{mc_data_formats}
\title{Formats of source data files}
\format{
An object of class \code{environment} of length 3.
}
\usage{
mc_data_formats
}
\description{
R object of class environment with the definitions how to
parse specific microclimatic logger files. In case you would like to add
new, unsupported logger, this is the place where the reading key is stored.
}
\details{
Package myClim support formats TOMST, TOMST_join and HOBO.
The environment object is stored in \code{./data/mc_data_formats.rda}.

\strong{TOMST}

TOMST data format has stable structure. Datetime is in UTC. Name of data file is in format data_\<serial_number\>_\<x\>.csv.
Value serial_number can be automatically detected from file name. Supported loggers are TMS and ThermoDataLogger.

\strong{TOMST_join}

TOMST_join data format is custom format for internal using of Institute of Botany of the Czech Academy of Sciences.
It is the output of joinTMS.exe modified, checked, curated and validated by Lucia.

\strong{HOBO}

HOBO data format is export format from software HOBOware of Onset company. Format is very variable
and can be adjusted by user in preferences of HOBOware. Strucuture of HOBO files format can be partly detected automatically from header of data.
Except of  format of date-time (\code{date_format}) which must be set manually in myClim reading functions (\code{\link[=mc_read_files]{mc_read_files()}}, \code{\link[=mc_read_data]{mc_read_data()}}).
Date and time separated in more columns is not supported in myClim reading. If time zone is not defined in header of HOBO txt or csv file
and is not UTC, then \code{tz_offset} must be filled in while reading. UTF-8 encoding of HOBO file is required for reding to myClim.
}
\seealso{
\link{mc_DataFormat}, \linkS4class{mc_TOMSTDataFormat}, \linkS4class{mc_TOMSTJoinDataFormat}, \linkS4class{mc_HOBODataFormat}
}
\keyword{datasets}
