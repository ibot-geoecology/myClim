#' Raw data reading
#'
#' This function read raw data of loger and return it in srdard format.
#'
#' @param filename path to source file
#' @param serial_number serial number of logger
#' @param id_locality id of locality
#' @param data_format definition of format source file
#' @param tz time zone of dates in source data
#' @return data in standard format
#' @export
prepare.read <- function(filename, serial_number, id_locality, data_format, tz="UTC") {
  data_table <- read.table(filename, sep=data_format@separator, header=data_format@header)
}
