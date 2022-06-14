#' Joining sensors from different loggers
#'
#' @description
#' Function join sensors from different loggers. Loggers with same type [mc_LoggerMetadata] and sensors with same height
#' are joined.
#'
#' @details
#'
#' @param data myClim object in Prep-format. See [myClim-package]
#' @return myClim object with joined sensors.
#' @export
#' @examples
mc_join <- function(data) {
    myClim:::.common_stop_if_not_prep_format(data)
    myClim:::.prep_warn_if_datetime_step_unprocessed(data)
    data
}
