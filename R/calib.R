.calib_MOIST_REF_T <- 24
.calib_MOIST_ACOR_T <- 1.91132689118083
.calib_MOIST_WCOR_T <- 0.64108

#' Calculation slope and intercept parameters of TMSmoisture sensor
#'
#' @description
#' Function calculate slope and intercept parameters from measuring moisture in water and air.
#'
#' @details
#'
#' @param raw_air TDT signal in air
#' @param raw_water TDT signal in water
#' @param t_air temperature of air (default 24)
#' @param t_water temperature of air (default 24)
#' @param ref_air (default 114.534)
#' @param ref_water (default 3634.723)
#' @param ref_t (default 24)
#' @param acor_t (default 1.91132689118083)
#' @param wcor_t (default 0.64108)
#' @return list with slope and intercept parameters
#' @export
#' @examples
mc_calib_moisture <- function(raw_air, raw_water,
                              t_air=24, t_water=24,
                              ref_air=114.534, ref_water=3634.723,
                              ref_t=.calib_MOIST_REF_T,
                              acor_t=.calib_MOIST_ACOR_T,
                              wcor_t=.calib_MOIST_WCOR_T) {
    norm_period_air <- raw_air + (ref_t - t_air) * acor_t
    norm_period_water <- raw_water + (ref_t - t_water) * wcor_t
    intercept <- ref_air - norm_period_air
    slope <- (ref_water - norm_period_water) - intercept
    list(slope = slope, intercept = intercept)
}
