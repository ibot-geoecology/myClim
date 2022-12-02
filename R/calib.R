.calib_MOIST_REF_T <- 24
.calib_MOIST_ACOR_T <- 1.91132689118083
.calib_MOIST_WCOR_T <- 0.64108

#' Calculates coefficients for TMS moisture conversion to VWC
#'
#' @description
#' Specialized, service function. You will typically not need to use this one. 
#' Function calculate correction parameters (slope and intercept) 
#' from TMS moisture measurements in pure water and air.
#'
#' @details
#' This is highly specialized service function designed to derive correction 
#' parameters `slope` and `intercept` for soil moisture sensor of TMS loggers 
#' measuring on the air and in the water. Slope and intercept calculated in this 
#' function could be used as sensor-specific alternative in [myClim::mc_calc_vwc()] 
#' function instead of defaults. User is not allowed to modify slope and 
#' intercept parameters when calling [myClim::mc_calc_vwc()]. 
#' But this must be done in code of the function. It is possible, but advanced, 
#' and typically not necessary, the default values should be working well.     
#'
#' @param raw_air Raw TMS moisture records in air
#' @param raw_water Raw TMS moisture records in water
#' @param t_air temperature of air (default 24)
#' @param t_water temperature of water (default 24)
#' @param ref_air (default 114.534) 
#' @param ref_water (default 3634.723)
#' @param ref_t (default 24)
#' @param acor_t (default 1.91132689118083)
#' @param wcor_t (default 0.64108)
#' @return list with slope and intercept parameters
#' @export
#' @examples
#' mc_calib_moisture(120, 3650)
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
