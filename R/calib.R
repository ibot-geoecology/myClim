#' Default ref. temperate for TMS moisture calibration
#' @description
#' `r mc_const_CALIB_MOIST_REF_T`°C = default reference calibration temperate for TMS moisture sensor
#' @export
mc_const_CALIB_MOIST_REF_T <- 24
#' Default temperature drift for TMS moisture in the air.
#' @description
#' `r sprintf("%.14f", mc_const_CALIB_MOIST_ACOR_T)` = default temperature drift correction parameter in the air -
#' TMS moisture sensor. This constant is used in the function [myClim::mc_calc_vwc].
#' @export
mc_const_CALIB_MOIST_ACOR_T <- 1.91132689118083
#' Default temperature drift for TMS moisture in the water
#' @description
#' `r mc_const_CALIB_MOIST_WCOR_T` = default temperature drift correction parameter in the water -
#' TMS moisture sensor. This constant is used in the function [myClim::mc_calc_vwc].
#' @export
mc_const_CALIB_MOIST_WCOR_T <- 0.64108
#' Calculates coefficients for TMS moisture conversion to VWC
#'
#' @description
#' Specialized function for calibration of TOMST TMS moisture sensor.
#' Function calculate correction parameters for individual logger (slope and intercept) 
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
#' @param raw_air Raw TMS moisture signal in air
#' @param raw_water Raw TMS moisture signal in water
#' @param t_air temperature of air (default 24)
#' @param t_water temperature of water (default 24)
#' @param ref_air (default 114.534) 
#' @param ref_water (default 3634.723)
#' @param ref_t (default 24)
#' @param acor_t temperature drift correction parameter in the air (default 1.91132689118083)
#' @param wcor_t temperature drift correction parameter in the water (default 0.64108)
#' @return list with slope and intercept parameters
#' @export
#' @examples
#' mc_calib_moisture(120, 3650)
mc_calib_moisture <- function(raw_air, raw_water,
                              t_air=24, t_water=24,
                              ref_air=114.534, ref_water=3634.723,
                              ref_t=mc_const_CALIB_MOIST_REF_T,
                              acor_t=mc_const_CALIB_MOIST_ACOR_T,
                              wcor_t=mc_const_CALIB_MOIST_WCOR_T) {
    norm_period_air <- raw_air + (ref_t - t_air) * acor_t
    norm_period_water <- raw_water + (ref_t - t_water) * wcor_t
    intercept <- ref_air - norm_period_air
    slope <- (ref_water - norm_period_water) - intercept
    list(slope = slope, intercept = intercept)
}
