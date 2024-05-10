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
#' from TMS moisture measurements in demineralized water and dry air.
#'
#' @details
#' This function calculate calibration parameters `cor_factor` and `cor_intercept`
#' accounting for individual differencies in TMS moisture sensor signal in air and in water against reference
#' loggers which were used for estimation of parameters of soil VWC conversion curves.
#' These parameters must be loaded into myClim object [myClim::mc_prep_calib_load()]
#' prior to calling [myClim::mc_calc_vwc()].
#' Parameters for soils available in my_Clim were derived for TMS3 logger version, with slightly different typical air and water signal.
#' Correction parameters for TMS4 loggers therefore can be expected in the range of values:
#' cor_factor = (-150; -450) and cor_slope = (100, 450)
#'
#'
#' @param raw_air Raw TMS moisture signal in air
#' @param raw_water Raw TMS moisture signal in water
#' @param t_air temperature of air (default 24)
#' @param t_water temperature of water (default 24)
#' @param ref_air raw air signal of reference logger used to derive soil calibration parameters (default 114.534)
#' @param ref_water raw air signal of reference logger used to derive soil calibration parameters (default 3634.723)
#' @param ref_t reference logger temperature (default 24)
#' @param acor_t temperature drift correction parameter in the air (default 1.911)
#' @param wcor_t temperature drift correction parameter in the water (default 0.641)
#' @return list with correction factor and correction slope
#' @export
#' @examples
#' # load example data
#' files <- c(system.file("extdata", "data_94184102_0.csv", package = "myClim"))
#' tomst_data <- mc_read_files(files, "TOMST")
#'
#' # vwc without calibration
#' tomst_data <- mc_calc_vwc(tomst_data, soiltype = "universal", output_sensor = "VWC_universal")
#'
#' # load calibration
#' my_cor <- mc_calib_moisture(raw_air = 394, raw_water = 3728, t_air = 21, t_water = 20)
#' my_calib_tb <- data.frame(serial_number = c("94184102"), sensor_id = "TMS_moist",
#'                           datetime = as.POSIXct("2020-01-01 00:00"),
#'                           cor_factor = my_cor$cor_factor, cor_slope = my_cor$cor_slope)
#' tomst_data_cal <- mc_prep_calib_load(tomst_data, my_calib_tb)
#' # vwc using calibration
#' tomst_data_cal <- mc_calc_vwc(tomst_data_cal, soiltype = "universal",
#'                               output_sensor = "VWC_universal_calib")
#' # plot results
#' # sensors <- mc_info(tomst_data_cal)$sensor_name
#' # (mc_plot_line(tomst_data_cal, sensors = c(sensors[startsWith(sensors,"VWC")]))
#' #     + ggplot2::scale_color_viridis_d(begin = 0.2, end = 0.8))}

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
    return(list(cor_factor = intercept, cor_slope = slope))
}
