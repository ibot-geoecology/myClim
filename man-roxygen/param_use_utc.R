#' @param use_utc if FALSE, then the time shift from `tz_offset` metadata is used to correct 
#' (shift) the output time-series (default TRUE)
#'
#' In the Agg-format myClim object `use_utc = FALSE` is allowed only for steps shorter than one day. In myClim 
#' the day nd longer time steps are defined by the midnight, but this represent whole day, week, month, year...
#' shifting daily, weekly, monthly... data (shift midnight) does not make sense in our opinion. 
#' But when user need more flexibility, then myClim Raw-format
#' can be used, In Raw-format `use_utc` is not limited, user can shift an data without the restrictions. 
#' See [myClim-package]
