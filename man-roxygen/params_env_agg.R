#' @template param_myClim_object_cleaned
#' @param period output period see [myClim::mc_agg()]
#' @param use_utc if FALSE, then local time is used for day aggregation see [myClim::mc_agg()] (default TRUE)
#' @param custom_start start date for custom period see [myClim::mc_agg()] (default NULL)
#' @param custom_end end date for custom period see [myClim::mc_agg()] (default NULL)
#' @param min_coverage §parameter for mc_agg see [myClim::mc_agg()] (default 1)§
