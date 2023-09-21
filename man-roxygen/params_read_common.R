#' @param clean if TRUE, then [mc_prep_clean] is called automatically while reading (default TRUE)
#' @param silent if TRUE, then any information is not printed in console (default FALSE)
#' @param user_data_formats custom data formats; use in case you have your own logger 
#' files not pre-defined in myClim  - list(key=mc_DataFormat) [mc_DataFormat-class] (default NULL)
#'
#' If custom data format is defined the key can be used in data_format parameter in [mc_read_files()] 
#' and [mc_read_data()]. Custom data format must be defined first, and then an be used for reading.
