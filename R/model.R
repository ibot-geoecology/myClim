# constants ================================================================================

.model_const_TZ_UTC <- "UTC"
.model_const_TZ_SOLAR <- "solar"
.model_const_TZ_USER_DEFINED <- "user defined"

.model_const_COUNT_TEST_VALUES <- 100
.model_const_EDITABLE_LOCALITY_METADATA_PARAMETERS <- c("locality_id", "elevation", "lat_wgs84", "lon_wgs84", "tz_offset")
.model_const_EDITABLE_SENSOR_METADATA_PARAMETERS <- c("name", "height")
.model_const_TOMST_THERMODATALOGGER_VALUE <- 65336
.model_const_TOMST_DENDROMETER_UM_RANGE <- 8890

.model_const_PHYSICAL_l_cm <- "l_cm"
.model_const_PHYSICAL_l_mm <- "l_mm"
.model_const_PHYSICAL_l_um <- "l_um"
.model_const_PHYSICAL_moisture <- "moisture"
.model_const_PHYSICAL_p_kPa <- "p_kPa"
.model_const_PHYSICAL_RH_perc <- "RH_perc"
.model_const_PHYSICAL_T_C <- "T_C"
.model_const_PHYSICAL_T_F <- "T_F"
.model_const_PHYSICAL_t_h <- "t_h"
.model_const_PHYSICAL_TMSmoisture <- "TMSmoisture"
.model_const_PHYSICAL_TOMSTdendro <- "TOMSTdendro"
.model_const_PHYSICAL_v <- "v"

.model_const_VALUE_TYPE_REAL <- "real"
.model_const_VALUE_TYPE_INTEGER <- "integer"
.model_const_VALUE_TYPE_LOGICAL <- "logical"

# logger sensors
.model_const_SENSOR_TMS_T1 <- "TMS_T1"
.model_const_SENSOR_TMS_T2 <- "TMS_T2"
.model_const_SENSOR_TMS_T3 <- "TMS_T3"
.model_const_SENSOR_TMS_TMSmoisture <- "TMS_TMSmoisture"
.model_const_SENSOR_TS_T <- "TS_T"
.model_const_SENSOR_DEND_T <- "DEND_T"
.model_const_SENSOR_DEND_TOMSTdendro <- "DEND_TOMSTdendro"
.model_const_SENSOR_HOBO_T_C <- "HOBO_T_C"
.model_const_SENSOR_HOBO_T_F <- "HOBO_T_F"
.model_const_SENSOR_HOBO_RH <- "HOBO_RH"

.model_const_WRONG_CALIBRATION_SENSOR_ID <- .model_const_SENSOR_TMS_TMSmoisture

# universal sensors
.model_const_SENSOR_count <- "count"
.model_const_SENSOR_coverage <- "coverage"
.model_const_SENSOR_FDD <- "FDD"
.model_const_SENSOR_GDD <- "GDD"
.model_const_SENSOR_precipitation <- "precipitation"
.model_const_SENSOR_dendro_l_um <- "dendro_l_um"
.model_const_SENSOR_snow_bool <- "snow_bool"
.model_const_SENSOR_snow_fresh <- "snow_fresh"
.model_const_SENSOR_snow_total <- "snow_total"
.model_const_SENSOR_sun_shine <- "sun_shine"
.model_const_SENSOR_VPD <- "VPD"
.model_const_SENSOR_wind <- "wind"

.model_const_SENSOR_moisture <- .model_const_PHYSICAL_moisture
.model_const_SENSOR_RH_perc <- .model_const_PHYSICAL_RH_perc
.model_const_SENSOR_T_C <- .model_const_PHYSICAL_T_C

.model_const_SENSOR_real <- .model_const_VALUE_TYPE_REAL
.model_const_SENSOR_integer <- .model_const_VALUE_TYPE_INTEGER
.model_const_SENSOR_logical <- .model_const_VALUE_TYPE_LOGICAL

.model_const_LOGGER_TOMST_TMS <- "TMS"
.model_const_LOGGER_TOMST_TMS_L45 <- "TMS_L45"
.model_const_LOGGER_TOMST_THERMODATALOGGER <- "ThermoDatalogger"
.model_const_LOGGER_TOMST_DENDROMETER <- "Dendrometer"
.model_const_LOGGER_HOBO <- "HOBO"

.model_const_DATA_FORMAT_TOMST <- "TOMST"
.model_const_DATA_FORMAT_TOMST_join <- "TOMST_join"
.model_const_DATA_FORMAT_HOBO <- "HOBO"

.model_const_SENSOR_STATE_SOURCE <- "source"
.model_const_SENSOR_STATE_ERROR <- "error"

.model_const_MESSAGE_NO_DATA <- "There aren't any data in source file."
.model_const_MESSAGE_SEPARATED_TIME <- "Separated time in source data isn't supported."
.model_const_MESSAGE_DATE_TIME_HEADER <- "It is not possible to detect timezone offset from header."
.model_const_MESSAGE_COLUMNS_PROBLEM <- "It is not possible to detect columns from header."
.model_const_MESSAGE_HOBO_DATE_FORMAT_PROBLEM <- "HOBO data format required filled in parameter date_format."

.model_const_FORMAT_RAW <- "raw"
.model_const_FORMAT_AGG <- "agg"

#' Custom list for myClim object
#'
#' Top level list for store myClim data. (see [myClim-package]) Rather service function used
#' for checking, whether object is myClimList. The same time can be used to create standard
#' R list from myClimList.
#'
#' @param metadata of data object
#' @param localities list of licalities
#' @return the list containing myClim object’s metadata and localities
#' @export
myClimList <- function(metadata=NULL, localities=list())
    structure(list(metadata=metadata, localities=localities), class=c("myClimList", "list"))

# classes ================================================================================

mc_Serializable <- setClass("mc_Serializable")

setGeneric(
    ".model_object_to_list",
    function(object){
        standardGeneric(".model_object_to_list")
    }
)

setMethod(
    ".model_object_to_list",
    "mc_Serializable",
    function(object) {
        slot_names <- slotNames(object)
        result <- purrr::map(slot_names, ~ attr(object, .x))
        names(result) <- slot_names
        result$class <- class(object)[[1]]
        return(result)
    }
)

.model_list_to_object <- function(obj_list) {
        object <- new(obj_list$class)
        slot_names <- slotNames(object)
        for(slot_name in slot_names)
        {
            if(!(slot_name %in% names(obj_list))) next
            attr(object, slot_name) <- obj_list[[slot_name]]
        }
        return(object)
}

#' Class for sensor definition
#' 
#' Sensor definitions are stored in [mc_data_sensors].
#' @slot sensor_id unique identifier of sensor (TMS_T1, TMS_T2, TMS_T3, TMS_TMSmoisture, ...)
#' @slot logger name of logger (TMS, ThermoDatalogger, ...)
#' @slot physical unit of sensor (T_C, TMSmoisture, moisture, RH_perc) (default NA)
#' @slot description character info
#' @slot value_type type of values (real, integer, logical) (default real)
#' @slot min_value minimal value (default NA)
#' @slot max_value maximal value (default NA)
#' @slot plot_color color in pot (default "")
#' @slot plot_line_width width of line in plot (default 1)
#' @exportClass mc_Sensor
#' @seealso [mc_data_sensors]
mc_Sensor <- setClass("mc_Sensor",
                      slots = c(sensor_id = "character",
                                logger = "character",
                                physical = "character",
                                description = "character",
                                value_type = "character",
                                min_value = "numeric",
                                max_value = "numeric",
                                plot_color = "character",
                                plot_line_width = "numeric"))

setMethod(f="initialize",
          signature="mc_Sensor",
          definition=function(.Object) {
              .Object@logger <- NA_character_
              .Object@physical <- NA_character_
              .Object@value_type <- .model_const_VALUE_TYPE_REAL
              .Object@min_value <- NA_real_
              .Object@max_value <- NA_real_
              .Object@plot_color <- NA_character_
              .Object@plot_line_width <- 1
              return(.Object)
          })

#' Class for physical
#' 
#' Class defining the element of the records (temperature, volumetric water content, height...)
#' @details See e.g. definition of temperature. Similarly as the definition of new loggers, new
#' physicals cn be added like modules. 
#' \preformatted{
#' Slot "name": "T_C"
#' slot "description": "Temperature °C"
#' Slot "units": "°C"
#' Slot "viridis_color_map": "C"
#' Slot "scale_coeff": 0.03333333
#' }
#' @slot name of physical
#' @slot description character info
#' @slot units measurument (°C, %, m3/m3, raw, mm, ...)
#' @slot viridis_color_map viridis color map option
#' @slot scale_coeff coefficient for plot; value * scale_coef is in range 0-1
#' @exportClass mc_Physical
#' @seealso [mc_data_physical] 
mc_Physical <- setClass("mc_Physical",
                        slots = c(
                            name = "character",
                            description = "character",
                            units = "character",
                            viridis_color_map = "character",
                            scale_coeff = "numeric"))

setMethod("initialize",
          "mc_Physical",
          function(.Object) {
              .Object@scale_coeff <- 1
              return(.Object)
          })

#' Class for myClim object metadata
#' @template slot_MainMetadata
#' @exportClass mc_MainMetadata
#' @seealso [myClim-package]
mc_MainMetadata <- setClass("mc_MainMetadata",
                            slots = c(version = "ANY",
                                      format_type = "character"),
                            contains = "mc_Serializable")

setMethod("initialize",
          "mc_MainMetadata",
          function(.Object) {
              .Object@version <- packageVersion("myClim")
              .Object@format_type <- .model_const_FORMAT_RAW
              return(.Object)
          })

#' Class for myClim object metadata in Agg-format
#' @template slot_MainMetadata
#' @slot step time step of data in seconds
#' @slot period value from [mc_agg()] (e.g. month, day, all...)
#' @slot intervals_start start datetime of data intervals for spacial periods all and custom (see [mc_agg()])
#' @slot intervals_end end datetime of data intervals for spacial periods all and custom (see [mc_agg()])
#' @exportClass mc_MainMetadataAgg
#' @seealso [mc_MainMetadata] [myClim-package]
mc_MainMetadataAgg <- setClass("mc_MainMetadataAgg",
                            slots = c(step = "numeric",
                                      period = "character",
                                      intervals_start = "POSIXct",
                                      intervals_end = "POSIXct"),
                            contains = "mc_MainMetadata")

setMethod("initialize",
          "mc_MainMetadataAgg",
          function(.Object) {
              .Object@version <- packageVersion("myClim")
              .Object@format_type <- .model_const_FORMAT_AGG
              .Object@intervals_start <- lubridate::NA_POSIXct_
              .Object@intervals_end <- lubridate::NA_POSIXct_
              return(.Object)
          })

#' Class for locality metadata
#' @details When reading without metadata, then locality is named after file
#' where the data come from, or after the sensor id where the data come form.  
#' @slot locality_id name of locality
#' @slot elevation of locality
#' @slot lat_wgs84 latitude of locality in WGS-84
#' @slot lon_wgs84 longitude of locality in WGS-84
#' @slot tz_offset offset from UTC in minutes
#' @slot tz_type type of time zone
#' @slot user_data list for user data
#' @exportClass mc_LocalityMetadata
#' @seealso [myClim-package], [mc_LoggerMetadata], [mc_SensorMetadata]
mc_LocalityMetadata <- setClass("mc_LocalityMetadata",
                                slots = c(locality_id = "character",
                                          elevation = "numeric",
                                          lat_wgs84 = "numeric",
                                          lon_wgs84 = "numeric",
                                          tz_offset = "numeric",
                                          tz_type = "character",
                                          user_data = "list"),
                                contains = "mc_Serializable")

setMethod("initialize",
          "mc_LocalityMetadata",
          function(.Object) {
              .Object@elevation <- NA_real_
              .Object@lat_wgs84 <- NA_real_
              .Object@lon_wgs84 <- NA_real_
              .Object@tz_offset <- NA_integer_
              .Object@tz_type <- .model_const_TZ_UTC
              .Object@user_data <- list()
              return(.Object)
          })

#' Class for logger metadata
#' @slot type of logger (TMS, ThermoDatalogger, Dendrometer, HOBO)
#' @slot serial_number serial number of the logger 
#' @slot step time step of microclimatic time-seris in seconds.
#' When provided by user, is used in [mc_prep_clean()] function instead of
#' automatic step detection
#' @exportClass mc_LoggerMetadata
mc_LoggerMetadata <- setClass("mc_LoggerMetadata",
                              slots = c(type = "character",
                                        serial_number = "character",
                                        step = "numeric"),
                              contains = "mc_Serializable")

setMethod("initialize",
          "mc_LoggerMetadata",
          function(.Object) {
              .Object@step <- NA_integer_
              return(.Object)
          })

#' Class for logger clean info
#' @slot step Time step of microclimatic data series in seconds
#' @slot count_duplicities count of duplicated records - values with same date
#' @slot count_missing count of missing records; Period between the records should be the same length. If not, than missing.
#' @slot count_disordered count of records incorrectly ordered in time. In table, newer record is followed by the older. 
#' @slot rounded T/F indication whether myClim automatically rounded time series to the closes half (HH:00, HH:30) e.g. 13:07 -> 13:00
#' @exportClass mc_LoggerCleanInfo
mc_LoggerCleanInfo <- setClass("mc_LoggerCleanInfo",
                               slots = c(step = "numeric",
                                         count_duplicities = "numeric",
                                         count_missing = "numeric",
                                         count_disordered = "numeric",
                                         rounded = "logical"),
                               contains = "mc_Serializable")

setMethod("initialize",
          "mc_LoggerCleanInfo",
          function(.Object) {
              .Object@step <- NA_integer_
              .Object@count_duplicities <- NA_integer_
              .Object@count_missing <- NA_integer_
              .Object@count_disordered <- NA_integer_
              .Object@rounded <- FALSE
              return(.Object)
          })

#' Class for sensor metadata
#' @details `sensor_id` must be one of the defined id in myClim. see [mc_data_sensors]. 
#' It is useful to select on of predefined, because it makes plotting and calculaton easier. 
#' Through `sensor_id` myClim assign pre-deined physicyl units or plotting colors see [mc_Sensor].  
#' @slot sensor_id unique identifier of sensor (TMS_T1, TMS_T2, TMS_T3, TMS_TMSmoisture, ...) [mc_data_sensors] e.g. TMS_T1, TMS_TMSmoisture, snow_fresh...
#' @slot name character, could be same as `sensor_id` but also defined by function or user.  
#' @slot height character
#' @slot calibrated logical - detect if sensor is calibrated
#' @exportClass mc_SensorMetadata
#' @seealso [myClim-package], [mc_LoggerMetadata], [mc_data_sensors]
mc_SensorMetadata <- setClass("mc_SensorMetadata",
                              slots = c(sensor_id = "character",
                                        name = "character",
                                        height = "character",
                                        calibrated = "logical"),
                              contains = "mc_Serializable")

setMethod("initialize",
          "mc_SensorMetadata",
          function(.Object) {
              .Object@height <- NA_character_
              .Object@calibrated <- FALSE
              return(.Object)
          })

setGeneric(
    ".model_is_physical",
    function(object, physical){
        standardGeneric(".model_is_physical")
    }
)

setMethod(
    ".model_is_physical",
    "mc_SensorMetadata",
    function(object, physical) {
        physical_id <- myClim::mc_data_sensors[[object@sensor_id]]@physical
        return(!is.na(physical_id) && physical_id == physical)
    }
)

setGeneric(
    ".model_is_physical_TMSmoisture",
    function(object){
        standardGeneric(".model_is_physical_TMSmoisture")
    }
)

setMethod(
    ".model_is_physical_TMSmoisture",
    "mc_SensorMetadata",
    function(object) {
        .model_is_physical(object, .model_const_PHYSICAL_TMSmoisture)
    }
)

setGeneric(
    ".model_is_physical_T_C",
    function(object){
        standardGeneric(".model_is_physical_T_C")
    }
)

setMethod(
    ".model_is_physical_T_C",
    "mc_SensorMetadata",
    function(object) {
        .model_is_physical(object, .model_const_PHYSICAL_T_C)
    }
)

setGeneric(
    ".model_is_type_real",
    function(object){
        standardGeneric(".model_is_type_real")
    }
)

setMethod(
    ".model_is_type_real",
    "mc_SensorMetadata",
    function(object) {
        value_type <- myClim::mc_data_sensors[[object@sensor_id]]@value_type
        return(value_type == .model_const_VALUE_TYPE_REAL)
    }
)

setGeneric(
    ".model_get_physical_description",
    function(object){
        standardGeneric(".model_get_physical_description")
    }
)

setMethod(
    ".model_get_physical_description",
    "mc_SensorMetadata",
    function(object) {
        if(!(object@sensor_id %in% names(myClim::mc_data_sensors))) {
            return(NA_character_)
        }
        sensor_info <- myClim::mc_data_sensors[[object@sensor_id]]
        if(is.na(sensor_info@physical)) {
            return(NA_character_)
        }
        physical <- myClim::mc_data_physical[[sensor_info@physical]]
        physical@description
    }
)

setGeneric(
    ".model_get_sensor_description",
    function(object){
        standardGeneric(".model_get_sensor_description")
    }
)

setMethod(
    ".model_get_sensor_description",
    "mc_SensorMetadata",
    function(object) {
        if(!(object@sensor_id %in% names(myClim::mc_data_sensors))) {
            return(NA_character_)
        }
        sensor_info <- myClim::mc_data_sensors[[object@sensor_id]]
        sensor_info@description
    }
)

#' Class for logger file data format
#'
#' The Class used for parsing source data files. Typically the csv files
#' downloaded from microclimatic loggers. Each supported logger has established
#' own specific object of class `mc_{logger}DataFormat` defining the parameters.
#' @details The logger definitions are stored in R environment object
#' `./data/mc_data_formats.rda`. And thus it easy to add the ability of
#' reading new, unsupported logger just by defining its Class parameters.
#' Below see e.g. the Class defining Tomst file format.
#'  
#' \preformatted{
#' An object of class "mc_TOMSTDataFormat"
#' attr(,"skip"): 0
#' attr(,"separator"): ";"
#' attr(,"date_column"): 2
#' attr(,"date_format"): NA
#' attr(,"na_strings"): "-200"
#' attr(,"columns"): list()
#' attr(,"filename_serial_number_pattern"): "data_(\\d+)_\\d+\\.csv$"
#' attr(,"data_row_pattern"): "^\\d+;[\\d.: ]+;\\d+;-?\\d+[.,]?\\d*;-?\\d+[.,]?\\d*;-?\\d+[.,]?\\d*;\\d+;\\d+;\\d+.*$"
#' attr(,"logger_type"): character(0)
#' }
#' 
#' @slot skip number of lines before data - header etc. (default 0)
#' @slot separator columns separator (default NA)
#' @slot date_column index of date column (default NA)
#' @slot date_format format of date (default NA)
#' @slot na_strings strings for NA values (default NA)
#' @slot error_value value means error of sensor (default NA)
#' @slot columns list with names and indexes of value columns (default list())
#' @slot filename_serial_number_pattern character pattern for detecting serial_number from file name (default NA)
#' @slot data_row_pattern character pattern for detecting right file format (default NA)
#'
#' If data_row_pattern is NA, then file format is not validated.
#' @slot logger_type type of logger: TMS, TMS_L45, ThermoDatalogger, Dendrometer, HOBO, ... (default NA)
#' @slot tz_offset timezone offset in minutes from UTC in source data (default 0)
#' @exportClass mc_DataFormat
#' @seealso [mc_data_formats], [mc_TOMSTDataFormat-class], [mc_TOMSTJoinDataFormat-class], [mc_HOBODataFormat-class]
mc_DataFormat <- setClass("mc_DataFormat",
                          slots = c(skip = "numeric",
                                    separator = "character",
                                    date_column = "numeric",
                                    date_format = "character",
                                    na_strings = "character",
                                    error_value = "numeric",
                                    columns = "list",
                                    filename_serial_number_pattern = "character",
                                    data_row_pattern = "character",
                                    logger_type = "character",
                                    tz_offset = "numeric"))

setMethod("initialize",
          "mc_DataFormat",
          function(.Object) {
              .Object@skip <- 0
              .Object@separator <- NA_character_
              .Object@date_column <- NA_integer_
              .Object@date_format <- NA_character_
              .Object@na_strings <- NA_character_
              .Object@error_value <- NA_integer_
              .Object@columns <- list()
              .Object@filename_serial_number_pattern <- NA_character_
              .Object@data_row_pattern <- NA_character_
              .Object@logger_type <- NA_character_
              .Object@tz_offset <- NA_integer_
              return(.Object)
          })

#' Class for reading Tomst logger files
#' 
#' Provides the key for the column in source files. Where is the date, 
#' in what format is the date, in which columns are records of which sensors.
#' The code defining the class is in section methods ./R/model.R  
#' 
#' @seealso [myClim::mc_DataFormat], [mc_data_formats], [mc_TOMSTJoinDataFormat-class]
#' @exportClass mc_TOMSTDataFormat
mc_TOMSTDataFormat <- setClass("mc_TOMSTDataFormat",
                               contains = "mc_DataFormat")

#' Class for reading TMS join files
#' 
#' Provides the key for the column in source files. Where is the date, 
#' in what format is the date, in which columns are records of which sensors.
#' The code defining the class is in section methods ./R/model.R
#' 
#' TMS join file format is the output of IBOT internal post-processing of Tomst logger files.
#' @seealso [myClim::mc_DataFormat],[mc_data_formats],[mc_TOMSTDataFormat-class], [mc_TOMSTJoinDataFormat-class]
#' @exportClass mc_TOMSTJoinDataFormat
mc_TOMSTJoinDataFormat <- setClass("mc_TOMSTJoinDataFormat", contains = "mc_DataFormat")

#' Class for reading HOBO logger files
#'
#' Provides the key for reading the HOBO source files. In which column is the date,
#' in what format is the date, in which columns are records of which sensors.
#' The code defining the class is in section methods ./R/model.R
#'
#' @seealso [myClim::mc_DataFormat], [mc_data_formats]
#' @exportClass mc_HOBODataFormat
mc_HOBODataFormat <- setClass("mc_HOBODataFormat", contains = "mc_DataFormat")

# generics ================================================================================

setGeneric(
    ".model_load_data_format_params_from_file",
    function(object, path){
        standardGeneric(".model_load_data_format_params_from_file")
    }
)

setGeneric(
  ".model_get_serial_number_from_file",
  function(object, path){
    standardGeneric(".model_get_serial_number_from_file")
  }
)

setGeneric(
    ".model_edit_data",
    function(object, data_table){
        standardGeneric(".model_edit_data")
    }
)

# methods ================================================================================

setMethod(
    ".model_load_data_format_params_from_file",
    "mc_DataFormat",
    function(object, path) {
        if(!.model_is_file_in_right_format(object, path)) {
            return(NULL)
        }
        object
    }
)

.model_is_file_in_right_format <- function(object, path) {
    if(is.na(object@data_row_pattern)) {
        return(TRUE)
    }
    con <- file(path, "r")
    skip <- object@skip
    while (TRUE) {
        line <- readLines(con, n = 1)
        if ( length(line) == 0 ) {
            close(con)
            return(FALSE)
        }
        if(skip > 0) {
            skip <- skip - 1
            next
        }
        close(con)
        return(stringr::str_detect(line, object@data_row_pattern))
    }
}

setMethod(
    ".model_load_data_format_params_from_file",
    "mc_TOMSTDataFormat",
    function(object, path) {
        if(!.model_is_file_in_right_format(object, path)) {
            return(NULL)
        }
        data <- .read_get_data_from_file(path, object, nrows = .model_const_COUNT_TEST_VALUES)
        object@date_format <- .get_tomst_datetime_format(data, object@date_column)
        .change_tomst_columns_and_logger_type(object, data)
    }
)

.get_tomst_datetime_format <- function(data, date_column){
    if(stringr::str_detect(data[1, date_column], "\\d{4}\\.\\d{1,2}\\.\\d{1,2} \\d{1,2}:\\d{2}"))
    {
        return("%Y.%m.%d %H:%M")
    }
    if(stringr::str_detect(data[1, date_column], "\\d{1,2}\\.\\d{1,2}\\.\\d{4} \\d{1,2}:\\d{2}"))
    {
        return("%d.%m.%Y %H:%M")
    }
    return(NA_character_)
}

.change_tomst_columns_and_logger_type <- function(object, data){
    tm_columns <- list(4)
    names(tm_columns) <- .model_const_SENSOR_TS_T
    dendro_columns <- list(4, 7)
    names(dendro_columns) <- c(.model_const_SENSOR_DEND_T, .model_const_SENSOR_DEND_TOMSTdendro)
    tms_columns <- list(4, 5, 6, 7)
    names(tms_columns) <- c(.model_const_SENSOR_TMS_T1, .model_const_SENSOR_TMS_T2, .model_const_SENSOR_TMS_T3,
                            .model_const_SENSOR_TMS_TMSmoisture)
    if(all(is.na(data[[tms_columns[[.model_const_SENSOR_TMS_T2]]]]))) {
        if(all(data[[dendro_columns[[.model_const_SENSOR_DEND_TOMSTdendro]]]] == .model_const_TOMST_THERMODATALOGGER_VALUE)) {
            object@columns <- tm_columns
            logger_type <- .model_const_LOGGER_TOMST_THERMODATALOGGER
        } else {
            object@columns <- dendro_columns
            logger_type <- .model_const_LOGGER_TOMST_DENDROMETER
        }
    } else {
        object@columns <- tms_columns
        logger_type <- .model_const_LOGGER_TOMST_TMS
    }
    if(is.na(object@logger_type)) {
        object@logger_type <- logger_type
    }
    object
}

setMethod(
    ".model_load_data_format_params_from_file",
    "mc_TOMSTJoinDataFormat",
    function(object, path) {
        if(!.model_is_file_in_right_format(object, path)) {
            return(NULL)
        }
        data <- .read_get_data_from_file(path, object, nrows = .model_const_COUNT_TEST_VALUES)
        .change_tomst_join_columns_and_logger_type(object, data)
    }
)

.change_tomst_join_columns_and_logger_type <- function(object, data){
    tmj_columns <- list(5)
    names(tmj_columns) <- .model_const_SENSOR_TS_T
    tmsj_columns <- list(5, 6, 7, 8, 9)
    names(tmsj_columns) <- c(.model_const_SENSOR_TMS_T1, .model_const_SENSOR_TMS_T2,.model_const_SENSOR_TMS_T3,
                             .model_const_SENSOR_TMS_TMSmoisture, .model_const_SENSOR_moisture)
    is_T1_NA <- all(is.na(data[[tmsj_columns[[.model_const_SENSOR_TMS_T1]]]]))
    is_NA_T2_T3 <- all(is.na(data[[tmsj_columns[[.model_const_SENSOR_TMS_T2]]]])) &&
        all(is.na(data[[tmsj_columns[[.model_const_SENSOR_TMS_T3]]]]))
    is_T1_T2_T3_equals <- (all(data[[tmsj_columns[[.model_const_SENSOR_TMS_T1]]]] == data[[tmsj_columns[[.model_const_SENSOR_TMS_T2]]]]) &&
        all(data[[tmsj_columns[[.model_const_SENSOR_TMS_T1]]]] == data[[tmsj_columns[[.model_const_SENSOR_TMS_T3]]]]))
    if(!is_T1_NA && (is_NA_T2_T3 || is_T1_T2_T3_equals)) {
        object@columns <- tmj_columns
        if(is.na(object@logger_type)) {
            object@logger_type <- .model_const_LOGGER_TOMST_THERMODATALOGGER
        }
        return(object)
    }
    if(is.na(object@logger_type)) {
        object@logger_type <- .model_const_LOGGER_TOMST_TMS
    }
    moisture <- data[[tmsj_columns[[.model_const_SENSOR_moisture]]]]
    if(!any(is.na(moisture)) && all(moisture == 0)) {
        object@columns <- within(tmsj_columns, rm(moisture))
        return(object)
    }
    object@columns <- tmsj_columns
    object
}

setMethod(
    ".model_load_data_format_params_from_file",
    "mc_HOBODataFormat",
    function(object, path) {
        count_lines <- 5
        lines <- .model_read_n_lines(path, count_lines)
        object@separator <- .model_hobo_get_separator(lines)
        if(is.na(object@separator)) {
            return(NULL)
        }
        data <- .read_get_data_from_file(path, object, nrows = count_lines)
        data[1, 1] <- stringr::str_trim(data[1, 1])
        object <- .model_hobo_set_skip(object, data)
        has_numbers_column <- data[[1]][[object@skip]] == "#"
        object <- .model_hobo_set_date_column(object, data, has_numbers_column)
        if(is.na(object@date_column)) {
            return(NULL)
        }
        object <- .model_hobo_set_tz_offset(object, data)
        object <- .model_hobo_set_columns(object, data, has_numbers_column)
        if(!.model_is_hobo_format_ok(object)) {
            return(NULL)
        }
        object
    }
)

.model_read_n_lines <- function(filename, count_lines) {
    con <- file(filename, "r")
    lines <- readLines(con, n = count_lines)
    close(con)
    purrr::map_chr(lines, ~ stringr::str_trim(.x))
}

.model_hobo_get_separator <- function(lines) {
    if(length(lines) >= 3) {
        line <- lines[[3]]
    } else if(length(lines) == 2) {
        line <- lines[[2]]
    } else {
        return(NA_character_)
    }
    for(sep in c(";", "\t", ",")) {
        parts <- stringr::str_split(line, sep)[[1]]
        if(length(parts) >= 3) {
            return(sep)
        }
    }
    return(NA_character_)
}

.model_hobo_set_skip <- function(object, data) {
    if(stringr::str_starts(data[[1]][[1]], "Plot Title:")) {
        object@skip <- 2
    } else {
        object@skip <- 1
    }
    object
}

.model_hobo_set_date_column <- function (object, data, has_numbers_column) {
    date_column <- if(has_numbers_column) 2 else 1
    if(data[[date_column]][[object@skip]] == "Date") {
        warning(.model_const_MESSAGE_SEPARATED_TIME)
        return(object)
    }
    object@date_column <- date_column
    object
}

.model_hobo_set_tz_offset <- function (object, data) {
    if(!is.na(object@tz_offset)) {
        return(object)
    }
    if(data[[object@date_column]][[object@skip]] == "Date Time") {
        object@tz_offset <- 0
        return(object)
    }
    parts <- stringr::str_match(data[[object@date_column]][[object@skip]], "Date Time, GMT([+-])(\\d{2}):(\\d{2})")
    if(is.na(parts[[1, 1]])) {
        warning(.model_const_MESSAGE_DATE_TIME_HEADER)
        object@tz_offset <- NA_integer_
    }
    tz_sign <- if(parts[[1, 2]] == "+") 1 else -1
    object@tz_offset <- tz_sign * (strtoi(parts[[3]]) * 60 + strtoi(parts[[4]]))
    object
}

.model_hobo_set_columns <- function(object, data, has_numbers_column) {
    add_count_columns <- if(has_numbers_column) 1 else 0
    temp_column <- 2 + add_count_columns
    rh_column <- 3 + add_count_columns
    parts <- stringr::str_match(data[[temp_column]][[object@skip]], "Temp,? \\(?(.[CF])\\)?")
    if(is.na(parts[[1, 2]])) {
        warning(.model_const_MESSAGE_COLUMNS_PROBLEM)
        return(object)
    }
    temp_sensor_id <- .model_const_SENSOR_HOBO_T_C
    if(parts[[1, 2]] == "\u00b0F") {
        temp_sensor_id <- .model_const_SENSOR_HOBO_T_F
    }
    columns <- list()
    columns[[temp_sensor_id]] <- temp_column
    if(ncol(data) < rh_column){
        object@columns <- columns
        return(object)
    }
    parts <- stringr::str_match(data[[rh_column]][[object@skip]], "RH,? \\(?%\\)?")
    if(is.na(parts[[1, 1]])) {
        object@columns <- columns
        return(object)
    }
    columns[[.model_const_SENSOR_HOBO_RH]] <- rh_column
    object@columns <- columns
    object
}

.model_is_hobo_format_ok <- function(object) {
    if(is.na(object@tz_offset)) {
        return(FALSE)
    }
    if(length(object@columns) == 0) {
        return(FALSE)
    }
    if(is.na(object@date_format)) {
        warning(.model_const_MESSAGE_HOBO_DATE_FORMAT_PROBLEM)
        return(FALSE)
    }
    return(TRUE)
}

setMethod(
    ".model_get_serial_number_from_file",
    signature("mc_DataFormat"),
    function(object, path) {
        if(is.null(object@filename_serial_number_pattern)) {
          stop(stringr::str_glue("It is not possible identify serial_number from file. Pattern is missed in data_format."))
        }
        result <- stringr::str_match(basename(path), object@filename_serial_number_pattern)[1, 2]
        if(is.na(result)) {
            result <- stringr::str_match(basename(path), "(.+)\\.[^.]+")[1, 2]
            if(is.na(result)) {
                result <- basename(path)
            }
            warning(stringr::str_glue("It is not possible identify serial_number from file. Name {result} is used."))
        }
        result
    }
)

setMethod(
    ".model_get_serial_number_from_file",
    signature("mc_HOBODataFormat"),
    function(object, path) {
        changed_object <- object
        changed_object@skip <- object@skip - 1
        data <- .read_get_data_from_file(path, changed_object, nrows = 1)
        temp_column <- changed_object@columns[[1]]
        parts <- stringr::str_match(data[[temp_column]][[1]], "Temp,? \\(?\u00b0[CF]\\)? \\(?LGR S\\/N: (\\d+),")
        if(is.na(parts[[1, 2]])) {
            return(callNextMethod())
        }
        return(parts[[1, 2]])
    }
)

setMethod(
    ".model_edit_data",
    "mc_DataFormat",
    function(object, data_table) {
        data_table
    }
)

setMethod(
    ".model_edit_data",
    "mc_HOBODataFormat",
    function(object, data_table) {
        last_value_column <- max(unlist(object@columns))
        if(ncol(data_table) == last_value_column) {
            return(data_table)
        }
        na_values <- purrr::map(object@columns, ~ is.na(data_table[[.x]]))
        na_values <- purrr::reduce(na_values, `&`)
        logged_values <- purrr::map((last_value_column+1):ncol(data_table), ~ data_table[[.x]] == "Logged")
        logged_values <- purrr::reduce(logged_values, `|`)
        data_table[!(na_values & logged_values),]
    }
)

