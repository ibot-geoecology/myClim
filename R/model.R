# constants ================================================================================

#' @export
mc_const_TZ_UTC <- "UTC"
#' @export
mc_const_TZ_SOLAR <- "solar"
#' @export
mc_const_TZ_USER_DEFINED <- "user defined"

.model_const_COUNT_TEST_VALUES <- 100
.model_const_EDITABLE_LOCALITY_METADATA_PARAMETERS <- c("altitude", "lat_wgs84", "lon_wgs84", "tz_offset")

.model_const_PHYSICAL_T_C <- "T_C"
.model_const_PHYSICAL_moisture <- "moisture"
.model_const_PHYSICAL_TMSmoisture <- "TMSmoisture"
.model_const_PHYSICAL_RH_perc <- "RH_perc"
.model_const_PHYSICAL_l_cm <- "l_cm"
.model_const_PHYSICAL_l_mm <- "l_mm"
.model_const_PHYSICAL_v <- "v"
.model_const_PHYSICAL_t_h <- "t_h"

.model_const_VALUE_TYPE_REAL <- "real"
.model_const_VALUE_TYPE_INTEGER <- "integer"
.model_const_VALUE_TYPE_LOGICAL <- "logical"

# logger sensors
.model_const_SENSOR_TMS_T1 <- "TMS_T1"
.model_const_SENSOR_TMS_T2 <- "TMS_T2"
.model_const_SENSOR_TMS_T3 <- "TMS_T3"
.model_const_SENSOR_TMS_TMSmoisture <- "TMS_TMSmoisture"
.model_const_SENSOR_TM_T <- "TM_T"
# universal sensors
.model_const_SENSOR_count <- "count"
.model_const_SENSOR_coverage <- "coverage"
.model_const_SENSOR_snow_bool <- "snow_bool"
.model_const_SENSOR_snow_fresh <- "snow_fresh"
.model_const_SENSOR_snow_total <- "snow_total"
.model_const_SENSOR_precipitation <- "precipitation"
.model_const_SENSOR_sun_shine <- "sun_shine"
.model_const_SENSOR_wind <- "wind"
.model_const_SENSOR_GDD <- "GDD"
.model_const_SENSOR_FDD <- "FDD"
.model_const_SENSOR_moisture <- .model_const_PHYSICAL_moisture
.model_const_SENSOR_T_C <- .model_const_PHYSICAL_T_C
.model_const_SENSOR_RH_perc <- .model_const_PHYSICAL_RH_perc
.model_const_SENSOR_real <- .model_const_VALUE_TYPE_REAL
.model_const_SENSOR_integer <- .model_const_VALUE_TYPE_INTEGER
.model_const_SENSOR_logical <- .model_const_VALUE_TYPE_LOGICAL

.model_const_LOGGER_TOMST_TMS <- "TMS"
.model_const_LOGGER_TOMST_THERMODATALOGGER <- "ThermoDatalogger"

.model_const_DATA_FORMAT_TOMST <- "TOMST"
.model_const_DATA_FORMAT_TOMST_join <- "TOMST_join"

# classes ================================================================================

#' Class for sensor definition
#' Sensor definitions intslevesl are stored in [mc_data_sensors].
#' @slot sensor_id unique identifier of sensor (TMS_T1, TMS_T2, TMS_T3, TMS_TMSmoisture, ...)
#' @slot logger name of logger (TMS, ThermoDatalogger, ...)
#' @slot physical unit of sensor (T_C, TMSmoisture, moisture, RH_perc) (default NA)
#' @slot description character info
#' @slot default_height default height of sensor in m (default NA)
#' @slot value_type type of values (real, integer, logical) (default real)
#' @slot min_value minimal value (default NA)
#' @slot max_value maximal value (default NA)
#' @slot plot_color color in pot (default "")
#' @slot plot_line_width width of line in plot (default 1)
#' @export mc_Sensor
#' @exportClass mc_Sensor
#' @seealso [mc_data_sensors]
mc_Sensor <- setClass("mc_Sensor",
                      slots = c(sensor_id = "character",
                                logger = "character",
                                physical = "character",
                                description = "character",
                                default_height = "numeric",
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
              .Object@default_height <- NA_real_
              .Object@value_type <- .model_const_VALUE_TYPE_REAL
              .Object@min_value <- NA_real_
              .Object@max_value <- NA_real_
              .Object@plot_color <- NA_character_
              .Object@plot_line_width <- 1
              return(.Object)
          })

#' Class for physical
#' Class defining the element of the records (temperature, volumetric water content, height...)
#' @details See e.g. definition of temperature. Similarly as the definition of new loggers, new
#' physicals cn be added like modules. 
#'  \preformatted{
#' Slot "name": "T_C"
#' slot "description": "Temperature °C"
#' Slot "units": "°C"
#' Slot "viridis_color_map": "C"
#' Slot "scale_coeff": 0.03333333
#' }
#' @slot name of physical
#' @slot description character info
#' @slot units measurument (°C, \%, m3/m3, raw, mm, ...)
#' @slot calibration_class class for calibration
#' @slot viridis_color_map viridis color map option
#' @slot scale_coeff coefficient for plot; value * scale_coef is in range 0-1
#' @export mc_Physical
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

#' Class for myClim object metadata in Calc-format
#' @slot step time step of data in minutes
#' @slot step_text time step of data in text for irregular periods (e.g. month)
#' @export mc_MainMetadata
#' @exportClass mc_MainMetadata
#' @seealso Calc-format section of [myClim-package]
mc_MainMetadata <- setClass("mc_MainMetadata",
                            slots = c(step = "numeric",
                                      step_text = "character"))

#' Class for locality metadata
#' @details When reading without metadata, then locality is named after file
#' where the data come from, or after the sensor id where the data come form.  
#' @slot id name of locality
#' @slot altitude of locality
#' @slot lat_wgs84 latitude of locality in WGS-84
#' @slot lon_wgs84 longitude of locality in WGS-84
#' @slot tz_offset offset from UTC in minutes
#' @slot tz_type type of time zone
#' @slot user_data list for user data
#' @export mc_LocalityMetadata
#' @exportClass mc_LocalityMetadata
#' @seealso [myClim-package], [mc_LoggerMetadata], [mc_SensorMetadata]
mc_LocalityMetadata <- setClass("mc_LocalityMetadata",
                                slots = c(locality_id = "character",
                                          altitude = "numeric",
                                          lat_wgs84 = "numeric",
                                          lon_wgs84 = "numeric",
                                          tz_offset = "numeric",
                                          tz_type = "character",
                                          user_data = "list"))

setMethod("initialize",
          "mc_LocalityMetadata",
          function(.Object) {
              .Object@altitude <- NA_real_
              .Object@lat_wgs84 <- NA_real_
              .Object@lon_wgs84 <- NA_real_
              .Object@tz_offset <- NA_integer_
              .Object@tz_type <- mc_const_TZ_UTC
              .Object@user_data <- list()
              return(.Object)
          })

#' Class for logger metadata
#' @slot type of logger (TMS, ThermoDatalogger, HOBO, ibutton)
#' @slot serial_number serial number of the logger 
#' @export mc_LoggerMetadata
#' @exportClass mc_LoggerMetadata
mc_LoggerMetadata <- setClass("mc_LoggerMetadata",
                              slots = c(type = "character",
                                        serial_number = "character"),)

#' Class for logger clean info
#' @slot step Time step of microclimatic data series in minutes
#' @slot count_duplicits count of duplicated records - values with same date
#' @slot count_missed count of missing records; Period between the records should be the same length. If not, than missing. 
#' @slot count_disordered count of records incorrectly ordered in time. In table, newer record is followed by the older. 
#' @export mc_LoggerCleanInfo
#' @exportClass mc_LoggerCleanInfo
mc_LoggerCleanInfo <- setClass("mc_LoggerCleanInfo",
                               slots = c(step = "numeric",
                                         count_duplicits = "numeric",
                                         count_missed = "numeric",
                                         count_disordered = "numeric"))

setMethod("initialize",
          "mc_LoggerCleanInfo",
          function(.Object) {
              .Object@step <- NA_integer_
              .Object@count_duplicits <- NA_integer_
              .Object@count_missed <- NA_integer_
              .Object@count_disordered <- NA_integer_
              return(.Object)
          })

#' Class for sensor metadata
#' @details `sensor_id` must be one of the defined id in myClim. see [mc_data_sensors]. 
#' It is useful to select on of predefined, because it makes plotting and calculaton easier. 
#' Through `sensor_id` myClim assign pre-deined physicyl units or plotting colors see [mc_Sensor].  
#' @slot sensor_id unique identifier of sensor (TMS_T1, TMS_T2, TMS_T3, TMS_TMSmoisture, ...) [mc_data_sensors] e.g. TMS_T1, TMS_TMSmoisture, snow_fresh...
#' @slot name character, could be same as `sensor_id` but also defined by function or user.  
#' @slot height in meters
#' @slot calibrated logical - detect if sensor is calibrated
#' @export mc_SensorMetadata
#' @exportClass mc_SensorMetadata
#' @seealso [myClim-package], [mc_LoggerMetadata], [mc_data_sensors]
mc_SensorMetadata <- setClass("mc_SensorMetadata",
                              slots = c(sensor_id = "character",
                                        name = "character",
                                        height = "numeric",
                                        calibrated = "logical"))

setMethod("initialize",
          "mc_SensorMetadata",
          function(.Object) {
              .Object@height <- NA_integer_
              .Object@calibrated <- FALSE
              return(.Object)
          })

setGeneric(
    ".model_is_physical_TMSmoisture",
    function(object, data){
        standardGeneric(".model_is_physical_TMSmoisture")
    }
)

setMethod(
    ".model_is_physical_TMSmoisture",
    "mc_SensorMetadata",
    function(object) {
        physical_id <- mc_data_sensors[[object@sensor_id]]@physical
        return(physical_id == .model_const_PHYSICAL_TMSmoisture)
    }
)

setGeneric(
    ".model_is_physical_T_C",
    function(object, data){
        standardGeneric(".model_is_physical_T_C")
    }
)

setMethod(
    ".model_is_physical_T_C",
    "mc_SensorMetadata",
    function(object) {
        physical_id <- mc_data_sensors[[object@sensor_id]]@physical
        return(physical_id == .model_const_PHYSICAL_T_C)
    }
)

setGeneric(
    ".model_is_type_real",
    function(object, data){
        standardGeneric(".model_is_type_real")
    }
)

setMethod(
    ".model_is_type_real",
    "mc_SensorMetadata",
    function(object) {
        value_type <- mc_data_sensors[[object@sensor_id]]@value_type
        return(value_type == .model_const_VALUE_TYPE_REAL)
    }
)

#' Class for logger file data format
#'
#' The Class used for parsing source data files. Typically the csv files downloaded from microclimatic loggers. 
#' Each supported logger has established own specific object of class "mc_TOMSTDataFormat" defining the parameters.   
#' @details The logger definitions are stored in R environment object ./data/mc_data_formats.rda 
#' And thus it easy to add the ability of reading new, unsupported logger just by defining its Class parameters. 
#' An object of class "mc_TOMSTDataFormat"
#' Below see e.g. the Class defining TOMST file format. 
#'  
#' \preformatted{
#' An object of class "mc_TOMSTDataFormat"
#' attr(,"has_header"): FALSE
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
#' @slot has_header detects if table has header (default TRUE)
#' @slot separator columns separator (default ";")
#' @slot date_column index of date column (default NA)
#' @slot date_format format of date (default NA)
#' @slot na_strings strings for NA values (default NA)
#' @slot columns list with names and indexes of value columns (default list())
#' @slot filename_serial_number_pattern character pattern for detecting serial_number from filename (default NA)
#' @slot data_row_pattern character pattern for detecting right file format
#' @slot logger_type type of logger: TMS, ThermoDatalogger (default NA)
#' @export mc_DataFormat
#' @exportClass mc_DataFormat
#' @seealso [mc_data_formats],[mc_TOMSTDataFormat-class], [mc_TOMSTJoinDataFormat-class]
mc_DataFormat <- setClass("mc_DataFormat",
                          slots = c(has_header = "logical",
                                    separator = "character",
                                    date_column = "numeric",
                                    date_format = "character",
                                    na_strings = "character",
                                    columns = "list",
                                    filename_serial_number_pattern = "character",
                                    data_row_pattern = "character",
                                    logger_type = "character"))

setMethod("initialize",
          "mc_DataFormat",
          function(.Object) {
              .Object@has_header <- TRUE
              .Object@separator <- ";"
              .Object@date_column <- NA_integer_
              .Object@date_format <- NA_character_
              .Object@na_strings <- NA_character_
              .Object@columns <- list()
              .Object@filename_serial_number_pattern <- NA_character_
              .Object@logger_type < NA_character_
              return(.Object)
          })

#' Class for reading TOMST logger files
#' Provides the key for the column in source files. Where is the date, 
#' in what format is the date, in which columns are records of which sensors.
#' The code defining the class is in section methods ./R/model.R  
#' 
#' @export mc_TOMSTDataFormat
#' @exportClass mc_TOMSTDataFormat
#' @seealso [myClim::mc_DataFormat],[mc_data_formats], [mc_TOMSTJoinDataFormat-class]
mc_TOMSTDataFormat <- setClass("mc_TOMSTDataFormat", contains = "mc_DataFormat")

#' Class for reading TMS join files
#' Provides the key for the column in source files. Where is the date, 
#' in what format is the date, in which columns are records of which sensors.
#' The code defining the class is in section methods ./R/model.R
#' 
#' TMS join file format is the output of IBOT internal post-processing of TOMST logger files.  
#' @export mc_TOMSTJoinDataFormat
#' @exportClass mc_TOMSTJoinDataFormat
#' @seealso [myClim::mc_DataFormat],[mc_data_formats],[mc_TOMSTDataFormat-class], [mc_TOMSTJoinDataFormat-class]
mc_TOMSTJoinDataFormat <- setClass("mc_TOMSTJoinDataFormat", contains = "mc_DataFormat")

# generics ================================================================================

setGeneric(
  ".model_load_data_format_params_from_data",
  function(object, data){
    standardGeneric(".model_load_data_format_params_from_data")
  }
)

setGeneric(
  ".model_get_serial_number_from_filename",
  function(object, filename){
    standardGeneric(".model_get_serial_number_from_filename")
  }
)

setGeneric(
  ".model_is_file_in_right_format",
  function(object, filename){
    standardGeneric(".model_is_file_in_right_format")
  }
)

# methods ================================================================================

setMethod(
    ".model_load_data_format_params_from_data",
    "mc_DataFormat",
    function(object, data) {
        object
    }
)

setMethod(
    ".model_load_data_format_params_from_data",
    "mc_TOMSTDataFormat",
    function(object, data) {
        object@date_format <- .get_tomst_datetime_format(data, object@date_column)
        .change_tomst_columns_and_logger_type(object, data)
    }
)

.get_tomst_datetime_format <- function(data, date_column){
    if(stringr::str_detect(data[1, date_column], "\\d{4}\\.\\d{1,2}\\.\\d{1,2} \\d{2}:\\d{2}"))
    {
        return("%Y.%m.%d %H:%M")
    }
    if(stringr::str_detect(data[1, date_column], "\\d{1,2}\\.\\d{1,2}\\.\\d{4} \\d{2}:\\d{2}"))
    {
        return("%d.%m.%Y %H:%M")
    }
    return(NA_character_)
}

.change_tomst_columns_and_logger_type <- function(object, data){
    tm_columns <- list(4)
    names(tm_columns) <- .model_const_SENSOR_TM_T
    tms_columns <- list(4, 5, 6, 7)
    names(tms_columns) <- c(.model_const_SENSOR_TMS_T1, .model_const_SENSOR_TMS_T2,.model_const_SENSOR_TMS_T3,
                            .model_const_SENSOR_TMS_TMSmoisture)
    data <- head(data, .model_const_COUNT_TEST_VALUES)
    if(all(is.na(data[[tms_columns[[.model_const_SENSOR_TMS_T2]]]]))) {
        object@columns <- tm_columns
        object@logger_type <- .model_const_LOGGER_TOMST_THERMODATALOGGER
    }
    else {
        object@columns <- tms_columns
        object@logger_type <- .model_const_LOGGER_TOMST_TMS
    }
    object
}

setMethod(
    ".model_load_data_format_params_from_data",
    "mc_TOMSTJoinDataFormat",
    function(object, data) {
        .change_tomst_join_columns_and_logger_type(object, data)
    }
)

.change_tomst_join_columns_and_logger_type <- function(object, data){
    tmj_columns <- list(5)
    names(tmj_columns) <- .model_const_SENSOR_TM_T
    tmsj_columns <- list(5, 6, 7, 8, 9)
    names(tmsj_columns) <- c(.model_const_SENSOR_TMS_T1, .model_const_SENSOR_TMS_T2,.model_const_SENSOR_TMS_T3,
                             .model_const_SENSOR_TMS_TMSmoisture, .model_const_SENSOR_moisture)
    data <- head(data, .model_const_COUNT_TEST_VALUES)
    is_T1_NA <- all(is.na(data[[tmsj_columns[[.model_const_SENSOR_TMS_T1]]]]))
    is_NA_T2_T3 <- all(is.na(data[[tmsj_columns[[.model_const_SENSOR_TMS_T2]]]])) &&
        all(is.na(data[[tmsj_columns[[.model_const_SENSOR_TMS_T3]]]]))
    is_T1_T2_T3_equals <- (all(data[[tmsj_columns[[.model_const_SENSOR_TMS_T1]]]] == data[[tmsj_columns[[.model_const_SENSOR_TMS_T2]]]]) &&
                           all(data[[tmsj_columns[[.model_const_SENSOR_TMS_T1]]]] == data[[tmsj_columns[[.model_const_SENSOR_TMS_T3]]]]))
    if(!is_T1_NA && (is_NA_T2_T3 || is_T1_T2_T3_equals)) {
        object@columns <- tmj_columns
        object@logger_type <- .model_const_LOGGER_TOMST_THERMODATALOGGER
        return(object)
    }
    object@logger_type <- .model_const_LOGGER_TOMST_TMS
    moisture <- data[[tmsj_columns[[.model_const_SENSOR_moisture]]]]
    if(!any(is.na(moisture)) && all(moisture == 0)) {
        object@columns <- within(tmsj_columns, rm(moisture))
        return(object)
    }
    object@columns <- tmsj_columns
    object
}

setMethod(
    ".model_get_serial_number_from_filename",
    signature("mc_DataFormat"),
    function(object, filename) {
        if(is.null(object@filename_serial_number_pattern)) {
          stop(stringr::str_glue("It is not possible identify serial_number from file {filename}."))
        }
        stringr::str_match(basename(filename), object@filename_serial_number_pattern)[1, 2]
    }
)

setMethod(
    ".model_is_file_in_right_format",
    signature("mc_DataFormat"),
    function(object, filename) {
        con <- file(filename, "r")
        skip <- object@has_header
        while (TRUE) {
            line <- readLines(con, n = 1)
            if ( length(line) == 0 ) {
                close(con)
                return(FALSE)
            }
            if(skip) {
              skip <- FALSE
              next
            }
            close(con)
            return(stringr::str_detect(line, object@data_row_pattern))
        }
    }
)
