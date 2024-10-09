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
.model_const_PHYSICAL_VWC <- "VWC"
.model_const_PHYSICAL_p_kPa <- "p_kPa"
.model_const_PHYSICAL_RH <- "RH"
.model_const_PHYSICAL_T_C <- "T_C"
.model_const_PHYSICAL_t_h <- "t_h"
.model_const_PHYSICAL_moisture_raw <- "moisture_raw"
.model_const_PHYSICAL_radius_raw <- "radius_raw"
.model_const_PHYSICAL_v <- "v"

.model_const_VALUE_TYPE_REAL <- "real"
.model_const_VALUE_TYPE_INTEGER <- "integer"
.model_const_VALUE_TYPE_LOGICAL <- "logical"

### logger sensors
#' Default sensor for TOMST TMS soil temperature
#' @description 
#' This constant is used in the function [myClim::mc_calc_vwc] to account for soil temperature effect
#' while converting the raw TMS soil moisture (scaled TDT signal) to volumetric water content (VWC). 
#' mc_const_SENSOR_TMS_T1 = "`r mc_const_SENSOR_TMS_T1`"
#' @export
mc_const_SENSOR_TMS_T1 <- "TMS_T1"
#' Default sensor for TOMST TMS temperature of soil surface
#' @export
mc_const_SENSOR_TMS_T2 <- "TMS_T2"
#' Default sensor for TOMST TMS air temperature
#' @export
mc_const_SENSOR_TMS_T3 <- "TMS_T3"
#' Default sensor for TOMST TMS raw soil moisture
#' @description 
#' This constant is used in the function [myClim::mc_calc_vwc] as default for sensor for
#' converting the raw TMS soil moisture (scaled TDT signal) to volumetric water content (VWC).
#' mc_const_SENSOR_TMS_moist = "`r mc_const_SENSOR_TMS_moist`"
#' @export
mc_const_SENSOR_TMS_moist <- "TMS_moist"
#' Default sensor for TOMST Thermologger temperature
#' @export
mc_const_SENSOR_Thermo_T <- "Thermo_T"
#' Default sensor for TOMST Dendrometer temperature
#' @export
mc_const_SENSOR_Dendro_T <- "Dendro_T"
#' Default sensor for TOMST Dendrometer radius difference
#' @description
#' This constant is used in the function [myClim::mc_calc_tomst_dendro]
#' as default sensor for converting the change in stem size from raw
#' TOMST units to micrometers.
#' mc_const_SENSOR_Dendro_raw = "`r mc_const_SENSOR_Dendro_raw`"
#' @export
mc_const_SENSOR_Dendro_raw <- "Dendro_raw"
#' Onset HOBO temperature sensor id
#' @export
mc_const_SENSOR_HOBO_T <- "HOBO_T"
#' Onset HOBO humidity sensor id
#' @export
mc_const_SENSOR_HOBO_RH <- "HOBO_RH"
#' Onset HOBO external temperature sensor id
#' @export
mc_const_SENSOR_HOBO_EXTT <- "HOBO_extT"

.model_const_WRONG_CALIBRATION_SENSOR_ID <- mc_const_SENSOR_TMS_moist

### universal sensors
#' Count sensor id see [myClim::mc_agg()]
#' @export
mc_const_SENSOR_count <- "count"
#' Coverage sensor id see [myClim::mc_agg()]
#' @export
mc_const_SENSOR_coverage <- "coverage"
#' Freezing Degree Days sensor id see [myClim::mc_calc_fdd()]
#' @export
mc_const_SENSOR_FDD <- "FDD"
#' Growing Degree Days sensor id see [myClim::mc_calc_gdd()]
#' @export
mc_const_SENSOR_GDD <- "GDD"
#' Precipitation sensor id
#' @export
mc_const_SENSOR_precipitation <- "precipitation"
#' Radius difference sensor id
#' @export
mc_const_SENSOR_dendro_l_um <- "dendro_l_um"
#' Snow existence sensor id see [myClim::mc_calc_snow()]
#' @export
mc_const_SENSOR_snow_bool <- "snow_bool"
#' Height of newly fallen snow sensor id
#' @export
mc_const_SENSOR_snow_fresh <- "snow_fresh"
#' Height snow sensor id
#' @export
mc_const_SENSOR_snow_total <- "snow_total"
#' Time of sun shine sensor id
#' @export
mc_const_SENSOR_sun_shine <- "sun_shine"
#' Vapor Pressure Deficit sensor id see [myClim::mc_calc_vpd()]
#' @export
mc_const_SENSOR_VPD <- "VPD"
#' Speed of wind sensor id
#' @export
mc_const_SENSOR_wind_speed <- "wind_speed"

#' Volumetric soil moisture sensor id see [myClim::mc_calc_vwc()]
#' @export
mc_const_SENSOR_VWC <- .model_const_PHYSICAL_VWC
#' Relative humidity sensor id
#' @export
mc_const_SENSOR_RH <- .model_const_PHYSICAL_RH
#' Temperature sensor id
#' @export
mc_const_SENSOR_T_C <- .model_const_PHYSICAL_T_C

#' General real sensor id
#' @export
mc_const_SENSOR_real <- .model_const_VALUE_TYPE_REAL
#' General integer sensor id
#' @export
mc_const_SENSOR_integer <- .model_const_VALUE_TYPE_INTEGER
#' General logical sensor id
#' @export
mc_const_SENSOR_logical <- .model_const_VALUE_TYPE_LOGICAL

.model_const_LOGGER_TOMST_TMS <- "TMS"
.model_const_LOGGER_TOMST_TMS_L45 <- "TMS_L45"
.model_const_LOGGER_TOMST_THERMODATALOGGER <- "Thermo"
.model_const_LOGGER_TOMST_DENDROMETER <- "Dendro"
.model_const_LOGGER_HOBO_U23_001A <- "HOBO_U23-001A"
.model_const_LOGGER_HOBO_U23_004 <- "HOBO_U23-004"

.model_logger_types <- list(
    .model_const_LOGGER_TOMST_TMS,
    .model_const_LOGGER_TOMST_TMS_L45,
    .model_const_LOGGER_TOMST_THERMODATALOGGER,
    .model_const_LOGGER_TOMST_DENDROMETER,
    .model_const_LOGGER_HOBO_U23_001A,
    .model_const_LOGGER_HOBO_U23_004
)

.model_const_DATA_FORMAT_TOMST <- "TOMST"
.model_const_DATA_FORMAT_TOMST_join <- "TOMST_join"
.model_const_DATA_FORMAT_HOBO <- "HOBO"

.model_const_SENSOR_STATE_SOURCE <- "source"
.model_const_SENSOR_STATE_ERROR <- "error"
.model_const_SENSOR_STATE_CONFLICT <- "conflict"

.model_const_MESSAGE_NO_DATA <- "There aren't any data in source file."
.model_const_MESSAGE_SEPARATED_TIME <- "Separated time in source data isn't supported."
.model_const_MESSAGE_DATE_TIME_HEADER <- "It is not possible to detect timezone offset from header."
.model_const_MESSAGE_COLUMNS_PROBLEM <- "It is not possible to detect columns from header."
.model_const_MESSAGE_HOBO_DATE_FORMAT_PROBLEM <- "HOBO data format required filled in parameter date_format."
.model_const_MESSAGE_HOBO_CONVERT_FAHRENHEIT <- "Temperature data in \u00b0F is converted to \u00b0C."
.model_const_MESSAGE_ERROR_DATA_FORMAT <- "The {parameter_name} parameter is required."
.model_const_MESSAGE_ERROR_DATA_FORMAT_COLUMNS <- "The column list cannot be empty."

.model_const_FORMAT_RAW <- "raw"
.model_const_FORMAT_AGG <- "agg"

.model_const_HOBO_LOGGER_TYPE_SECONDARY_TITLES <- list("RH,? \\(?%\\)?", "Temp,? \\(?(.[CF])\\)?")
names(.model_const_HOBO_LOGGER_TYPE_SECONDARY_TITLES) <- c(.model_const_LOGGER_HOBO_U23_001A, .model_const_LOGGER_HOBO_U23_004)

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

#' Print function for myClim object
#'
#' Function print metadata of myClim object and table from function mc_info().
#'
#' @param x myClim object see [myClim-package]
#' @param ... other parameters from function print for tibble [tibble::tibble]
#' @export
#' @examples
#' print(mc_data_example_agg, n=10)
print.myClimList <- function(x, ...) {
    print(stringr::str_glue("myClim object created with myClim package version {x$metadata@version}"))
    print(stringr::str_glue("{x$metadata@format_type} format"))
    count_env <- .common_get_count_items(x)
    if(.common_is_agg_format(x)) {
        print(stringr::str_glue("{x$metadata@period} period"))
        print(stringr::str_glue("{count_env$localities} localities, {count_env$sensors} sensors"))
    }
    else {
        print(stringr::str_glue("{count_env$localities} localities, {count_env$loggers} loggers, {count_env$sensors} sensors"))
    }

    info_df <- mc_info(x)
    sensors_text <- paste0(sort(unique(info_df$sensor_name)), collapse = ", ")
    print(stringr::str_glue("sensors: {sensors_text}"))
    print(tibble::as_tibble(info_df), ...)
}

#' Extract localities with []
#'
#' Using [] for extract localities.
#'
#' @param x myClim object see [myClim-package]
#' @param ... indexes for extract localities
#' @return myClim object with subset of localities see [myClim-package]
#' @export
#' @examples
#' filtered_data <- mc_data_example_raw[1:2]
`[.myClimList` <- function(x, ...) {
    x$localities <- `[`(x$localities, ...)
    return(x)
}

#' Length function for myClim object
#'
#' Function return number of localities.
#'
#' @param x myClim object see [myClim-package]
#' @param ... other parameters from function length
#' @export
#' @examples
#' length(mc_data_example_agg)
length.myClimList <- function(x, ...) {
    length(x$localities)
}

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
#' @slot sensor_id unique identifier of sensor (TMS_T1, TMS_T2, TMS_T3, TMS_moist, ...)
#' @slot logger name of logger (TMS, Thermo, ...)
#' @slot physical unit of sensor (T_C, moisture_raw, moisture, RH) (default NA)
#' @slot description character info
#' @slot value_type type of values (real, integer, logical) (default real)
#' @slot min_value minimal value (default NA)
#' @slot max_value maximal value (default NA)
#' @slot plot_color color in plot (default "")
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
#' physicals can be added like modules. 
#' \preformatted{
#' Slot "name": "T_C"
#' Slot "description": "Temperature °C"
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
#' @slot type of logger (TMS, Thermo, Dendro, HOBO)
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
#' @slot sensor_id unique identifier of sensor (TMS_T1, TMS_T2, TMS_T3, TMS_moist, ...) [mc_data_sensors] e.g. TMS_T1, TMS_moist, snow_fresh...
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
    ".model_is_physical_moisture_raw",
    function(object){
        standardGeneric(".model_is_physical_moisture_raw")
    }
)

setMethod(
    ".model_is_physical_moisture_raw",
    "mc_SensorMetadata",
    function(object) {
        .model_is_physical(object, .model_const_PHYSICAL_moisture_raw)
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

#' Class for Logger File Data Format
#'
#' This class is used for parsing source TXT/CSV files downloaded from microclimatic
#' loggers.
#'
#' @details myClim offers several pre-defined
#' logger file data formats, such as TOMST TMS or HOBO. Users can also define custom
#' readings for their own loggers. Pre-defined and custom loggers in myClim each have
#' their own specific object of class `mc_{logger}DataFormat`, which defines the
#' parameters for handling logger files.
#' The pre-defined logger definitions are stored in the R environment object
#' `./data/mc_data_formats.rda`.
#' 
#' @slot skip The number of rows to skip before the first row containing microclimatic records.
#' For example, to skip the header (default 0).
#' @slot separator The column separator (default is a comma ",").
#' @slot date_column The index of the date column - required (default NA).
#' @slot date_format The format of the date (default NA).
#'
#' For a description of the date_format parameter, see [strptime()]. If the format is in ISO8601
#' and the function [vroom::vroom()] automatically detects datetime values,
#' the date_format parameter can be NA.
#' @slot na_strings Strings for representing NA values, e.g., "-100", "9999" (default "").
#' @slot error_value The value that represents an error of the sensor, e.g., 404, 9999 (default NA).
#'
#' The error_value is replaced by NA, and intervals of errors are flagged in `sensor$states`
#' (see [myClim-package]).
#' @slot columns A list with names and indexes of value columns - required (default list()).
#'
#' Names come from names(mc_data_sensors). Names are defined as constants `mc_const_SENSOR_*`.
#' For example, if the third column is temperature, you can define it as `columns[[mc_const_SENSOR_T_C]] <- 3`.
#' There are universal sensors for arbitrary value types: `mc_const_SENSOR_real`, `mc_const_SENSOR_integer`
#' and `mc_const_SENSOR_logical`. Multiple columns with same sensor type can be defined
#' as `columns[[mc_const_SENSOR_real]] <- c(2, 3, 4)`. The names in this example will be `real1`, `real2` and `real3`.
#' @slot col_types Parameter for [vroom::vroom()] (default NA).
#'
#' To ensure the correct reading of values, you have the possibility to strictly define the types of columns.
#' @slot filename_serial_number_pattern A character pattern for detecting the serial number from the file name (default NA).
#'
#' The regular expression with brackets around the serial number.
#' For example, the pattern for old TOMST files is `"data_(\\d+)_\\d+\\.csv$"`. If the value is NA, the name of the file is used
#' as the serial number.
#' @slot data_row_pattern A character pattern for detecting the correct file format (default NA).
#'
#' The regular expression. If data_row_pattern is NA, then the file format is not checked.

#' @slot logger_type The type of logger: TMS, TMS_L45, Thermo, Dendro, HOBO, ... (default NA).
#' @slot tz_offset The timezone offset in minutes from UTC - required (default NA).
#'
#' If the value of the tz_offset parameter is 0, then datetime values are in UTC.
#' If the time zone offset is defined in the value, e.g., `"2020-10-06 09:00:00+0100"`,
#' and `date_format` is `"%Y-%m-%d %H:%M:%S%z"`, the value is automatically converted to UTC.
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
                                    col_types = "character",
                                    filename_serial_number_pattern = "character",
                                    data_row_pattern = "character",
                                    logger_type = "character",
                                    tz_offset = "numeric"))

setMethod("initialize",
          "mc_DataFormat",
          function(.Object) {
              .Object@skip <- 0
              .Object@separator <- ","
              .Object@date_column <- NA_integer_
              .Object@date_format <- NA_character_
              .Object@na_strings <- ""
              .Object@error_value <- NA_integer_
              .Object@columns <- list()
              .Object@col_types <- NA_character_
              .Object@filename_serial_number_pattern <- NA_character_
              .Object@data_row_pattern <- NA_character_
              .Object@logger_type <- NA_character_
              .Object@tz_offset <- NA_integer_
              return(.Object)
          })

#' Class for reading TOMST logger files
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
#' TMS join file format is the output of IBOT internal post-processing of TOMST logger files.
#' @seealso [myClim::mc_DataFormat],[mc_data_formats],[mc_TOMSTDataFormat-class], [mc_TOMSTJoinDataFormat-class]
#' @exportClass mc_TOMSTJoinDataFormat
mc_TOMSTJoinDataFormat <- setClass("mc_TOMSTJoinDataFormat", contains = "mc_DataFormat")

#' Class for reading HOBO logger files
#'
#' Provides the key for reading the HOBO source files. In which column is the date,
#' in what format is the date, in which columns are records of which sensors.
#' The code defining the class is in section methods ./R/model.R
#'
#' @slot convert_fahrenheit if TRUE temperature values are converted from °F to °C (default FALSE)
#' @seealso [myClim::mc_DataFormat], [mc_data_formats]
#' @exportClass mc_HOBODataFormat
mc_HOBODataFormat <- setClass("mc_HOBODataFormat",
                              slots = c(convert_fahrenheit = "logical"),
                              contains = "mc_DataFormat")

setMethod("initialize",
          "mc_HOBODataFormat",
          function(.Object) {
              .Object <- callNextMethod(.Object)
              .Object@convert_fahrenheit <- FALSE
              return(.Object)
          })

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

setGeneric(
    ".model_check_format",
    function(object){
        standardGeneric(".model_check_format")
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
        object <- .model_change_tomst_columns_and_logger_type(object, data)
        object <- .model_tomst_change_col_type(object, data)
        return(object)
    }
)

.model_change_tomst_columns_and_logger_type <- function(object, data){
    tm_columns <- list(4)
    names(tm_columns) <- mc_const_SENSOR_Thermo_T
    dendro_columns <- list(4, 7)
    names(dendro_columns) <- c(mc_const_SENSOR_Dendro_T, mc_const_SENSOR_Dendro_raw)
    tms_columns <- list(4, 5, 6, 7)
    names(tms_columns) <- c(mc_const_SENSOR_TMS_T1, mc_const_SENSOR_TMS_T2, mc_const_SENSOR_TMS_T3,
                            mc_const_SENSOR_TMS_moist)
    if(all(is.na(data[[tms_columns[[mc_const_SENSOR_TMS_T2]]]]))) {
        if(all(is.na(data[[dendro_columns[[mc_const_SENSOR_Dendro_raw]]]])) ||
           all(data[[dendro_columns[[mc_const_SENSOR_Dendro_raw]]]] == .model_const_TOMST_THERMODATALOGGER_VALUE)) {
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

.model_tomst_change_col_type <- function(object, data) {
    has_comma <- .model_is_comma_in_data(object, data)
    if(!any(has_comma)) {
        object@col_types <- "icinnniin"
    }
    return(object)
}

.model_is_comma_in_data <- function(object, data) {
    return(purrr::map_lgl(object@columns, ~ any(stringr::str_detect(data[[.x]], ","), na.rm=TRUE)))
}

setMethod(
    ".model_load_data_format_params_from_file",
    "mc_TOMSTJoinDataFormat",
    function(object, path) {
        if(!.model_is_file_in_right_format(object, path)) {
            return(NULL)
        }
        data <- .read_get_data_from_file(path, object, nrows = .model_const_COUNT_TEST_VALUES)
        .model_change_tomst_join_columns_and_logger_type(object, data)
    }
)

.model_change_tomst_join_columns_and_logger_type <- function(object, data){
    thermoj_columns <- list(5)
    names(thermoj_columns) <- mc_const_SENSOR_Thermo_T
    tmsj_columns <- list(5, 6, 7, 8, 9)
    names(tmsj_columns) <- c(mc_const_SENSOR_TMS_T1, mc_const_SENSOR_TMS_T2, mc_const_SENSOR_TMS_T3,
                            mc_const_SENSOR_TMS_moist, mc_const_SENSOR_VWC)
    is_T1_NA <- all(is.na(data[[tmsj_columns[[mc_const_SENSOR_TMS_T1]]]]))
    is_NA_T2_T3 <- all(is.na(data[[tmsj_columns[[mc_const_SENSOR_TMS_T2]]]])) &&
        all(is.na(data[[tmsj_columns[[mc_const_SENSOR_TMS_T3]]]]))
    is_T1_T2_T3_equals <- (all(data[[tmsj_columns[[mc_const_SENSOR_TMS_T1]]]] == data[[tmsj_columns[[mc_const_SENSOR_TMS_T2]]]]) &&
        all(data[[tmsj_columns[[mc_const_SENSOR_TMS_T1]]]] == data[[tmsj_columns[[mc_const_SENSOR_TMS_T3]]]]))
    if((!is.na(object@logger_type) && object@logger_type == .model_const_LOGGER_TOMST_THERMODATALOGGER) ||
        (!is_T1_NA && (is_NA_T2_T3 || is_T1_T2_T3_equals))) {
        object@columns <- thermoj_columns
        if(is.na(object@logger_type)) {
            object@logger_type <- .model_const_LOGGER_TOMST_THERMODATALOGGER
        }
        return(object)
    }
    if(is.na(object@logger_type)) {
        object@logger_type <- .model_const_LOGGER_TOMST_TMS
    }
    moisture <- data[[tmsj_columns[[mc_const_SENSOR_VWC]]]]
    if(!any(is.na(moisture)) && all(moisture == 0)) {
        object@columns <- tmsj_columns[names(tmsj_columns) != mc_const_SENSOR_VWC]
        return(object)
    }
    object@col_types <- "icccdddid"
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
        object <- .model_hobo_set_skip(object, lines)
        data <- .read_get_data_from_file(path, object, nrows = .model_const_COUNT_TEST_VALUES)
        object@skip <- object@skip + 1
        has_numbers_column <- data[[1]][[1]] == "#"
        object <- .model_hobo_set_date_column(object, data, has_numbers_column)
        if(is.na(object@date_column)) {
            return(NULL)
        }
        object <- .model_hobo_set_tz_offset(object, data)
        object <- .model_hobo_set_columns_and_logger_type(object, data, has_numbers_column)
        object <- .model_hobo_change_col_type(object, data)

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

.model_hobo_set_skip <- function(object, lines) {
    if(stringr::str_starts(lines[[1]], '"?Plot Title:')) {
        object@skip <- 1
    } else {
        object@skip <- 0
    }
    object
}

.model_hobo_set_date_column <- function (object, data, has_numbers_column) {
    date_column <- if(has_numbers_column) 2 else 1
    if(data[[date_column]][[1]] == "Date") {
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
    if(data[[object@date_column]][[1]] == "Date Time") {
        object@tz_offset <- 0
        return(object)
    }
    parts <- stringr::str_match(data[[object@date_column]][[1]], "Date Time, GMT([+-])(\\d{2}):(\\d{2})")
    if(is.na(parts[[1, 1]])) {
        warning(.model_const_MESSAGE_DATE_TIME_HEADER)
        object@tz_offset <- NA_integer_
    }
    tz_sign <- if(parts[[1, 2]] == "+") 1 else -1
    object@tz_offset <- tz_sign * (strtoi(parts[[3]]) * 60 + strtoi(parts[[4]]))
    object
}

.model_hobo_set_columns_and_logger_type <- function(object, data, has_numbers_column) {
    col_types <- rep("c", ncol(data))
    add_count_columns <- if(has_numbers_column) 1 else 0
    temp_column <- 2 + add_count_columns
    secondary_column <- 3 + add_count_columns
    parts <- stringr::str_match(data[[temp_column]][[1]], "Temp,? \\(?(.[CF])\\)?")
    if(is.na(parts[[1, 2]])) {
        warning(.model_const_MESSAGE_COLUMNS_PROBLEM)
        return(object)
    }
    temp_sensor_id <- mc_const_SENSOR_HOBO_T
    if(parts[[1, 2]] == "\u00b0F") {
        object@convert_fahrenheit <- TRUE
    }
    columns <- list()
    columns[[temp_sensor_id]] <- temp_column
    col_types[[temp_column]] <- "d"
    object@columns <- columns
    object@col_types <- paste0(col_types, collapse="")
    if(ncol(data) < secondary_column){
        return(object)
    }

    if(.model_is_logger_type_hobo(object, data, secondary_column, .model_const_LOGGER_HOBO_U23_001A)) {
        columns[[mc_const_SENSOR_HOBO_RH]] <- secondary_column
        col_types[[secondary_column]] <- "d"
        object@logger_type  <- .model_const_LOGGER_HOBO_U23_001A
    }
    if(.model_is_logger_type_hobo(object, data, secondary_column, .model_const_LOGGER_HOBO_U23_004)) {
        columns[[mc_const_SENSOR_HOBO_EXTT]] <- secondary_column
        col_types[[secondary_column]] <- "d"
        object@logger_type  <- .model_const_LOGGER_HOBO_U23_004
    }

    object@columns <- columns
    object@col_types <- paste0(col_types, collapse="")
    return(object)
}

.model_is_logger_type_hobo <- function(object, data, secondary_column, logger_type) {
    if(!is.na(object@logger_type)) {
        return(object@logger_type == logger_type)
    }
    column_pattern <- .model_const_HOBO_LOGGER_TYPE_SECONDARY_TITLES[[logger_type]]
    parts <- stringr::str_match(data[[secondary_column]][[1]], column_pattern)
    return(!is.na(parts[[1, 1]]))
}

.model_hobo_change_col_type <- function(object, data) {
    has_comma <- .model_is_comma_in_data(object, data[-1, ])
    if(any(has_comma)) {
        col_types <- strsplit(object@col_types, split="")[[1]]
        for(i in object@columns) {
            col_types[[i]] <- "c"
        }
        object@col_types <- paste0(col_types, collapse="")
    }
    return(object)
}

.model_is_hobo_format_ok <- function(object) {
    if(is.na(object@tz_offset)) {
        return(FALSE)
    }
    if(length(object@columns) == 0) {
        return(FALSE)
    }
    if(all(is.na(object@date_format))) {
        warning(.model_const_MESSAGE_HOBO_DATE_FORMAT_PROBLEM)
        return(FALSE)
    }
    return(TRUE)
}

setMethod(
    ".model_get_serial_number_from_file",
    signature("mc_DataFormat"),
    function(object, path) {
        result <- NA_character_
        if(!is.na(object@filename_serial_number_pattern)) {
            result <- stringr::str_match(basename(path), object@filename_serial_number_pattern)[1, 2]
        }
        if(is.na(result)) {
            result <- stringr::str_match(basename(path), "(.+)\\.[^.]+")[1, 2]
            if(is.na(result)) {
                result <- basename(path)
            }
            if(!is.na(object@filename_serial_number_pattern)) {
                warning(stringr::str_glue("It is not possible identify serial_number from file. Name {result} is used."))
            }
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
        changed_object@col_types <- paste0(rep("c", nchar(object@col_types)), collapse="")
        data <- .read_get_data_from_file(path, changed_object, nrows=1)
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
        if(object@convert_fahrenheit && mc_const_SENSOR_HOBO_T %in% names(object@columns)) {
            column_index <- object@columns[[mc_const_SENSOR_HOBO_T]]
            warning(.model_const_MESSAGE_HOBO_CONVERT_FAHRENHEIT)
            data_table[[column_index]] <- (data_table[[column_index]] - 32) * 5 / 9
        }
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

setMethod(
    ".model_check_format",
    "mc_DataFormat",
    function(object) {
        required_parameters <- c("date_column", "tz_offset")
        for(parameter_name in required_parameters) {
            if(is.na(slot(object, parameter_name))){
                stop(stringr::str_glue(.model_const_MESSAGE_ERROR_DATA_FORMAT))
            }
        }
        if(length(object@columns) == 0) {
            stop(.model_const_MESSAGE_ERROR_DATA_FORMAT_COLUMNS)
        }
    }
)

