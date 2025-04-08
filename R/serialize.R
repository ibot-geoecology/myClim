.serialize_const_SKIP_MESSAGE <- "Data in file {file_item} is not myClim object and will be skipped."

.serialize_MAP_1_0_6 <- new.env()
.serialize_MAP_1_0_6$loggers <- new.env()
.serialize_MAP_1_0_6$loggers$ThermoDatalogger <- "Thermo"
.serialize_MAP_1_0_6$loggers$Dendrometer <- "Dendro"
.serialize_MAP_1_0_6$sensor_ids <- new.env()
.serialize_MAP_1_0_6$sensor_ids$TMS_TMSmoisture <- "TMS_moist"
.serialize_MAP_1_0_6$sensor_ids$TS_T <- "Thermo_T"
.serialize_MAP_1_0_6$sensor_ids$DEND_T <- "Dendro_T"
.serialize_MAP_1_0_6$sensor_ids$DEND_TOMSTdendro <- "Dendro_raw"
.serialize_MAP_1_0_6$sensor_ids$HOBO_T_F <- "real"
.serialize_MAP_1_0_6$sensor_ids$HOBO_T_C <- "HOBO_T"
.serialize_MAP_1_0_6$sensor_ids$moisture <- "VWC"
.serialize_MAP_1_0_6$sensor_ids$RH_perc <- "RH"
.serialize_MAP_1_0_6$sensor_ids$wind <- "wind_speed"


#' Save myClim object
#'
#' This function was designed for saving the myClim data object to an 
#' .rds file, which can be later correctly loaded by any further version 
#' of myClim package with [mc_load]. This is the safest way how to store and 
#' share your myClim data.
#'
#' @template param_myClim_object
#' @param file path to output .rds file
#' @return RDS file saved at the output path destination
#' @examples
#' tmp_dir <- tempdir()
#' tmp_file <- tempfile(tmpdir = tmp_dir)
#' mc_save(mc_data_example_agg, tmp_file)
#' file.remove(tmp_file)
#' @export
mc_save <- function(data, file) {
    output_object <- .save_convert_classes_to_lists(data)
    saveRDS(output_object, file=file)
}

.save_convert_classes_to_lists <- function(data) {
    get_items <- function(item) {
        if(is(item, "list")) {
            return(.save_convert_classes_to_lists(item))
        } else if(is(item, "mc_Serializable")) {
            return(.model_object_to_list(item))
        }
        return(item)
    }
    return(purrr::map(data, get_items))
}

#' Load myClim object
#'
#' This function loads the myClim .rds data object saved with [mc_save].
#' The `mc_save` and `mc_load` functions secure that the myClim object is correctly 
#' loaded across myClim versions.
#'
#' @param file path to input .rds file. If value is vector of files, myClim objects
#' are merged with function [mc_prep_merge]. If path is directory, then all .rds files are used.
#' @return loaded myClim object
#' @examples
#' tmp_dir <- tempdir()
#' tmp_file <- tempfile(tmpdir = tmp_dir)
#' mc_save(mc_data_example_agg, tmp_file)
#' data <- mc_load(tmp_file)
#' file.remove(tmp_file)
#' @export
mc_load <- function(file) {
    expand_directory_function <- function(file_item) {
        if(!file.info(file_item)$isdir) {
            return(list(file_item))
        }
        rds_files <- list.files(file_item, pattern = "\\.rds$", full.names = TRUE)
        return(rds_files)
    }

    file_items <- purrr::flatten(purrr::map(file, expand_directory_function))

    load_bar <- progress::progress_bar$new(format = "load [:bar] :current/:total files",
                                           total=length(file_items))
    load_bar$tick(0)
    file_function <- function(file_item) {
        obj_list <- readRDS(file=file_item)
        if(!.can_be_myClim_object(obj_list)) {
            warning(stringr::str_glue(.serialize_const_SKIP_MESSAGE))
            load_bar$tick()
            return(NULL)
        }
        result_object <- .load_convert_lists_to_classes(obj_list)
        load_bar$tick()
        return(result_object)
    }

    data_items <- purrr::map(file_items, file_function)
    data_items <- purrr::discard(data_items, is.null)
    return(mc_prep_merge(data_items))
}

.can_be_myClim_object <- function(obj_list) {
    two_items <- length(obj_list) == 2
    has_metadata <- "metadata" %in% names(obj_list)    
    has_localities <- "localities" %in% names(obj_list)
    return(two_items && has_metadata && has_localities)
}

.load_convert_lists_to_classes <- function(obj_list) {
    is_raw <- obj_list$metadata$format_type == "raw"

    sensor_function <- function(item) {
        item <- .serialize_edit_sensor_list_before_load(item, obj_list$metadata$version)
        metadata <- .model_list_to_object(item$metadata)
        return(list(metadata=metadata, values=item$values,
                    calibration=item$calibration, states=item$states))
    }

    logger_function <- function(item) {
        item <- .serialize_edit_logger_list_before_load(item, obj_list$metadata$version)
        metadata <- .model_list_to_object(item$metadata)
        clean_info <- .model_list_to_object(item$clean_info)
        sensors <- purrr::map(item$sensors, sensor_function)
        return(list(metadata=metadata, clean_info=clean_info, datetime=item$datetime, sensors=sensors))
    }

    locality_function <- function(item) {
        item <- .serialize_edit_locality_list_before_load(item, obj_list$metadata$version)
        metadata <- .model_list_to_object(item$metadata)
        if(is_raw) {
            loggers <- purrr::map(item$loggers, logger_function)
            result <- list(metadata=metadata, loggers=loggers)
            result <- .serialize_edit_raw_locality_list_after_load(result, obj_list$metadata$version)
            return(result)
        }
        sensors <- purrr::map(item$sensors, sensor_function)
        return(list(metadata=metadata, datetime=item$datetime, sensors=sensors))
    }

    localities <- purrr::map(obj_list$localities, locality_function)

    class_name <- if(is_raw) "mc_MainMetadata" else "mc_MainMetadataAgg"
    main_metadata <- .model_list_to_object(obj_list$metadata)

    return(myClimList(main_metadata, localities))
}

.serialize_edit_locality_list_before_load <- function(item, original_version) {
    if(original_version < "0.2.6") {
        item$metadata$elevation <- item$metadata$altitude
        item$metadata <- item$metadata[names(item$metadata) != "altitude"]
    }
    return(item)
}

.serialize_edit_raw_locality_list_after_load <- function(item, original_version) {
    if(original_version < "1.3.4") {
        item <- .read_generate_logger_names(item)
    }
    return(item)
}

.serialize_edit_logger_list_before_load <- function(item, original_version) {
    if(original_version < "1.4.2") {
        if(is.na(item$clean_info$step)) {
            item$metadata$raw_index <- seq_along(item$datetime)
        } else {
            item$metadata$raw_index <- NA_integer_
        }
    }
    if(original_version < "1.1.0") {
        if(item$metadata$type == "HOBO") {
            item$metadata$type <- .model_const_LOGGER_HOBO_U23_001A
        }
    }
    if(original_version < "1.0.6") {
        if(item$metadata$type %in% names(.serialize_MAP_1_0_6$loggers)) {
            item$metadata$type <- .serialize_MAP_1_0_6$loggers[[item$metadata$type]]
        }
    }
    return(item)
}

.serialize_edit_sensor_list_before_load <- function(item, original_version) {
    if(original_version < "1.0.6") {
        if(item$metadata$sensor_id %in% names(.serialize_MAP_1_0_6$sensor_ids)) {
            item$metadata$sensor_id <- .serialize_MAP_1_0_6$sensor_ids[[item$metadata$sensor_id]]
        }
    }
    return(item)
}

#' Save myClim object separated by localities
#'
#' This function was designed for saving the myClim data object to multiple
#' .rds files, which every contains data of one locality. Every file is named by `locality_id`.
#'
#' @template param_myClim_object
#' @param directory path to output directory
#' @return RDS files saved at the output path destination
#' @examples
#' tmp_dir <- tempdir()
#' tmp_dir <- file.path(tmp_dir, "localities")
#' dir.create(tmp_dir)
#' mc_save_localities(mc_data_example_agg, tmp_dir)
#' unlink(tmp_dir, recursive = TRUE)
#' @export
mc_save_localities <- function(data, directory) {
    save_bar <- progress::progress_bar$new(format = "save [:bar] :current/:total localities",
                                           total=length(data$localities))
    save_bar$tick(0)
    locality_function <- function(locality_id) {
        locality_data <- mc_filter(data, localities=locality_id)
        locality_file <- file.path(directory, paste0(locality_id, ".rds"))
        mc_save(locality_data, locality_file)
        save_bar$tick()
    }
    purrr::walk(names(data$localities), locality_function)
}
