#' Save myClim object
#'
#' §This function save the myClim data object to an RDS file. The standard R functions save and saveRDS are unsafe
#' to save the MyClim object. The mc_save a mc_load functions ensure that the myClim object is correctly loaded
#' in the newer version.§
#'
#' @param data myClim object see [myClim-package]
#' @param file path to output RDS file
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
            return(myClim:::.model_object_to_list(item))
        }
        return(item)
    }
    return(purrr::map(data, get_items))
}

#' Load myClim object
#'
#' §This function load the myClim data object from an RDS file. The standard R functions load and readRDS are unsafe
#' to load the MyClim object. The mc_save a mc_load functions ensure that the myClim object is correctly loaded
#' in the newer version.§
#'
#' @param file path to input RDS file
#' @export
mc_load <- function(file) {
    obj_list <- readRDS(file=file)
    return(.load_convert_lists_to_classes(obj_list))
}

.load_convert_lists_to_classes <- function(obj_list) {
    is_raw <- obj_list$metadata$format_type == "raw"

    sensor_function <- function(item) {
        metadata <- myClim:::.model_list_to_object(item$metadata)
        return(list(metadata=metadata, values=item$values,
                    calibration=item$calibration, states=item$states))
    }

    logger_function <- function(item) {
        metadata <- myClim:::.model_list_to_object(item$metadata)
        clean_info <- myClim:::.model_list_to_object(item$clean_info)
        sensors <- purrr::map(item$sensors, sensor_function)
        return(list(metadata=metadata, clean_info=clean_info, datetime=item$datetime, sensors=sensors))
    }

    locality_function <- function(item) {
        metadata <- myClim:::.model_list_to_object(item$metadata)
        if(is_raw) {
            loggers <- purrr::map(item$loggers, logger_function)
            return(list(metadata=metadata, loggers=loggers))
        }
        sensors <- purrr::map(item$sensors, sensor_function)
        return(list(metadata=metadata, datetime=item$datetime, sensors=sensors))
    }

    localities <- purrr::map(obj_list$localities, locality_function)

    class_name <- if(is_raw) "mc_MainMetadata" else "mc_MainMetadataAgg"
    main_metadata <- myClim:::.model_list_to_object(obj_list$metadata)

    return(myClimList(main_metadata, localities))
}

