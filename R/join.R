.join_const_MESSAGE_DIFFERENT_SENSOR_ID <- "Parameters sensor_id in {logger1$metadata@serial_number}-{sensor1$metadata@name} and {logger2$metadata@serial_number}-{sensor2$metadata@name} are different."
.join_const_MESSAGE_INCONSISTENT_CALIBRATION <- "Calibration in sensors is inconsistent."
.join_const_MESSAGE_SENSORS_NOT_FOUND <- "Selected sensors not found - {sensor_name} used."
.join_const_MESSAGE_JOINING_EXIT <- "Joining exited by user."

.join_const_MENU_TITLE <- "Loggers are different. They cannot be joined automatically."
.join_const_MENU_CHOICE_OLDER <- 1
.join_const_MENU_CHOICE_NEWER <- 2
.join_const_MENU_CHOICE_OLDER_ALWAYS <- 3
.join_const_MENU_CHOICE_NEWER_ALWAYS <- 4
.join_const_MENU_EXIT_CHOICES <- c(0, 5)
.join_const_MENU_CHOICES <- c("use older logger",
                              "use newer logger",
                              "use always older logger",
                              "use always newer logger",
                              "exit")

.join_const_PLOT_NEIGHBORHOODS_DAYS <- 7

#' Joining sensors from different loggers
#'
#' @description
#' Function join sensors from different loggers. Loggers with same type [mc_LoggerMetadata] and step are joined.
#' Every sensore from first logger must have pair sensor from second one with same height. If sensor values are different,
#' then user interactively select source logger.
#'
#' @details
#' Name of result sensor is used from logger with older data. If `serial_number` is not equal in joining loggers, then
#' result `serial_number` is `NA`. Clean info is changed to `NA` except step. If uncalibrated sensor is joining with calibrated one,
#' then calibration inforamtion must be empty for uncalibrated sensor.
#'
#' @param data myClim object in Prep-format. See [myClim-package]
#' @param comp_sensors senors for compare and select source logger; If NULL then first is used. (default NULL)
#' @return myClim object with joined loggers.
#' @export
#' @examples
mc_join <- function(data, comp_sensors=NULL) {
    myClim:::.common_stop_if_not_prep_format(data)
    myClim:::.prep_check_datetime_step_unprocessed(data, stop)
    locality_function <- function(locality) {
        types <- purrr::map_chr(locality$loggers, ~ .x$metadata@type)
        unique_types <- unique(types)
        type_function <- function(logger_type) {
            indexes <- which(types == logger_type)
            if(length(indexes) == 1) {
                return(locality$loggers[[indexes]])
            }
            .join_loggers_same_type(locality$loggers[indexes], comp_sensors, locality$metadata@locality_id)
        }
        locality$loggers <- purrr::flatten(purrr::map(unique_types, type_function))
        locality
    }
    purrr::map(data, locality_function)
}

.join_loggers_same_type <- function(loggers, comp_sensors, locality_id) {
    steps <- purrr::map_int(loggers, ~ as.integer(.x$clean_info@step))
    shifts <- purrr::map_int(loggers, ~ as.integer(myClim:::.common_get_logger_shift(.x)))
    heights <- .join_get_heights(loggers)
    table <- tibble::tibble(logger_id=seq_along(loggers), steps=steps, shifts=shifts, heights=heights)
    table <- dplyr::group_by(table, steps, shifts, heights)
    e_choice <- new.env()
    e_choice$choice <- NA_integer_
    group_function <- function(group, .y) {
        .join_loggers(loggers[group$logger_id], comp_sensors, e_choice, locality_id)
    }
    dplyr::group_map(table, group_function)
}

.join_get_heights <- function(loggers) {
    logger_function <- function(logger) {
        stringr::str_c(sort(purrr::map_chr(logger$sensors, ~ .x$metadata@height)), collapse=",")
    }

    purrr::map_chr(loggers, logger_function)
}

.join_loggers <- function(loggers, comp_sensors, e_choice, locality_id) {
    reduce_function <- function(logger1, logger2) {
        if(logger2$datetime[[1]] < logger1$datetime[[1]]) {
            temp <- logger1
            logger1 <- logger2
            logger2 <- temp
        }
        start <- logger1$datetime[[1]]
        end <- max(dplyr::last(logger1$datetime), dplyr::last(logger2$datetime))
        data_table <- tibble::tibble(datetime = seq(start, end, logger1$clean_info@step * 60))
        names_table <- .join_get_names_table(logger1, logger2)
        l1_table <- myClim:::.common_sensor_values_as_tibble(logger1)
        colnames(l1_table) <- .join_get_logger_table_column_names(colnames(l1_table), names_table, TRUE)
        data_table <- dplyr::left_join(data_table, l1_table, by="datetime")
        l2_table <- myClim:::.common_sensor_values_as_tibble(logger2)
        colnames(l2_table) <- .join_get_logger_table_column_names(colnames(l2_table), names_table, FALSE)
        data_table <- dplyr::left_join(data_table, l2_table, by="datetime")
        data_table <- .join_add_select_column(logger1, logger2, data_table, names_table, comp_sensors, e_choice, locality_id)
        .join_get_joined_logger(logger1, logger2, data_table, names_table)
    }
    purrr::reduce(loggers, reduce_function)
}

.join_get_names_table <- function(logger1, logger2){
    l1_names <- purrr::map_chr(logger1$sensors, ~ .x$metadata@name)
    l1_heights <- purrr::map_chr(logger1$sensors, ~ .x$metadata@height)
    l2_names <- purrr::map_chr(logger2$sensors, ~ .x$metadata@name)
    l2_heights <- purrr::map_chr(logger2$sensors, ~ .x$metadata@height)
    l1 <- tibble::tibble(height=l1_heights, l1_name=l1_names, l1_new_name=paste0("l1_", l1_names))
    l2 <- tibble::tibble(height=l2_heights, l2_name=l2_names, l2_new_name=paste0("l2_", l2_names))
    dplyr::left_join(l1, l2, by="height")
}

.join_get_logger_table_column_names <- function (old_names, names_table, is_l1) {
    search_column <- if(is_l1) "l1_name" else "l2_name"
    result_column <- if(is_l1) "l1_new_name" else "l2_new_name"

    new_name_function <- function (old_name) {
        if(old_name == "datetime") {
            return(old_name)
        }
        row_index <- which(names_table[[search_column]] == old_name)
        names_table[[result_column]][[row_index]]
    }

    purrr::map_chr(old_names, new_name_function)
}

.join_add_select_column <- function(logger1, logger2, data_table, names_table, comp_sensors, e_choice, locality_id) {
    columns <- .join_get_compare_columns(names_table, comp_sensors)
    equal_data <- .join_get_equal_data(data_table, columns)
    data_l1 <- purrr::reduce(purrr::map(columns$l2, ~ is.na(data_table[[.x]])), `&`) | equal_data
    data_l2 <- purrr::reduce(purrr::map(columns$l1, ~ is.na(data_table[[.x]])), `&`)
    data_table$use_l1 <- data_l1
    problems <- !(data_l1 | data_l2)
    if(any(problems)) {
        if(is.na(e_choice$choice)) {
            choice <- .join_ask_user_choice(logger1, logger2, data_table, problems, columns, locality_id)
            if(choice %in% c(.join_const_MENU_CHOICE_OLDER_ALWAYS, .join_const_MENU_CHOICE_NEWER_ALWAYS)) {
                e_choice$choice <- choice
            }
        } else {
            choice <- e_choice$choice
        }
        use_l1 <- choice %in% c(.join_const_MENU_CHOICE_OLDER, .join_const_MENU_CHOICE_OLDER_ALWAYS)
        data_table$use_l1[problems] <- use_l1
    }
    data_table
}

.join_get_compare_columns <- function(names_table, comp_sensors) {
    l1_columns <- dplyr::first(names_table$l1_new_name)
    l1_orig <- dplyr::first(names_table$l1_name)
    l2_columns <- dplyr::first(names_table$l2_new_name)
    l2_orig <- dplyr::first(names_table$l2_name)
    if(!is.null(comp_sensors)) {
        sensors_select <- names_table$l1_name %in% comp_sensors | names_table$l2_name %in% comp_sensors
        if(!any(sensors_select)) {
            sensor_name <- dplyr::first(names_table$l1_name)
            warning(stringr::str_glue(.join_const_MESSAGE_SENSORS_NOT_FOUND))
        } else {
            l1_columns <- names_table$l1_new_name[sensors_select]
            l1_orig <- names_table$l1_name[sensors_select]
            l2_columns <- names_table$l2_new_name[sensors_select]
            l2_orig <- names_table$l2_name[sensors_select]
        }
    }
    tibble::tibble(l1=l1_columns, l1_orig=l1_orig, l2=l2_columns, l2_orig=l2_orig)
}

.join_get_equal_data <- function(data_table, columns) {
    is_equal <- purrr::map2(columns$l1, columns$l2, ~ dplyr::near(data_table[[.x]], data_table[[.y]]))
    result <- purrr::reduce(is_equal, `&`)
    result[is.na(result)] <- FALSE
    result
}

.join_ask_user_choice <- function(logger1, logger2, data_table, problems, columns, locality_id) {
    problem_interval <- lubridate::interval(min(data_table$datetime[problems]), max(data_table$datetime[problems]))
    plot_interval <- lubridate::interval(lubridate::int_start(problem_interval) - lubridate::days(.join_const_PLOT_NEIGHBORHOODS_DAYS),
                                         lubridate::int_start(problem_interval) + lubridate::days(.join_const_PLOT_NEIGHBORHOODS_DAYS))
    print(stringr::str_glue("Locality: {locality_id}"))
    print(stringr::str_glue("Problematic interval: {problem_interval}"))
    logger1_text <- .join_get_logger_text(logger1, TRUE)
    print(logger1_text)
    .join_print_info_logger(logger1, dplyr::first(columns$l1_orig))
    logger2_text <- .join_get_logger_text(logger2, FALSE)
    print(logger2_text)
    .join_print_info_logger(logger2, dplyr::first(columns$l2_orig))
    problems_data_table <- dplyr::filter(data_table, lubridate::`%within%`(datetime, plot_interval))
    problems_data_table <- problems_data_table[c("datetime", columns$l1, columns$l2)]
    colnames(problems_data_table) <- c("datetime", paste0("Older ", columns$l1_orig), paste0("Newer ", columns$l2_orig))
    plot_data_table <- tidyr::pivot_longer(problems_data_table, !datetime)
    plot_data_table <- dplyr::filter(plot_data_table, !is.na(value))
    y_label <- .join_get_y_label(logger1, dplyr::first(columns$l1_orig))
    #myClim:::
    .plot_show_joining_chart(plot_data_table, stringr::str_glue("{locality_id}: {logger1_text} - {logger2_text}"), y_label)
    choice <- utils::menu(.join_const_MENU_CHOICES, title=.join_const_MENU_TITLE)
    if(choice %in% .join_const_MENU_EXIT_CHOICES) {
        stop(.join_const_MESSAGE_JOINING_EXIT)
    }
    choice
}

.join_get_logger_text <- function(logger, is_older) {
    texts <- c(if(is_older) "Older logger" else "Newer logger",
               logger$metadata@type,
               logger$metadata@serial_number)
    paste0(texts[!is.na(texts)], collapse = " ")
}

.join_print_info_logger <- function(logger, sensor_name) {
    source_states <- dplyr::filter(logger$sensors[[sensor_name]]$states, tag == myClim:::.model_const_SENSOR_STATE_SOURCE)
    print(source_states)
}

.join_get_y_label <- function(logger, sensor_name) {
    sensor <- logger$sensors[[sensor_name]]
    sensor_description <- myClim:::.model_get_sensor_description(sensor$metadata)
    physical_description <- myClim:::.model_get_physical_description(sensor$metadata)
    if(!is.na(physical_description)) {
        return(physical_description)
    }
    if(!is.na(sensor_description)) {
        return(sensor_description)
    }
    return("Value")
}

.join_get_joined_logger <- function(logger1, logger2, data_table, names_table) {
    result_logger <- logger1
    if(is.na(logger1$metadata@serial_number) || is.na(logger2$metadata@serial_number) ||
        logger1$metadata@serial_number != logger2$metadata@serial_number) {
        result_logger$metadata@serial_number <- NA_character_
    }
    result_logger$clean_info@count_duplicits <- NA_integer_
    result_logger$clean_info@count_missed <- NA_integer_
    result_logger$clean_info@count_disordered <- NA_integer_
    result_logger$clean_info@rounded <- NA
    result_logger$datetime <- data_table$datetime

    l1_intervals <- myClim:::.common_get_time_series_intervals(data_table$datetime, data_table$use_l1)
    l2_intervals <- myClim:::.common_get_time_series_intervals(data_table$datetime, !data_table$use_l1)
    l1_origin_interval <- lubridate::interval(dplyr::first(logger1$datetime), dplyr::last(logger1$datetime))
    l2_origin_interval <- lubridate::interval(dplyr::first(logger2$datetime), dplyr::last(logger2$datetime))
    sensor_function <- function (l1_sensor_name) {
        l1_sensor <- result_logger$sensors[[l1_sensor_name]]
        current_name <- l1_sensor_name == names_table$l1_name
        l2_sensor_name <- unname(names_table$l2_name[current_name])
        l2_sensor <- logger2$sensors[[l2_sensor_name]]
        if(l1_sensor$metadata@sensor_id != l2_sensor$metadata@sensor_id) {
            stop(stringr::str_glue(.join_const_MESSAGE_DIFFERENT_SENSOR_ID))
        }
        result <- l1_sensor
        result$metadata@calibrated <- l1_sensor$metadata@calibrated || l2_sensor$metadata@calibrated
        result$calibration <- .join_get_sensors_calibration(l1_sensor, l2_sensor,
                                                            l1_origin_interval, l2_origin_interval,
                                                            l1_intervals, l2_intervals)
        result$states <- .join_get_sensors_states(l1_sensor, l2_sensor, l1_intervals, l2_intervals)
        result$values <- data_table[[names_table$l2_new_name[current_name]]]
        result$values[data_table$use_l1] <- data_table[[names_table$l1_new_name[current_name]]][data_table$use_l1]
        result
    }
    sensor_names <- names(result_logger$sensors)
    result_logger$sensors <- purrr::map(sensor_names, sensor_function)
    names(result_logger$sensors) <- sensor_names
    result_logger
}

.join_get_sensors_calibration <- function(l1_sensor, l2_sensor, l1_origin_interval, l2_origin_interval, l1_intervals, l2_intervals) {
    if((l1_sensor$metadata@calibrated && !l2_sensor$metadata@calibrated && nrow(l2_sensor$calibration) > 0) ||
       (!l1_sensor$metadata@calibrated && l2_sensor$metadata@calibrated && nrow(l1_sensor$calibration) > 0)) {
        stop(.join_const_MESSAGE_INCONSISTENT_CALIBRATION)
    }

    l1_calibration <- .join_get_sensor_calibration(l1_sensor, l1_origin_interval, l1_intervals)
    l2_calibration <- .join_get_sensor_calibration(l2_sensor, l2_origin_interval, l2_intervals)
    calibration <- dplyr::arrange(dplyr::bind_rows(l1_calibration, l2_calibration), datetime)
    na_calibration <- is.na(calibration$cor_factor) & is.na(calibration$cor_slope)
    rle_na <- rle(na_calibration)
    if(rle_na$values[[1]]) {
        calibration <- dplyr::filter(calibration, dplyr::row_number() > rle_na$lengths[[1]])
    }
    as.data.frame(calibration)
}

.join_get_sensor_calibration <- function(sensor, origin_data_interval, intervals) {
    calibration_function <- function(start, end, cor_factor, cor_slope) {
        calib_interval <- lubridate::interval(start, end)
        result_intervals <- lubridate::intersect(calib_interval, intervals)
        if(length(result_intervals) == 0) {
            return(tibble::tibble())
        }
        tibble::tibble(datetime=lubridate::int_start(result_intervals), cor_factor=cor_factor, cor_slope=cor_slope)
    }

    if(nrow(sensor$calibration) == 0) {
        return(tibble::tibble(datetime=lubridate::int_start(intervals), cor_factor=NA_real_, cor_slope=NA_real_))
    }

    starts <- sensor$calibration$datetime
    ends <- c(sensor$calibration$datetime[-1], lubridate::int_end(origin_data_interval))
    cor_factor <- sensor$calibration$cor_factor
    cor_slope <- sensor$calibration$cor_slope
    if(starts[[1]] > lubridate::int_start(origin_data_interval)) {
        starts <- c(lubridate::int_start(origin_data_interval), starts)
        ends <- c(starts[2], ends)
        cor_factor <- c(NA_real_, sensor$calibration$cor_factor)
        cor_slope <- c(NA_real_, sensor$calibration$cor_slope)
    }

    return(purrr::pmap_dfr(list(start=starts,
                                end=ends,
                                cor_factor=sensor$calibration$cor_factor,
                                cor_slope=sensor$calibration$cor_slope), calibration_function))
}

.join_get_sensors_states <- function(l1_sensor, l2_sensor, l1_intervals, l2_intervals) {
    l1_sensor_states <- myClim:::.common_crop_states_table(l1_sensor$states, l1_intervals)
    l2_sensor_states <- myClim:::.common_crop_states_table(l2_sensor$states, l2_intervals)
    as.data.frame(dplyr::bind_rows(l1_sensor_states, l2_sensor_states))
}

