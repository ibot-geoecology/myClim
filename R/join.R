.join_const_MESSAGE_DIFFERENT_SENSOR_ID <- "Parameters sensor_id in {logger1$metadata@serial_number}-{sensor1$metadata@name} and {logger2$metadata@serial_number}-{sensor2$metadata@name} are different."
.join_const_MESSAGE_INCONSISTENT_CALIBRATION <- "Calibration in sensors is inconsistent."
.join_const_MESSAGE_SENSORS_NOT_FOUND <- "Selected sensors not found - {sensor_name} used."
.join_const_MESSAGE_JOINING_EXIT <- "Joining canceled by user."
.join_const_MESSAGE_DIFFERENT_LOGGER_SENSORS <- "locality {locality_id}: different sensors in {logger_type} loggers -> skip joining"

.join_const_MENU_TITLE <- "Loggers are different. They cannot be joined automatically."
.join_const_MENU_INFO <- "Type choice number or type the start datetime of newer logger to be used in format YYYY-MM-DD hh:mm."
.join_const_MENU_WRONG <- "Your choice is not valid!"
.join_const_MENU_CHOICE_OLDER <- 1
.join_const_MENU_CHOICE_NEWER <- 2
.join_const_MENU_CHOICE_SKIP <- 3
.join_const_MENU_CHOICE_OLDER_ALWAYS <- 4
.join_const_MENU_CHOICE_NEWER_ALWAYS <- 5
.join_const_MENU_EXIT_CHOICE <- "6"
.join_const_MENU_CHOICES <- c("use older logger",
                              "use newer logger",
                              "skip this join",
                              "use always older logger",
                              "use always newer logger",
                              "exit")

.join_const_PLOT_NEIGHBORHOODS_DAYS <- 7
.join_const_PLOT_SIZE_OLDER <- 1.0
.join_const_PLOT_SIZE_NEWER <- 0.5

#' Joining time-series from repeated downloads
#'
#' @description
#' The function is designed to merge time-series data obtained through 
#' repeated downloads in the same location. Within a specific locality, 
#' the function performs the merging based on 1) logger type, 
#' physical element, and sensor height, or 2) based on the list of logger 
#' serial numbers to be joined, provided by user in locality metadata.  
#'
#' @details
#' Joining is restricted to the myClim Raw-format (refer to [myClim-package]). 
#' Loggers need to be organized within localities. The simplest method is to use [mc_read_data], 
#' providing `files_table` with locality IDs. When using [mc_read_files] 
#' without metadata, a bit more coding is needed. In this case, you can create 
#' multiple myClim objects and specify correct locality names afterwards, 
#' then merge these objects using [mc_prep_merge], which groups loggers 
#' based on identical locality names.
#' 
#' The joining function operates seamlessly without user intervention
#' in two scenarios: 
#' 
#' 1) when the start of a newer time series aligns with the end of an older one, and 
#' 2) when the two time-series share identical values during the overlap. 
#' 
#' However, if values differ during the overlap, the user is prompted to 
#' interactively choose which time-series to retain and which to discard. 
#' myClim provides information about differing time-series in the console, 
#' including locality ID, problematic interval (start-end), 
#' older logger ID and its time series start-end, and newer logger ID and 
#' its time series start-end. Additionally, an interactive graphical 
#' window (plotly) displays conflicting time series, allowing the user to 
#' zoom in and explore values. In case of multiple conflicts, myClim 
#' sequentially asks the user for decisions. 
#' 
#' Users have seven options for handling overlap conflicts, six of which are pre-defined.
#' The seventh option allows the user to specify the exact time
#' to trim the older time-series and use the newer one. The options include:
#' 
#' * 1: using the older logger (to resolve this conflict), 
#' * 2: using the newer logger (to resolve this conflict), 
#' * 3: skip this join (same type loggers in locality aren't joined),
#' * 4: always using the older logger (to resolve this and all other conflicts),
#' * 5: always using the newer logger (to resolve this and all other conflicts)
#' * 6: exit joining process.
#' 
#' Users must press the number key, hit Return/Enter, 
#' or write in console the exact date in the format `YYYY-MM-DD hh:mm` 
#' to trim the older series and continue with the newer series. 
#' 
#' by_type = TRUE (default)  
#' Loggers are joined based on logger type, physical element, 
#' and sensor height. This is a good option for the localities,
#' were are NOT more loggers of identical type and height recording simultaneously.
#' 
#' by_type = FALSE    
#' Loggers are joined based on the list of logger_serial belonging to each locality. 
#' User must specify in locality metadata, which logger serials are joined together. 
#' This is a good option for the localities, with more loggers of identical type and 
#' height measuring simultaneously. 
#' 
#' Loggers with multiple sensors are joined based on one or 
#' more selected sensors (see parameter comp_sensors).
#' The name of the resulting joined sensor is taken from the logger with 
#' the oldest data. If serial_number is not equal during logger joining, 
#' the resulting serial_number is NA. Clean info is changed to NA except 
#' for the step. When joining a non-calibrated sensor with a calibrated one, 
#' the calibration information must be empty in the non-calibrated sensor.
#' 
#' The `tolerance` parameter can be used for cases, when joining multiple 
#' time-series which is "almost" identical, and the difference is caused 
#' e.g. by logger precision or resolution.  
#' 
#' For example of joining see [myClim vignette](http://labgis.ibot.cas.cz/myclim/articles/myclim-demo.html). 
#' 
#' @template param_myClim_object_raw
#' @param comp_sensors senors for compare and select source logger; If NULL then first is used. (default NULL)
#' @param by_type if TRUE loggers are joined by logger type, height and physical element
#' if FALSE loggers are joined by logger `serial_number` see [mc_LoggerMetadata] (default TRUE)
#' @param tolerance list of tolerance values for each physical unit see [mc_data_physical].
#' e.g. list(T_C = 0.5). Values from  older time-series are used for overlaps below tolerance.
#' @return myClim object with joined loggers.
#' @export
mc_join <- function(data, comp_sensors=NULL, by_type=TRUE, tolerance=NULL) {
    .common_stop_if_not_raw_format(data)
    .prep_check_datetime_step_unprocessed(data, stop)
    e_state <- new.env()
    e_state$choice <- NA_integer_
    e_state$tolerance <- tolerance
    join_bar <- progress::progress_bar$new(format = "join [:bar] :current/:total localities",
                                           total=length(data$localities))
    locality_function <- function(locality) {
        groups_table <- .join_get_logger_groups_table(locality, by_type)
        group_function <- function(group_table, .y) {
            indexes <- group_table$index
            logger_type <- group_table$type[[1]]
            locality_id <- locality$metadata@locality_id
            loggers <- locality$loggers[indexes]
            can_join <- .join_can_be_joined(loggers, group_table, locality_id, logger_type, by_type)
            if(!can_join) {
                return(loggers)
            }
            result <- .join_loggers(loggers, comp_sensors, e_state, locality_id)
            return(result)
        }
        joined_loggers <- dplyr::group_map(groups_table, group_function)
        locality$loggers <- purrr::flatten(joined_loggers)
        locality <- .read_generate_logger_names(locality)
        join_bar$tick()
        return(locality)
    }
    data$localities <- purrr::map(data$localities, locality_function)
    return(data)
}

.join_get_logger_groups_table <- function(locality, by_type) {
    types <- purrr::map_chr(locality$loggers, ~ .x$metadata@type)
    serial_numbers <- purrr::map_chr(locality$loggers, ~ .x$metadata@serial_number)
    steps <- purrr::map_int(locality$loggers, ~ as.integer(.x$clean_info@step))
    shifts <- purrr::map_int(locality$loggers, ~ as.integer(.common_get_logger_shift(.x)))
    table <- tibble::tibble(index=seq_along(types),
                            serial_number=serial_numbers,
                            type=types,
                            step=steps,
                            shift=shifts)
    env_params <- new.env()
    env_params$last_group <- 0
    serial_groups <- NULL
    if(!by_type) {
        serial_groups <- .join_get_locality_serial_groups(locality)        
    }

    group_function <- function(group_table, .y) {
        result <- tibble::tibble(index=group_table$index,
                                 serial_number=group_table$serial_number,
                                 type=.y$type,
                                 step=.y$step,
                                 shift=.y$shift)
        if(by_type) {
            env_params$last_group <- env_params$last_group + 1
            result$group <- env_params$last_group
            return(result)
        }
        result <- .join_add_groups_by_serial_numbers_to_locality(result, serial_groups, env_params)
        return(result)
    }   
    group_table <- dplyr::group_by(table, .data$type, .data$step, .data$shift)
    result_tables <- dplyr::group_map(group_table, group_function)
    result <- dplyr::bind_rows(result_tables)
    result <- dplyr::group_by(result, .data$group)
    return(result)
}

.join_add_groups_by_serial_numbers_to_locality <- function(group_table, serial_groups, env_params)
{
    temp_groups_table <- tibble::tibble(serial_number=unique(group_table$serial_number))
    temp_groups_table <- dplyr::left_join(temp_groups_table, serial_groups, by="serial_number")
    unique_groups <- unique(temp_groups_table$group)
    new_groups <- seq_along(unique_groups) + env_params$last_group
    env_params$last_group <- max(new_groups)
    group_mapping <- tibble::tibble(group=unique_groups, new_group=new_groups)
    temp_groups_table <- dplyr::left_join(temp_groups_table, group_mapping, by="group")
    temp_groups_table <- dplyr::select(temp_groups_table, -"group")
    temp_groups_table <- dplyr::rename(temp_groups_table, group="new_group")
    result <- dplyr::left_join(group_table, temp_groups_table, by="serial_number")
    return(result)
}

.join_get_locality_serial_groups <- function(locality) {
    group_function <- function(serial_numbers, i) {
        return(list(serial_number=serial_numbers,
                    group=i))
    }
    group_table <- purrr::imap_dfr(locality$metadata@join_serial, group_function)
    max_group <- if (nrow(group_table) == 0) 0 else max(group_table$group)
    serial_numbers <- purrr::map_chr(locality$loggers, ~ .x$metadata@serial_number)
    result <- tibble::tibble(serial_number=serial_numbers[!is.na(serial_numbers)])
    if(nrow(group_table) == 0) {
        result$group <- NA_integer_
    } else {
        result <- dplyr::left_join(result, group_table, by="serial_number")
    }
    na_group_serial_numbers <- unique(result$serial_number[is.na(result$group)])
    na_group_table <- tibble::tibble(serial_number=na_group_serial_numbers, group_na=seq_along(na_group_serial_numbers) + max_group)
    result <- dplyr::left_join(result, na_group_table, by="serial_number")
    result$group <- ifelse(is.na(result$group), result$group_na, result$group)
    result <- dplyr::select(result, -"group_na")
    result <- dplyr::distinct(result)
    return(result)
}

.join_can_be_joined <- function(loggers, group_table, locality_id, logger_type, by_type){
    if(length(loggers) == 1) {
        return(FALSE)
    }
    if(!by_type && all(is.na(group_table$serial_number))) {
        return(FALSE)
    }
    return(.join_check_logger_sensors(loggers, locality_id, logger_type))
}

.join_check_logger_sensors <- function(loggers, locality_id, logger_type) {
    all_sensors <- as.character(unique(purrr::flatten(purrr::map(loggers, ~ names(.x$sensors)))))
    sensor_function <- function(sensor) {
        result <- all(purrr::map_lgl(loggers, ~ sensor %in% names(.x$sensors)))
        return(result)
    }
    any_sensor_in_all <- any(purrr::map_lgl(all_sensors, sensor_function))
    if(!any_sensor_in_all){
        warning(stringr::str_glue(.join_const_MESSAGE_DIFFERENT_LOGGER_SENSORS))
    }
    return(any_sensor_in_all)
}

.join_loggers <- function(loggers, comp_sensors, e_state, locality_id) {
    reduce_function <- function(logger1, logger2) {
        if(is.null(logger1)) {
            return(NULL)
        }
        if(logger2$datetime[[1]] < logger1$datetime[[1]]) {
            temp <- logger1
            logger1 <- logger2
            logger2 <- temp
        }
        sensor_names_table <- .join_get_sensor_names_table(logger1, logger2, e_state$tolerance)
        data_table <- .join_get_loggers_data_table(sensor_names_table, logger1, logger2)
        data_table <- .join_select_values(logger1, logger2, data_table, sensor_names_table, comp_sensors, e_state, locality_id)
        if(is.null(data_table)) {
            return(NULL)
        }
        return(.join_get_joined_logger(logger1, logger2, data_table, sensor_names_table))
    }
    result <- purrr::reduce(loggers, reduce_function)
    if(is.null(result)) {
        return(loggers)
    }
    return(list(result))
}

.join_get_sensor_names_table <- function(logger1, logger2, tolerance){
    l1_names <- purrr::map_chr(logger1$sensors, ~ .x$metadata@name)
    l1_heights <- purrr::map_chr(logger1$sensors, ~ .x$metadata@height)
    l1_sensor_ids <- purrr::map_chr(logger1$sensors, ~ .x$metadata@sensor_id)
    l1_physical <- purrr::map_chr(l1_sensor_ids, ~ myClim::mc_data_sensors[[.x]]@physical)
    l2_names <- purrr::map_chr(logger2$sensors, ~ .x$metadata@name)
    l2_heights <- purrr::map_chr(logger2$sensors, ~ .x$metadata@height)
    l2_sensor_ids <- purrr::map_chr(logger2$sensors, ~ .x$metadata@sensor_id)
    l2_physical <- purrr::map_chr(l2_sensor_ids, ~ myClim::mc_data_sensors[[.x]]@physical)
    l1 <- tibble::tibble(height=l1_heights, name=l1_names, physical=l1_physical, l1_new_name=paste0("l1_", l1_names))
    l2 <- tibble::tibble(height=l2_heights, name=l2_names, physical=l2_physical, l2_new_name=paste0("l2_", l2_names))
    result <- dplyr::full_join(l1, l2, by=c("height", "name", "physical"))
    result$tolerance <- NA_real_
    if(!is.null(tolerance)) {
        result$tolerance <- purrr::map_dbl(result$physical, ~ {if (.x %in% names(tolerance)) tolerance[[.x]] else NA_real_})
    }
    return(result)
}

.join_get_loggers_data_table <- function(sensor_names_table, logger1, logger2) {
    start <- logger1$datetime[[1]]
    end <- max(dplyr::last(logger1$datetime), dplyr::last(logger2$datetime))
    data_table <- tibble::tibble(datetime = seq(start, end, logger1$clean_info@step))
    l1_table <- .common_sensor_values_as_tibble(logger1)
    colnames(l1_table) <- .join_get_logger_table_column_names(colnames(l1_table), sensor_names_table, TRUE)
    data_table <- dplyr::left_join(data_table, l1_table, by="datetime")
    l2_table <- .common_sensor_values_as_tibble(logger2)
    colnames(l2_table) <- .join_get_logger_table_column_names(colnames(l2_table), sensor_names_table, FALSE)
    data_table <- dplyr::left_join(data_table, l2_table, by="datetime")
    return(data_table)
}

.join_get_logger_table_column_names <- function (old_names, names_table, is_l1) {
    result_column <- if(is_l1) "l1_new_name" else "l2_new_name"

    new_name_function <- function (old_name) {
        if(old_name == "datetime") {
            return(old_name)
        }
        row_index <- which(names_table$name == old_name)
        names_table[[result_column]][[row_index]]
    }

    purrr::map_chr(old_names, new_name_function)
}

.join_select_values <- function(logger1, logger2, data_table, sensor_names_table, comp_sensors, e_state, locality_id) {
    columns <- .join_get_compare_columns(sensor_names_table, comp_sensors)
    data_table <- .join_add_select_columns(data_table, columns)
    if(any(data_table$conflict)) {
        if(is.na(e_state$choice)) {
            choice <- .join_ask_user_choice(logger1, logger2, data_table, data_table$conflict, columns, locality_id)
            if(choice %in% c(.join_const_MENU_CHOICE_OLDER_ALWAYS, .join_const_MENU_CHOICE_NEWER_ALWAYS)) {
                e_state$choice <- choice
            }
        } else {
            choice <- e_state$choice
        }
        if(lubridate::is.POSIXct(choice)) {
            data_table$use_l1[data_table$conflict] <- TRUE
            data_table$use_l1[data_table$conflict & data_table$datetime >= choice] <- FALSE
        } else if (choice == .join_const_MENU_CHOICE_SKIP) {
            return(NULL)
        } else {
            use_l1 <- choice %in% c(.join_const_MENU_CHOICE_OLDER, .join_const_MENU_CHOICE_OLDER_ALWAYS)
            data_table$use_l1[data_table$conflict] <- use_l1
        }
    }
    return(data_table)
}

.join_add_select_columns <- function(data_table, columns) {
    equal_data <- .join_get_equal_data(data_table, columns)
    data_l1 <- purrr::reduce(purrr::map(columns$l2, ~ is.na(data_table[[.x]])), `&`) | equal_data
    data_l2 <- purrr::reduce(purrr::map(columns$l1, ~ is.na(data_table[[.x]])), `&`)
    data_table$use_l1 <- data_l1
    data_table$conflict <- !(data_l1 | data_l2)
    return(data_table)
}

.join_get_compare_columns <- function(names_table, comp_sensors) {
    not_na_sensors <- !(is.na(names_table$l1_new_name) | is.na(names_table$l2_new_name))
    l1_columns <- dplyr::first(names_table$l1_new_name[not_na_sensors])
    l2_columns <- dplyr::first(names_table$l2_new_name[not_na_sensors])
    orig <- dplyr::first(names_table$name[not_na_sensors])
    tolerance <- dplyr::first(names_table$tolerance[not_na_sensors])
    if(!is.null(comp_sensors)) {
        sensors_select <- names_table$name %in% comp_sensors
        if(!any(sensors_select)) {
            sensor_name <- dplyr::first(names_table$name[not_na_sensors])
            warning(stringr::str_glue(.join_const_MESSAGE_SENSORS_NOT_FOUND))
        } else {
            l1_columns <- names_table$l1_new_name[sensors_select]
            l2_columns <- names_table$l2_new_name[sensors_select]
            orig <- names_table$name[sensors_select]
            tolerance <- names_table$tolerance[sensors_select]
        }
    }
    tibble::tibble(l1=l1_columns, l2=l2_columns, orig=orig, tolerance=tolerance)
}

.join_get_equal_data <- function(data_table, columns) {
    compare_function <- function(l1, l2, tolerance) {
        x_is_na <- is.na(data_table[[l1]])
        y_is_na <- is.na(data_table[[l2]])
        is_both_na <- x_is_na & y_is_na
        is_one_na <- (x_is_na & !y_is_na) | (!x_is_na & y_is_na)
        if(is.na(tolerance)) {
            result <- dplyr::near(data_table[[l1]], data_table[[l2]])
        } else {
            result <- dplyr::near(data_table[[l1]], data_table[[l2]], tol=tolerance)
        }
        result[is_both_na] <- TRUE
        result[is_one_na] <- FALSE
        return(result)
    }

    is_equal <- purrr::pmap(dplyr::select(columns, "l1", "l2", "tolerance"), compare_function)
    result <- purrr::reduce(is_equal, `&`)
    result[is.na(result)] <- FALSE
    result
}

.join_ask_user_choice <- function(logger1, logger2, data_table, problems, columns, locality_id) {
    problem_interval <- lubridate::interval(min(data_table$datetime[problems]), max(data_table$datetime[problems]))
    plot_interval <- lubridate::interval(lubridate::int_start(problem_interval) - lubridate::days(.join_const_PLOT_NEIGHBORHOODS_DAYS),
                                         lubridate::int_end(problem_interval) + lubridate::days(.join_const_PLOT_NEIGHBORHOODS_DAYS))
    writeLines(stringr::str_glue("Locality: {locality_id}"))
    writeLines(stringr::str_glue("Problematic interval: {problem_interval}"))
    logger1_text <- .join_get_logger_text(logger1, TRUE)
    writeLines(logger1_text)
    .join_print_info_logger(logger1, dplyr::first(columns$orig))
    logger2_text <- .join_get_logger_text(logger2, FALSE)
    writeLines(logger2_text)
    .join_print_info_logger(logger2, dplyr::first(columns$orig))
    plot_data_table <- .join_get_plot_data(data_table, columns, plot_interval)
    highlight_data_table <- .join_get_plot_highlight_data(data_table, problems, plot_data_table, columns$orig, logger1$clean_info@step)
    y_label <- .join_get_y_label(logger1, dplyr::first(columns$orig))
    .plot_show_joining_chart(plot_data_table, stringr::str_glue("{locality_id}: {logger1_text} - {logger2_text}"),
                             y_label, c(.join_const_PLOT_SIZE_OLDER, .join_const_PLOT_SIZE_NEWER), highlight_data_table)
    .join_select_choice()
}

.join_get_logger_text <- function(logger, is_older) {
    texts <- c(if(is_older) "Older logger" else "Newer logger",
               logger$metadata@type,
               logger$metadata@serial_number)
    paste0(texts[!is.na(texts)], collapse = " ")
}

.join_print_info_logger <- function(logger, sensor_name) {
    source_states <- dplyr::filter(logger$sensors[[sensor_name]]$states, .data$tag == .model_const_SENSOR_STATE_SOURCE)
    print(tibble::tibble(source_states))
}

.join_get_plot_data <- function(data_table, columns, plot_interval) {
    problems_data_table <- dplyr::filter(data_table, lubridate::`%within%`(.data$datetime, plot_interval))
    problems_data_table <- problems_data_table[c("datetime", columns$l1, columns$l2)]
    plot_data_table <- tidyr::pivot_longer(problems_data_table, !.data$datetime)
    plot_data_table$sensor <- NA_character_
    plot_data_table$size <- NA_character_
    for(i in seq(nrow(columns))) {
        select_old <- plot_data_table$name == columns[[i, "l1"]]
        select_new <- plot_data_table$name == columns[[i, "l2"]]
        old_name <- paste0("A. Older ", columns[[i, "orig"]])
        new_name <- paste0("B. Newer ", columns[[i, "orig"]])
        plot_data_table$name[select_old] <- old_name
        plot_data_table$name[select_new] <- new_name
        plot_data_table$sensor[select_old | select_new] <- columns[[i, "orig"]]
        plot_data_table$size[select_old] <- "A"
        plot_data_table$size[select_new] <- "B"
    }
    plot_data_table <- dplyr::filter(plot_data_table, !is.na(.data$value))
    return(plot_data_table)
}

.join_get_plot_highlight_data <- function(data_table, problems, plot_data_table, sensors, step) {
    problem_intervals <- .common_get_time_series_intervals(data_table$datetime, problems)
    result_function <- function(sensor) {
        select_sensor <- plot_data_table$sensor == sensor
        tibble::tibble(start = lubridate::int_start(problem_intervals),
                       end = lubridate::int_end(problem_intervals) + lubridate::seconds(step),
                       sensor = sensor,
                       ymin = min(plot_data_table$value[select_sensor]),
                       ymax = max(plot_data_table$value[select_sensor]))
    }

    result <- purrr::map_dfr(sensors, result_function)
    result$group <- seq(nrow(result))
    result
}

.join_get_y_label <- function(logger, sensor_name) {
    sensor <- logger$sensors[[sensor_name]]
    sensor_description <- .model_get_sensor_description(sensor$metadata)
    physical_description <- .model_get_physical_description(sensor$metadata)
    if(!is.na(physical_description)) {
        return(physical_description)
    }
    if(!is.na(sensor_description)) {
        return(sensor_description)
    }
    return("Value")
}

.join_select_choice <- function() {
    writeLines(c(.join_const_MENU_TITLE, ""))
    writeLines(purrr::imap_chr(.join_const_MENU_CHOICES, ~ stringr::str_glue("{.y}: {.x}")))
    writeLines(c("", .join_const_MENU_INFO))
    while(TRUE) {
        text <- readline(stringr::str_glue("CHOICE>"))

        if(stringr::str_detect(text, "^[12345]$")) {
            return(as.integer(text))
        }
        if(text == .join_const_MENU_EXIT_CHOICE) {
            stop(.join_const_MESSAGE_JOINING_EXIT)
        }
        if(stringr::str_detect(text, "^\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}$")) {
            return(lubridate::ymd_hm(text))
        }
        writeLines(.join_const_MENU_WRONG)
    }
}

.join_get_joined_logger <- function(logger1, logger2, data_table, names_table) {
    result_logger <- logger1
    if(is.na(logger1$metadata@serial_number) || is.na(logger2$metadata@serial_number) ||
        logger1$metadata@serial_number != logger2$metadata@serial_number) {
        result_logger$metadata@serial_number <- NA_character_
    }
    result_logger$clean_info@count_duplicities <- NA_integer_
    result_logger$clean_info@count_missing <- NA_integer_
    result_logger$clean_info@count_disordered <- NA_integer_
    result_logger$clean_info@rounded <- NA
    result_logger$datetime <- data_table$datetime
    if(any(is.na(names_table$l1_new_name))) {
        na_names <- names_table$name[is.na(names_table$l1_new_name)]
        result_logger$sensors <- c(result_logger$sensors, logger2$sensors[na_names])
    }

    l1_intervals <- .common_get_time_series_intervals(data_table$datetime, data_table$use_l1)
    l2_intervals <- .common_get_time_series_intervals(data_table$datetime, !data_table$use_l1)
    l1_origin_interval <- lubridate::interval(dplyr::first(logger1$datetime), dplyr::last(logger1$datetime))
    l2_origin_interval <- lubridate::interval(dplyr::first(logger2$datetime), dplyr::last(logger2$datetime))
    sensor_function <- function (sensor_name) {
        current_name <- sensor_name == names_table$name
        l1_sensor <- NULL
        l2_sensor <- NULL
        l1_new_name <- names_table$l1_new_name[current_name]
        l2_new_name <- names_table$l2_new_name[current_name]
        if(!is.na(l1_new_name)) {
            l1_sensor <- logger1$sensors[[sensor_name]]
        }
        if(!is.na(l2_new_name)) {
            l2_sensor <- logger2$sensors[[sensor_name]]
        }
        return(.join_get_joined_sensor(l1_sensor, l2_sensor, l1_new_name, l2_new_name,
                                       l1_intervals, l2_intervals, l1_origin_interval, l2_origin_interval,
                                       data_table))
    }
    sensor_names <- names(result_logger$sensors)
    result_logger$sensors <- purrr::map(sensor_names, sensor_function)
    names(result_logger$sensors) <- sensor_names
    result_logger
}

.join_get_joined_sensor <- function(l1_sensor, l2_sensor,
                                    l1_new_name, l2_new_name,
                                    l1_intervals, l2_intervals,
                                    l1_origin_interval, l2_origin_interval,
                                    data_table) {
    is_only_one <- is.null(l1_sensor) || is.null(l2_sensor)
    result <- if(!is.null(l1_sensor)) l1_sensor else l2_sensor

    if(is_only_one) {
        new_name <- if(is.na(l1_new_name)) l2_new_name else l1_new_name
        result$values <- data_table[[new_name]]
        return(result)
    }

    if(l1_sensor$metadata@sensor_id != l2_sensor$metadata@sensor_id) {
        stop(stringr::str_glue(.join_const_MESSAGE_DIFFERENT_SENSOR_ID))
    }
    result$metadata@calibrated <- l1_sensor$metadata@calibrated || l2_sensor$metadata@calibrated
    result$calibration <- .join_get_sensors_calibration(l1_sensor, l2_sensor,
                                                        l1_origin_interval, l2_origin_interval,
                                                        l1_intervals, l2_intervals)
    result$states <- .join_get_sensors_states(l1_sensor, l2_sensor, l1_intervals, l2_intervals)

    result$values <- data_table[[l2_new_name]]
    result$values[data_table$use_l1] <- data_table[[l1_new_name]][data_table$use_l1]
    result
}

.join_get_sensors_calibration <- function(l1_sensor, l2_sensor, l1_origin_interval, l2_origin_interval, l1_intervals, l2_intervals) {
    if((l1_sensor$metadata@calibrated && !l2_sensor$metadata@calibrated && nrow(l2_sensor$calibration) > 0) ||
       (!l1_sensor$metadata@calibrated && l2_sensor$metadata@calibrated && nrow(l1_sensor$calibration) > 0)) {
        stop(.join_const_MESSAGE_INCONSISTENT_CALIBRATION)
    }

    l1_calibration <- .join_get_sensor_calibration(l1_sensor, l1_origin_interval, l1_intervals)
    l2_calibration <- .join_get_sensor_calibration(l2_sensor, l2_origin_interval, l2_intervals)
    calibration <- dplyr::arrange(dplyr::bind_rows(l1_calibration, l2_calibration), .data$datetime)
    duplicated_rows <- .common_duplicated_abreast(calibration$cor_factor) & .common_duplicated_abreast(calibration$cor_slope)
    calibration <- calibration[!duplicated_rows, ]
    as.data.frame(calibration)
}

.join_get_sensor_calibration <- function(sensor, origin_data_interval, intervals) {
    calibration_function <- function(start, end, cor_factor, cor_slope) {
        calib_interval <- lubridate::interval(start, end)
        result_intervals <- lubridate::intersect(calib_interval, intervals)
        result_intervals <- purrr::discard(result_intervals, ~ is.na(.x))
        if(length(result_intervals) == 0) {
            return(tibble::tibble())
        }
        result <- tibble::tibble(datetime=lubridate::int_start(result_intervals), cor_factor=cor_factor, cor_slope=cor_slope)
        return(result)
    }

    if(nrow(sensor$calibration) == 0) {
        return(tibble::tibble(datetime=lubridate::int_start(intervals), cor_factor=NA_real_, cor_slope=NA_real_))
    }

    calibration <- sensor$calibration

    if(calibration$datetime[[1]] > lubridate::int_start(origin_data_interval)) {
        calibration$datetime[[1]] <- lubridate::int_start(origin_data_interval)
    }

    later_calibrations_selector <- calibration$datetime <= lubridate::int_end(origin_data_interval)
    calibration <- calibration[later_calibrations_selector, ]

    starts <- calibration$datetime
    ends <- c(calibration$datetime[-1], lubridate::int_end(origin_data_interval))
    cor_factor <- calibration$cor_factor
    cor_slope <- calibration$cor_slope

    return(purrr::pmap_dfr(list(start=starts,
                                end=ends,
                                cor_factor=calibration$cor_factor,
                                cor_slope=calibration$cor_slope), calibration_function))
}

.join_get_sensors_states <- function(l1_sensor, l2_sensor, l1_intervals, l2_intervals) {
    l1_sensor_states <- .common_crop_states_table(l1_sensor$states, l1_intervals)
    l2_sensor_states <- .common_crop_states_table(l2_sensor$states, l2_intervals)
    as.data.frame(dplyr::bind_rows(l1_sensor_states, l2_sensor_states))
}

