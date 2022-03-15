.plot_const_MOISTURE_PHYSICAL <- c(myClim:::.model_const_PHYSICAL_TMSmoisture,
                                   myClim:::.model_const_PHYSICAL_moisture)

#' Plot data from loggers
#'
#' Function plot loggers to directory
#'
#' @param data in format for preparing
#' @param directory output directory
#' @param localities names of localities; if NULL then all
#' @param sensors names of sensors; if NULL then all
#' @param crop datetime range for plot, not cropping if NA (default c(NA, NA))
#' @export
#' @examples
#' mc_plot_loggers(example_tomst_data1, "Figures")
mc_plot_loggers <- function(data, directory, localities=NULL, sensors=NULL, crop=c(NA, NA)) {
    myClim:::.common_stop_if_not_prep_format(data)
    data <- mc_filter(data, localities, sensors)
    myClim:::.prep_warn_if_datetime_step_unprocessed(data)
    loggers <- myClim:::.common_get_loggers(data)
    dir.create(directory, showWarnings = F)
    for(logger in loggers) {
        filename <- file.path(directory, paste0(logger$metadata@serial_number, ".png"))
        .plot_logger(logger, filename, crop)
    }
}

.plot_logger <- function(logger, filename, crop=c(NA, NA)) {
    png(filename=filename, width=1920, height=1000, res=200)
    physical <- .plot_get_logger_sensors_by_physical(logger)
    moisture_physical <- intersect(.plot_const_MOISTURE_PHYSICAL, names(physical))
    .plot_logger_set_parameters(physical, moisture_physical)
    xlimit <- .plot_get_xlimit(logger$datetime, crop)
    months <- .plot_get_months_from_xlimit(xlimit)
    .plot_logger_temperature(logger, xlimit, months, physical[[myClim:::.model_const_PHYSICAL_T_C]])
    if(length(moisture_physical) > 0){
        .plot_logger_moisture(logger, xlimit, months, physical[[moisture_physical[[1]]]])
    }
    axis.POSIXct(1, at=months, labels=T, format="%m/%Y", las=2)
    dev.off()
}

.plot_get_logger_sensors_by_physical <- function(logger) {
    physical <- sapply(logger$sensors, function(x) {
        myClim:::.common_get_sensor_info(x$metadata)@physical})
    sensor_names <- names(logger$sensors)
    tapply(sensor_names, physical, c, simplify = FALSE)
}

.plot_logger_set_parameters <-function(physical_quantities, moisture_sensor)
{
    top_margin <- 1.5
    right_margin <- 8
    left_margin <- 5
    if(length(moisture_sensor) > 0) {
        layout(matrix(1:2))
        bottom_margin <- 0
    }
    else {
        bottom_margin <- 5
    }
    par(mar=c(bottom_margin, left_margin, top_margin, right_margin), mgp=c(2.5,1,0), las=1)
}

.plot_get_xlimit <- function(datetime, crop){
    result <- crop
    if(is.na(result[[1]])) {
        result[[1]] <- min(datetime)
    }
    if(is.na(result[[2]])) {
        result[[2]] <- max(datetime)
    }
    myClim:::.common_as_utc_posixct(result)
}

.plot_logger_temperature <- function(logger, xlimit, months, sensors)
{
    if(is.null(sensors)) {
        ylimit <- c(-15, 30)
    }
    else {
        sensor_info <- sapply(sensors, function(x) {
            myClim:::.common_get_sensor_info(logger$sensors[[x]]$metadata)})
        values_range <- .plot_get_values_range(logger, sensors)
        ylimit <- c(min(c(-15, values_range[[1]]), na.rm=T), max(c(30, values_range[[2]]), na.rm=T))
    }
    plot(logger$datetime, rep(NA, length(logger$datetime)), type="n", xaxt="n", xlab=NA, ylab="Temperature (°C)",
         main=logger$metadata@serial_number, xlim=xlimit, ylim=ylimit)
    abline(v=months, lty=1, col=gray(0.9))
    grid(nx=NA, ny=NULL, col=gray(0.9), lty=1)
    abline(0,0,col="gray70")
    box()
    for(sensor_name in sensors) {
        lines(logger$datetime, logger$sensors[[sensor_name]]$values, col=sensor_info[[sensor_name]]@plot_color,
              lwd=sensor_info[[sensor_name]]@plot_line_width)
    }
    colors <- sapply(sensors, function(x) sensor_info[[x]]@plot_color)
    line_widths <- sapply(sensors, function(x) sensor_info[[x]]@plot_line_width)
    legend(grconvertX(1610, "device"), grconvertY(60, "device"), lwd=line_widths,
           sensors, lty = rep(1, length(sensors)), col=colors, xpd=NA)
}

.plot_logger_moisture <- function(logger, xlimit, months, sensor)
{
    sensor_info <- myClim:::.common_get_sensor_info(logger$sensors[[sensor]]$metadata)
    physical <- myClim::mc_data_physical[[sensor_info@physical]]
    right_margin <- 8
    par(mar=c(5, 5, 0.25, right_margin))
    par(new=F)
    plot(logger$datetime, logger$sensors[[sensor]]$values, type="n", xaxt="n", yaxt="n", xlab=NA, ylab=NA, xlim=xlimit)
    abline(v=months, lty=1, col=gray(0.9))
    grid(nx=NA, ny=NULL, col=gray(0.9), lty=1)
    box()
    axis(4)
    lines(logger$datetime, logger$sensors[[sensor]]$values, col=sensor_info@plot_color,
          lwd=sensor_info@plot_line_width)
    mtext(physical@description, 4, line=3, las=3)
}

.plot_get_values_range <- function(logger, sensors){
    result <- min(sapply(sensors, function(x) min(logger$sensors[[x]]$values, na.rm = T)), na.rm = T)
    result[[2]] <- max(sapply(sensors, function(x) max(logger$sensors[[x]]$values, na.rm = T)), na.rm = T)
    result
}

.plot_get_months_from_xlimit <- function(xlimit){
    start_month <- .plot_get_start_month_by_datetime(xlimit[[1]])
    end_month <- .plot_get_start_month_by_datetime(xlimit[[2]])
    seq(start_month, end_month, "months")
}

.plot_get_start_month_by_datetime <- function(datetime){
    as.POSIXct(paste0(format(datetime, "%Y-%m"), "-01 00:00", tz="UTC"))
}

#' Plot data - image
#'
#' Function plot data to file with image function
#'
#' @param data in format for preparing or calculation
#' @param filename output filename
#' @param title of plot; default is empty
#' @param localities names of localities; if empty then all
#' @param sensors names of sensors; if empty then all
#' @param height of image; default = 1900
#' @param left_margin width of space for sensor_labels; default = 12
#' @export
#' @examples
#' mc_plot_image(data, "T1_image.png", "T1 sensor", sensors="TMS_T1")
mc_plot_image <- function(data, filename, title="", localities=NULL, sensors=NULL, height=1900, left_margin=12) {
    data_table <- mc_reshape_wide(data, localities, sensors)
    values_matrix <- as.matrix(data_table[,-1])
    png(filename=filename,width=1900, height=height, res=200)
    x_labels <- substr(data_table$datetime[seq(1, nrow(data_table), len=20)], 1, 10)
    bottom_margin <- 7
    top_margin <- 3
    right_margin <- 8
    par(mar=c(bottom_margin, left_margin, top_margin, right_margin))
    image(values_matrix, xaxt ="n", yaxt="n", col = hcl.colors(12, "viridis", rev = FALSE))
    axis(side = 1, at=seq(0, 1, len=20), labels=x_labels, las=3)
    cex.axis_function <- function() {
        ref_value <- 50
        current_value <- height / (ncol(data_table) - 1)
        if(current_value >= ref_value) return(1)
        current_value / ref_value
    }
    axis(side = 2, at=seq(0, 1, len=ncol(values_matrix)), labels=colnames(data_table)[-1], las=2, cex.axis=cex.axis_function())
    legend_values <- round(seq(max(values_matrix, na.rm=T), min(values_matrix, na.rm=T), len=12), 2)
    legend(grconvertX(1630, "device"), grconvertY(120, "device"),
           legend_values, fill = hcl.colors(12, "viridis", rev = TRUE), xpd = NA)
    title(main=title, line=0.5, cex.lab=1.2)
    dev.off()
}

#' Plot data - ggplot2 geom_raster
#'
#' Function plot data to file with ggplot2 geom_raster
#'
#' @param data in format for preparing or calculation
#' @param filename output - supported formats are pdf and png
#' @param sensors names of sensor; should have same unit
#' @param by_hour if TRUE, then y axis is hour, alse time (default TRUE)
#' @param png_width width for png output (default 1900)
#' @param png_height height for png output (default 1900)
#' @param viridis_color_map viridis color map option; if NULL, then used value from mc_data_physical
#'
#' * "A" - magma
#' * "B" - inferno
#' * "C" - plasma
#' * "D" - viridis
#' * "E" - cividis
#' * "F" - rocket
#' * "G" - mako
#' * "H" - turbo
#' @param start_crop POSIXct datetime for crop data (default NULL)
#' @param end_crop POSIXct datetime for crop data (default NULL)
#' @export
mc_plot_raster <- function(data, filename, sensors=NULL, by_hour=TRUE, png_width=1900, png_height=1900,
                           viridis_color_map=NULL, start_crop=NULL, end_crop=NULL) {
    data <- mc_filter(data, sensors=sensors)
    if(!is.null(start_crop) || !is.null(end_crop)) {
        data <- mc_prep_crop(data, start_crop, end_crop)
    }
    data_table <-mc_reshape_long(data, )
    data_table <- dplyr::mutate(data_table, date = lubridate::date(datetime))
    if(by_hour) {
        data_table <- dplyr::mutate(data_table, y_values = lubridate::hour(datetime))
        y_name <- "hour"
    } else {
        data_table <- dplyr::mutate(data_table, y_values = format(datetime, format = "%H:%M:%S"))
        y_name <- "time"
    }
    plot <- ggplot2::ggplot(data_table, ggplot2::aes(date, y_values, na.rm = FALSE))
    plot <- plot + ggplot2::ylab(y_name)
    plot <- plot + ggplot2::geom_raster(ggplot2::aes(fill=value))
    plot <- .plot_set_ggplot_physical_colors(data, plot, viridis_color_map)
    plot <- plot + .plot_set_ggplot_raster_theme()
    plot <- plot + ggplot2::scale_x_date(date_labels="%Y-%m")
    file_type <- .plot_get_file_type(filename)
    if(file_type == "pdf"){
        .plot_print_pdf(filename, plot, locality_id ~ sensor_name, 40)
    } else if(file_type == "png") {
        .plot_print_png(filename, plot, png_width, png_height, locality_id ~ sensor_name)
    } else {
        stop(stringr::str_glue("Format of {filename} isn't supported."))
    }
}

.plot_set_ggplot_physical_colors <- function(data, plot, viridis_color_map) {
    locality <- dplyr::first(myClim:::.common_get_localities(data))
    if(.common_is_calc_format(data)) {
        item <- locality
    } else {
        item <- dplyr::first(locality$loggers)
    }
    sensor_metadata <- dplyr::first(item$sensors)$metadata
    if(is.na(sensor_metadata@sensor_id) || !(sensor_metadata@sensor_id %in% names(mc_data_sensors)) ||
        is.na(mc_data_sensors[[sensor_metadata@sensor_id]]@physical)) {
        if(is.null(viridis_color_map)) {
            viridis_color_map <- "D"
        }
        return(plot + viridis::scale_fill_viridis(name=sensor_metadata@name, option=viridis_color_map, direction=1))
    }

    sensor <- mc_data_sensors[[sensor_metadata@sensor_id]]
    physical <- mc_data_physical[[sensor@physical]]
    if(is.null(viridis_color_map)) {
        viridis_color_map <- physical@viridis_color_map
    }
    plot + viridis::scale_fill_viridis(name=physical@description, option=viridis_color_map, direction=1)
}

.plot_set_ggplot_raster_theme <- function() {
    ggplot2::theme(strip.text.y = ggplot2::element_text(angle = 0),
                   axis.ticks.y=ggplot2::element_blank(),
                   axis.text.y = ggplot2::element_blank(),
                   legend.position="bottom",
                   legend.key.width= ggplot2::unit(2, 'cm'),
                   legend.key.height= ggplot2::unit(0.4, 'cm'),
                   panel.border = ggplot2::element_blank(),
                   panel.grid.major = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank())
}

.plot_get_file_type <- function(filename) {
    match <- stringr::str_match(filename, ".+\\.([^.]+)$")
    match[[2]]
}

.plot_print_pdf <- function(filename, plot, facets, nrow) {
    plot <- plot + ggforce::facet_grid_paginate(facets, ncol = 1, nrow = nrow, page = 1, drop = FALSE, byrow = FALSE)
    n_pages <- ggforce::n_pages(plot)
    pdf(filename, family="ArialMT", paper="a4", w=210/25.4, h=297/25.4)
    purrr::walk(seq(1:n_pages), function (x) print(plot + ggforce::facet_grid_paginate(facets, ncol = 1, nrow = nrow, page = x, drop = TRUE, byrow = FALSE)))
    dev.off()
}

.plot_print_png <- function(filename, plot, width, height, facets) {
    plot <- plot + ggforce::facet_grid_paginate(facets, ncol = 1, byrow = FALSE)
    png(filename, width=width, height=height, res=200)
    print(plot)
    dev.off()
}

#' Plot data - ggplot2 geom_line
#'
#' Function plot data to file with ggplot2 geom_line
#'
#' Maximal number of physical units of sensors is two. Main and secondary y axis.
#'
#' @param data in format for preparing or calculation
#' @param filename output - supported formats are pdf and png
#' @param sensors names of sensor
#' @param scale_coeff scale coefficient for secondary axis (default NULL)
#'
#' Values from secondary axis are scaled with calculation values * scale_coeff. If coefficient is NULL
#' than function try detects scale coefficient from physical unit of sensors.
#' @param png_width width for png output (default 1900)
#' @param png_height height for png output (default 1900)
#' @param start_crop POSIXct datetime for crop data (default NULL)
#' @param end_crop POSIXct datetime for crop data (default NULL)
#' @export
mc_plot_line <- function(data, filename, sensors=NULL,
                         scale_coeff=NULL,
                         png_width=1900, png_height=1900,
                         start_crop=NULL, end_crop=NULL) {
    data <- mc_filter(data, sensors=sensors)
    if(!is.null(start_crop) || !is.null(end_crop)) {
        data <- mc_prep_crop(data, start_crop, end_crop)
    }
    sensors_table <- .plot_get_sensors_table(data)
    sensors_table <- .plot_add_coeff_to_sensors_table(sensors_table, scale_coeff)
    data_table <- mc_reshape_long(data)
    plot <- ggplot2::ggplot()

    line_function <- function(sensor, color, coeff) {
        sensor_name <- sensor
        data_plot <- dplyr::filter(data_table, sensor == sensor_name)
        ggplot2::geom_line(data_plot, mapping = ggplot2::aes(x=datetime, y=value*coeff, group=sensor, color=sensor))
    }

    plots <- purrr::pmap(dplyr::select(sensors_table, sensor, color, coeff), line_function)
    plot <- purrr::reduce(plots, `+`, .init=plot)
    plot <- plot + ggplot2::scale_color_manual(values=sensors_table$color)
    plot <- plot + .plot_set_ggplot_line_theme()
    plot <- plot + .plot_line_set_y_axes(sensors_table)

    file_type <- .plot_get_file_type(filename)
    if(file_type == "pdf"){
        .plot_print_pdf(filename, plot, ggplot2::vars(locality_id), 8)
    } else if(file_type == "png") {
        .plot_print_png(filename, plot, png_width, png_height, ggplot2::vars(locality_id))
    } else {
        stop(stringr::str_glue("Format of {filename} isn't supported."))
    }
}

.plot_get_sensors_table <- function(data) {
    is_prep_format <- myClim:::.common_is_prep_format(data)

    sensors_item_function <- function(item) {
        physical_function <- function(sensor) {
            sensor_info <- mc_data_sensors[[sensor$metadata@sensor_id]]
            if(!is.na(sensor_info@physical)) {
                return(sensor_info@physical)
            }
            if(sensor_info@value_type == "logical") {
                return(sensor_info@value_type)
            }
            sensor$metadata@sensor_id
        }

        color_function <- function(sensor) {
            sensor_info <- mc_data_sensors[[sensor$metadata@sensor_id]]
            if(is.na(sensor_info@plot_color)) {
                return("black")
            }
            sensor_info@plot_color
        }

        sensor_names <- names(item$sensors)
        physicals <- purrr::map_chr(item$sensors, physical_function)
        colors <- purrr::map_chr(item$sensors, color_function)
        tibble::tibble(sensor=sensor_names, physical=physicals, color=colors)
    }

    prep_locality_function <- function(locality) {
        purrr::map_dfr(locality$loggers, sensors_item_function)
    }

    if(is_prep_format) {
        table <- purrr::map_dfr(data, prep_locality_function)
    } else {
        table <- purrr::map_dfr(data$localities, sensors_item_function)
    }
    table <- dplyr::distinct(table)
    physicals <- unique(table$physical)
    if(length(physicals) > 2) {
        stop("There are more then two physical units.")
    }
    main_physical <- physicals[[1]]
    if(myClim:::.model_const_PHYSICAL_T_C %in% physicals) {
        main_physical <- myClim:::.model_const_PHYSICAL_T_C
    }
    table$main_axis <- (table$physical == main_physical)
    table
}

.plot_add_coeff_to_sensors_table <- function(sensors_table, scale_coeff) {
    physical_table <- dplyr::distinct(dplyr::select(sensors_table, physical, main_axis))

    get_scale_coeff <- function(selector) {
        physical <- physical_table$physical[selector]
        if(physical %in% names(mc_data_physical)) {
            return(mc_data_physical[[physical]]@scale_coeff)
        }
        1
    }

    if(is.null(scale_coeff) && nrow(physical_table) > 1) {
        main_scale_coeff <- get_scale_coeff(physical_table$main_axis)
        secondary_scale_coeff <- get_scale_coeff(!physical_table$main_axis)
        scale_coeff <- 1 / main_scale_coeff * secondary_scale_coeff
    }

    sensors_table$coeff <- purrr::map_dbl(sensors_table$main_axis, ~ if(.x) 1 else scale_coeff)
    sensors_table
}

.plot_line_set_y_axes <- function(sensors_table) {
    physical_table <- dplyr::distinct(dplyr::select(sensors_table, physical, main_axis, coeff))
    sec.axis <- ggplot2::waiver()
    if(nrow(physical_table) == 2) {
        physical <- physical_table$physical[!physical_table$main_axis]
        coeff <- physical_table$coeff[!physical_table$main_axis]
        description <- physical
        if(physical %in% names(mc_data_physical)) {
            description <- mc_data_physical[[physical]]@description
        }
        sec.axis <- ggplot2::sec_axis(~./coeff, name=description)
    }
    main_physical <- physical_table$physical[physical_table$main_axis]
    main_description <- main_physical
    if(main_physical %in% names(mc_data_physical)) {
        main_description <- mc_data_physical[[main_physical]]@description
    }
    ggplot2::scale_y_continuous(name=main_description, sec.axis=sec.axis)
}

.plot_set_ggplot_line_theme <- function() {
    ggplot2::theme(strip.text.y = ggplot2::element_text(angle = 0),
                   legend.position="bottom",
                   legend.key.width= ggplot2::unit(2, 'cm'),
                   legend.key.height= ggplot2::unit(0.4, 'cm'),
                   panel.border = ggplot2::element_blank())
}

