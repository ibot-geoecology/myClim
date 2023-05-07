.plot_const_MOISTURE_PHYSICAL <- c(.model_const_PHYSICAL_TMSmoisture,
                                   .model_const_PHYSICAL_moisture)
.plot_const_MESSAGE_DUPLICATED_SENSOR <- "Sensor {duplicated_sensors} contains multiple physicals. It is not allowed."

#' Plot data from loggers
#'
#' Function save separate files (*.png) per the loggers to the directory.
#' Only Raw-format supported, Agg-format not supported.
#' For Agg-format use [myClim::mc_plot_line()]. Function was primary designed
#' for Tomst  TMS loggers for fast, and easy data visualization.
#'
#' @template param_myClim_object_raw
#' @param directory path to output directory
#' @template param_localities_sensors
#' @param crop datetime range for plot, not cropping if NA (default c(NA, NA))
#' @export
#' @return PNG files created in the output directory
#' @examples
#' tmp_dir <- file.path(tempdir(), "plot")
#' mc_plot_loggers(mc_data_example_clean, tmp_dir)
#' unlink(tmp_dir, recursive=TRUE)
mc_plot_loggers <- function(data, directory, localities=NULL, sensors=NULL, crop=c(NA, NA)) {
    oldpar <- par(no.readonly = TRUE)
    on.exit(par(oldpar))
    .common_stop_if_not_raw_format(data)
    data <- mc_filter(data, localities, sensors)
    .prep_check_datetime_step_unprocessed(data)
    loggers <- .common_get_loggers(data)
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
    .plot_logger_temperature(logger, xlimit, months, physical[[.model_const_PHYSICAL_T_C]])
    if(length(moisture_physical) > 0){
        .plot_logger_moisture(logger, xlimit, months, physical[[moisture_physical[[1]]]])
    }
    axis.POSIXct(1, at=months, labels=TRUE, format="%m/%Y", las=2)
    dev.off()
}

.plot_get_logger_sensors_by_physical <- function(logger) {
    physical <- purrr::map_chr(logger$sensors, function(x) {
        .common_get_sensor_info(x$metadata)@physical})
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
    .common_as_utc_posixct(result)
}

.plot_logger_temperature <- function(logger, xlimit, months, sensors)
{
    if(is.null(sensors)) {
        ylimit <- c(-15, 30)
    }
    else {
        sensor_info <- sapply(sensors, function(x) {
            .common_get_sensor_info(logger$sensors[[x]]$metadata)})
        values_range <- .plot_get_values_range(logger, sensors)
        ylimit <- c(min(c(-15, values_range[[1]]), na.rm=T), max(c(30, values_range[[2]]), na.rm=T))
    }
    plot(logger$datetime, rep(NA, length(logger$datetime)), type="n", xaxt="n", xlab=NA, ylab="Temperature (\u00b0C)",
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
    sensor_info <- .common_get_sensor_info(logger$sensors[[sensor]]$metadata)
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
#' Function plots single sensor form myClim data into PNG file with image() R base function.
#' This was designed for fast, and easy data visualization especially focusing on missing
#' values visualization and general data picture.
#'
#' @details Be careful with bigger data. Can take some time. 
#' @template param_myClim_object
#' @param filename output file name (file path)
#' @param title of plot; default is empty
#' @template param_localities_sensors
#' @param height of image; default = 1900
#' @param left_margin width of space for sensor_labels; default = 12
#' @return PNG file created as specified in output file name
#' @export
#' @examples
#' tmp_dir <- tempdir()
#' tmp_file <- tempfile(tmpdir = tmp_dir)
#' mc_plot_image(mc_data_example_clean, tmp_file, "T1 sensor", sensors="TMS_T1")}
#' file.remove(tmp_file)
mc_plot_image <- function(data, filename, title="", localities=NULL, sensors=NULL, height=1900, left_margin=12) {
    oldpar <- par(no.readonly = TRUE)
    on.exit(par(oldpar))
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
#' Function plots data with ggplot2 geom_raster. Plot is returned as ggplot faced raster and
#' is primary designed to be saved as .pdf file (recommended) or .png file.
#' Plotting into R environment without saving any file is also possible.
#' See details.
#'
#' @details Saving as the .pdf file is recommended, because the plot is optimized
#' to be paginate PDF (facet raster plot is distributed to pages), which is especially useful
#' for bigger data. In case of plotting multiple sensors to PDF, the facet grids are grouped by sensor.
#' I.e., all localities of sensor_1 followed by all localities of sensor_2 etc.
#' When plotting only few localities, but multiple sensors,
#' each sensor has own page. I.e., when plotting data from one locality, and 3 sensors resulting PDF has 3 pages.
#' In case of plotting PNG, sensors are plotted in separated images (PNG files) by physical.
#' I.e, when plotting 3 sensors in PNG it will save 3 PNG files named after sensors.
#' Be careful with bigger data in PNG. Play with `png_height` and `png_width`.
#' When too small height/width, image does not fit and is plotted incorrectly. Plotting into
#' R environment instead of saving PDF or PNG is possible, but is recommended only for
#' low number  of localities (e.g. up to 10), because
#' high number of localities plotted in R environment results in very small picture which is hard/impossible to read.
#'
#' 
#' @template param_myClim_object
#' @param filename output with the extension - supported formats are .pdf and .png (default NULL)
#' If NULL then the plot is shown/returned into R environment as ggplot object, but not saved to file.
#' @param sensors names of sensor; should have same physical unit see `names(mc_data_sensors)`
#' @param by_hour if TRUE, then y axis is plotted as an hour, else original time step (default TRUE) 
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
#' @return list of ggplot2 objects
#' @examples
#' tmp_dir <- tempdir()
#' tmp_file <- tempfile(tmpdir = tmp_dir, fileext=".pdf")
#' mc_plot_raster(mc_data_example_agg, filename=tmp_file, sensors=c("TMS_T3","TM_T"))
#' file.remove(tmp_file)
#' @export
mc_plot_raster <- function(data, filename=NULL, sensors=NULL, by_hour=TRUE, png_width=1900, png_height=1900,
                           viridis_color_map=NULL, start_crop=NULL, end_crop=NULL) {
    data <- mc_filter(data, sensors=sensors)
    if(!is.null(start_crop) || !is.null(end_crop)) {
        data <- mc_prep_crop(data, start_crop, end_crop)
    }
    sensors_table <- .plot_get_data_sensors_by_physical(data)
    sensors_table <- dplyr::group_by(sensors_table, .data$physical)

    group_function <- function(group, .y) {
        filtered_data <- mc_filter(data, sensors=group$sensor)
        .plot_raster_physical(filtered_data, by_hour, viridis_color_map)
    }

    plots <- dplyr::group_map(sensors_table, group_function)

    if(!is.null(filename)) {
        file_parts <- .plot_get_file_parts(filename)
        if(file_parts[[2]] == "pdf"){
            .plot_print_pdf(filename, plots, locality_id ~ sensor_name, 40)
        } else if(file_parts[[2]] == "png") {
            .plot_print_raster_pngs(file_parts[[1]], plots, dplyr::group_keys(sensors_table)$physical, png_width, png_height)
        } else {
            stop(stringr::str_glue("Format of {filename} isn't supported."))
        }
    }
    plots <- purrr::map(plots, ~ .x + ggplot2::facet_grid(locality_id ~ sensor_name))
    return(plots)
}

.plot_get_data_sensors_by_physical <- function(data) {
    item_function <- function (item) {
        physical <- purrr::map_chr(item$sensors, function(x) {
            .common_get_sensor_info(x$metadata)@physical})
        tibble::tibble(sensor=names(physical),
                       physical=unname(physical))
    }

    raw_locality_function <- function(locality) {
        purrr::map_dfr(locality$loggers, item_function)
    }

    if(.common_is_agg_format(data)) {
        result <- purrr::map_dfr(data$localities, item_function)
    } else {
        result <- purrr::map_dfr(data$localities, raw_locality_function)
    }
    result <- dplyr::distinct(result)
    if(any(duplicated(result$sensor))) {
        duplicated_sensors <- result$sensor[duplicated(result$sensor)]
        stop(stringr::str_glue(.plot_const_MESSAGE_DUPLICATED_SENSOR))
    }
    return(result)
}

.plot_raster_physical <- function(data, by_hour, viridis_color_map) {
    data_table <-mc_reshape_long(data)
    data_table <- dplyr::mutate(data_table, date = lubridate::date(.data$datetime))
    if(by_hour) {
        data_table <- dplyr::mutate(data_table, y_values = lubridate::hour(.data$datetime))
        y_name <- "hour"
    } else {
        data_table <- dplyr::mutate(data_table, y_values = format(.data$datetime, format = "%H:%M:%S"))
        y_name <- "time"
    }
    plot <- ggplot2::ggplot(data_table, ggplot2::aes(.data$date, .data$y_values, na.rm = FALSE))
    plot <- plot + ggplot2::ylab(y_name)
    plot <- plot + ggplot2::geom_raster(ggplot2::aes(fill=.data$value))
    plot <- .plot_set_ggplot_physical_colors(data, plot, viridis_color_map)
    plot <- plot + .plot_set_ggplot_raster_theme()
    plot <- plot + ggplot2::scale_x_date(date_labels="%Y-%m")
}

.plot_set_ggplot_physical_colors <- function(data, plot, viridis_color_map) {
    locality <- dplyr::first(data$localities)
    if(.common_is_agg_format(data)) {
        item <- locality
    } else {
        item <- dplyr::first(locality$loggers)
    }
    sensor_metadata <- dplyr::first(item$sensors)$metadata
    if(is.na(sensor_metadata@sensor_id) || !(sensor_metadata@sensor_id %in% names(myClim::mc_data_sensors)) ||
        is.na(myClim::mc_data_sensors[[sensor_metadata@sensor_id]]@physical)) {
        if(is.null(viridis_color_map)) {
            viridis_color_map <- "D"
        }
        return(plot + viridis::scale_fill_viridis(name=sensor_metadata@name, option=viridis_color_map, direction=1))
    }

    sensor <- myClim::mc_data_sensors[[sensor_metadata@sensor_id]]
    physical <- myClim::mc_data_physical[[sensor@physical]]
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

.plot_get_file_parts <- function(filename) {
    match <- stringr::str_match(filename, "(.+)\\.([^.]+)$")
    match[2:3]
}

.plot_print_pdf <- function(filename, plots, facets, nrow) {
    facet_function <- function(page, drop) {
        ggforce::facet_grid_paginate(facets, ncol = 1, nrow = nrow, page = page, drop = drop, byrow = FALSE)
    }
    plots <- purrr::map(plots, ~ .x + facet_function(1, FALSE))

    print_plot <- function(plot) {
        n_pages <- sum(ggforce::n_pages(plot))
        purrr::walk(seq(1:n_pages), function (x) print(plot + facet_function(x, TRUE)))
    }

    pdf(filename, family="ArialMT", paper="a4", width=210/25.4, height=297/25.4)
    purrr::walk(plots, print_plot)
    dev.off()
}

.plot_print_raster_pngs <- function(filename_prefix, plots, physicals, width, height) {
    print_function <- function(plot, physical) {
        filename <- stringr::str_glue("{filename_prefix}_{physical}.png")
        .plot_print_png(filename, plot, width, height, locality_id ~ sensor_name)
    }
    purrr::walk2(plots, physicals, print_function)
}

.plot_print_png <- function(filename, plot, width, height, facets) {
    plot <- plot + ggforce::facet_grid_paginate(facets, ncol = 1, byrow = FALSE)
    png(filename, width=width, height=height, res=200)
    print(plot)
    dev.off()
}

#' Plot data - ggplot2 geom_line
#'
#' Function plots data with ggplot2 geom_line. Plot is returned as ggplot faced grid and
#' is primary designed to be saved as PDF file. PNG is also possible, the same as
#' plotting ggplot object into R environment. See details.
#'
#' @details
#' Saving as the PDF file is recommended, because the plot is optimized
#' to be paginate PDF (facet raster plot is distributed to pages), which is especially useful
#' for bigger data. Maximal number of physical units (elements) of sensors to be plotted in one
#' plot is two with main and secondary y axis. In case, there are multiple sensors with
#' identical physical on one locality, they are plotted together. E.g., when you have
#' TMS_T1, TMS_T2, TMS_T3, TS_T, and moisture you get plot with 5 lines of different colors and
#' two y axes. Secondary y axes are scaled with calculation `values * scale_coeff`.
#' If coefficient is NULL than function try detects scale coefficient from
#' physical unit of sensors see [mc_Physical-class]. Scaling is useful when
#' plotting together e.g. temperature and moisture. For TMS and HOBO scaling coefficients
#' are estimated automatically, correctly. For other data it is better to set it by hand.
#'
#' @template param_myClim_object
#' @param filename output file name/path with the extension - supported formats are .pdf and .png (default NULL)
#'
#' If NULL then the plot is displayed and can be returned into r environment but is not saved to file.
#' @param sensors select the names of sensors to be plotted (max 2) see `names(mc_data_sensors)`
#' @param scale_coeff scale coefficient for secondary axis (default NULL)
#' @param png_width width for png output (default 1900)
#' @param png_height height for png output (default 1900)
#' @param start_crop POSIXct datetime for crop data (default NULL)
#' @param end_crop POSIXct datetime for crop data (default NULL)
#' @return ggplot2 object
#' @examples
#' tms.plot <- mc_filter(mc_data_example_agg, localities = "A6W79")
#' p <- mc_plot_line(tms.plot,sensors = c("TMS_T3","TMS_T1","TMS_TMSmoisture"))
#' p <- p+ggplot2::scale_x_datetime(date_breaks = "1 week", date_labels = "%W")
#' p <- p+ggplot2::xlab("week")
#' p <- p+ggplot2::scale_color_manual(values=c("hotpink","pink", "darkblue"),name=NULL)
#' @export
mc_plot_line <- function(data, filename=NULL, sensors=NULL,
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
        data_plot <- dplyr::filter(data_table, sensor == .data$sensor_name)
        ggplot2::geom_line(data_plot, mapping = ggplot2::aes(x=.data$datetime, y=.data$value*coeff, group=sensor, color=sensor))
    }

    plots <- purrr::pmap(dplyr::select(sensors_table, "sensor", "color", "coeff"), line_function)
    plot <- purrr::reduce(plots, `+`, .init=plot)
    plot <- plot + ggplot2::scale_color_manual(values=sensors_table$color)
    plot <- plot + .plot_set_ggplot_line_theme()
    plot <- plot + .plot_line_set_y_axes(sensors_table)

    if(!is.null(filename)) {
        file_parts <- .plot_get_file_parts(filename)
        if(file_parts[[2]] == "pdf"){
            .plot_print_pdf(filename, list(plot), ggplot2::vars(.data$locality_id), 8)
        } else if(file_parts[[2]] == "png") {
            .plot_print_png(filename, plot, png_width, png_height, ggplot2::vars(.data$locality_id))
        } else {
            stop(stringr::str_glue("Format of {filename} isn't supported."))
        }
    }
    plot <- plot + ggplot2::facet_grid(rows = ggplot2::vars(.data$locality_id))
    return(plot)
}

.plot_get_sensors_table <- function(data) {
    is_raw_format <- .common_is_raw_format(data)

    sensors_item_function <- function(item) {
        physical_function <- function(sensor) {
            sensor_info <- myClim::mc_data_sensors[[sensor$metadata@sensor_id]]
            if(!is.na(sensor_info@physical)) {
                return(sensor_info@physical)
            }
            if(sensor_info@value_type == "logical") {
                return(sensor_info@value_type)
            }
            sensor$metadata@sensor_id
        }

        color_function <- function(sensor) {
            sensor_info <- myClim::mc_data_sensors[[sensor$metadata@sensor_id]]
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

    raw_locality_function <- function(locality) {
        purrr::map_dfr(locality$loggers, sensors_item_function)
    }

    if(is_raw_format) {
        table <- purrr::map_dfr(data$localities, raw_locality_function)
    } else {
        table <- purrr::map_dfr(data$localities, sensors_item_function)
    }
    table <- dplyr::distinct(table)
    physicals <- unique(table$physical)
    if(length(physicals) > 2) {
        stop("There are more then two physical units.")
    }
    main_physical <- physicals[[1]]
    if(.model_const_PHYSICAL_T_C %in% physicals) {
        main_physical <- .model_const_PHYSICAL_T_C
    }
    table$main_axis <- (table$physical == main_physical)
    table
}

.plot_add_coeff_to_sensors_table <- function(sensors_table, scale_coeff) {
    physical_table <- dplyr::distinct(dplyr::select(sensors_table, "physical", "main_axis"))

    get_scale_coeff <- function(selector) {
        physical <- physical_table$physical[selector]
        if(physical %in% names(myClim::mc_data_physical)) {
            return(myClim::mc_data_physical[[physical]]@scale_coeff)
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
    physical_table <- dplyr::distinct(dplyr::select(sensors_table, "physical", "main_axis", "coeff"))
    sec.axis <- ggplot2::waiver()
    if(nrow(physical_table) == 2) {
        physical <- physical_table$physical[!physical_table$main_axis]
        coeff <- physical_table$coeff[!physical_table$main_axis]
        description <- physical
        if(physical %in% names(myClim::mc_data_physical)) {
            description <- myClim::mc_data_physical[[physical]]@description
        }
        sec.axis <- ggplot2::sec_axis(~./coeff, name=description)
    }
    main_physical <- physical_table$physical[physical_table$main_axis]
    main_description <- main_physical
    if(main_physical %in% names(myClim::mc_data_physical)) {
        main_description <- myClim::mc_data_physical[[main_physical]]@description
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

.plot_show_joining_chart <- function(data_table, title, y_label, sizes, highlight_data_table) {
    p <- ggplot2::ggplot(data=data_table, ggplot2::aes(x=.data$datetime, y=.data$value, group=.data$name)) +
        ggplot2::geom_line(ggplot2::aes(color=.data$name, size=.data$size)) +
        ggplot2::scale_size_manual(values = sizes) +
        ggplot2::theme(legend.position="bottom") +
        ggplot2::ggtitle(title) +
        ggplot2::ylab(y_label) +
        ggplot2::xlab("Date") +
        ggplot2::geom_rect(data=highlight_data_table, inherit.aes = FALSE,
                           ggplot2::aes(xmin=.data$start, xmax=.data$end,
                                        ymin=.data$ymin, ymax=.data$ymax, group=.data$group),
                           color="transparent", fill="orange", alpha=0.3) +
        ggplot2::facet_grid(rows = ggplot2::vars(.data$sensor))
    p <- plotly::ggplotly(p)
    print(p)
}
