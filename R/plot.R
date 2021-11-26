.plot_const_MOISTURE_PHYSICAL <- c("TMSmoisture", "moisture")

#' Plot data from loggers
#'
#' Function plot loggers to directory
#'
#' @param data all data in standard format
#' @param directory output directory
#' @param localities names of localities; if empty then all
#' @param sensors names of sensors; if empty then all
#' @param crop datetime range for plot, not cropping if NA (default c(NA, NA))
#' @export
#' @examples
#' mc_plot_loggers(example_tomst_data1, "Figures")
mc_plot_loggers <- function(data, directory, localities=c(), sensors=c(), crop=c(NA, NA)) {
    data <- mc_filter(data, localities, sensors)
    loggers <- microclim:::.common_get_loggers(data)
    dir.create(directory, showWarnings = F)
    for(logger in loggers) {
        filename <- file.path(directory, paste0(logger$metadata@serial_number, ".png"))
        .plot_logger(logger, filename, crop)
    }
}

.plot_logger <- function(logger, filename, crop=c(NA, NA)) {
    microclim:::.clean_warn_if_datetime_step_unprocessed(logger)
    png(filename=filename, width=1920, height=1000, res=200)
    physical <- .plot_get_logger_sensors_by_physical(logger)
    moisture_physical <- intersect(.plot_const_MOISTURE_PHYSICAL, names(physical))
    .plot_logger_set_parameters(physical, moisture_physical)
    xlimit <- .plot_get_xlimit(logger$datetime, crop)
    months <- .plot_get_months_from_xlimit(xlimit)
    .plot_logger_temperature(logger, xlimit, months, physical[["T"]])
    if(length(moisture_physical) > 0){
        .plot_logger_moisture(logger, xlimit, months, physical[[moisture_physical[[1]]]])
    }
    axis.POSIXct(1, at=months, labels=T, format="%m/%Y", las=2)
    dev.off()
}

.plot_get_logger_sensors_by_physical <- function(logger) {
    physical <- sapply(logger$sensors, function(x) {
        microclim:::.common_get_sensor_info(x$metadata)@physical})
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
    microclim:::.common_as_utc_posixct(result)
}

.plot_logger_temperature <- function(logger, xlimit, months, sensors)
{
    if(is.null(sensors)) {
        ylimit <- c(-15, 30)
    }
    else {
        sensor_info <- sapply(sensors, function(x) {
            microclim:::.common_get_sensor_info(logger$sensors[[x]]$metadata)})
        values_range <- .plot_get_values_range(logger, sensors)
        ylimit = c(min(c(-15, values_range[[1]]), na.rm=T), max(c(30, values_range[[2]]), na.rm=T))
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
    sensor_info <- microclim:::.common_get_sensor_info(logger$sensors[[sensor]]$metadata)
    physical <- microclim::mc_data_physical[[sensor_info@physical]]
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
#' @param data all data in standard format
#' @param filename output filename
#' @param title of plot
#' @param localities names of localities; if empty then all
#' @param sensors names of sensors; if empty then all
#' @param height of image default = 1900
#' @export
#' @examples
#' mc_plot_image(data, "T1_image.png", "T1 sensor", sensors="TMS_T1")
mc_plot_image <- function(data, filename, title, localities=NULL, sensors=NULL, height=1900) {
    data_table <- microclim::mc_reshape_wideformat(data, localities, sensors)
    values_matrix <- as.matrix(data_table[,-1])
    png(filename=filename,width=1900, height=height, res=200)
    x_labels <- substr(data_table$datetime[seq(1, nrow(data_table), len=20)], 1, 10)
    bottom_margin <- 7
    left_margin <- 12
    top_margin <- 3
    right_margin <- 8
    par(mar=c(bottom_margin, left_margin, top_margin, right_margin))
    image(values_matrix, xaxt ="n", yaxt="n", col = hcl.colors(12, "viridis", rev = FALSE))
    axis(side = 1, at=seq(0, 1, len=20), labels=x_labels, las=3)
    axis(side = 2, at=seq(0, 1, len=ncol(values_matrix)), labels=colnames(data_table)[-1], las=2)
    legend_values <- round(seq(max(values_matrix, na.rm=T), min(values_matrix, na.rm=T), len=12), 2)
    legend(grconvertX(1630, "device"), grconvertY(120, "device"),
           legend_values, fill = hcl.colors(12, "viridis", rev = TRUE), xpd = NA)
    title(main=title, line=0.5, cex.lab=1.2)
    dev.off()
}
