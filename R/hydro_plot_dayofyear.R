#' hydro_plot_dayofyear
#' Function to plot hydrometric data
#' Plots daily values of hydrometric data relative to historic values for each day of year
#' @param station_number Station number
#' @param parameter The input parameter
#' @param select_years Range of years to plot in yyyy:yyyy format
#' @param after_bennett If T sets historic min to 1972
#' @param historic_min Set historic min year
#' @param historic_max Set historic max
#' @param historic If true, plots min/max and average range of historic values
#' @param log_scale T or F to plot on log scale
#' @param start_month Month number used to specify a start date
#' @param start_day Day number used to specify a start date
#' @param end_month Month number used to specify an end date
#' @param end_day Day number used to specify an end date
#' @param line_colours Option to select colours for sites plotted: c("blue","red","green"...)
#' @param legend_position The position of the legend on the plot
#' @param line_size Linewidth
#' @param point_size size of geom_points
#' @param legend_text_size size of legend text. Default is 8
#' @param y_min The lower limit of plot y-axis
#' @param y_max The upper limit of plot y-axis
#' @param save Option to save plot
#' @param plot_width Width of the plot in cm
#' @param plot_height Height of the plot in cm
#' @param dpi dots per inch resolution of plot
#' @param file_name Name of plot file if save = TRUE
#' @param extension Plot file type extension: ex. "png"

#' @return A ggplot object of the selected station
#' @export



hydro_plot_dayofyear <- function(
  station_number,
  parameter,
  select_years,
  after_bennett = FALSE,
  historic_min = NA,
  historic_max = 2023,
  water_year_start = 1,
  historic = TRUE,
  log_scale = FALSE,
  start_month = 01,
  start_day = 01,
  end_month = 12,
  end_day = 31,
  line_colours = c("blue4",
                   "orange4",
                   "green4",
                   "red4",
                   "purple4",
                   "yellow4"),
  legend_position = "top",
  line_size = 0.5,
  point_size = 0,
  legend_text_size = 8,
  y_min = NA,
  y_max = NA,
  save = FALSE,
  plot_width = 18,
  plot_height = 11,
  dpi = 900,
  file_name = "Default hydrometric plot",
  extension = "png")

{
  # Read in station metadata
  station <- tidyhydat::hy_stations(station_number)

  # Call in parameter details for plotting
  parameter <- hydro_parameter(parameter = parameter)[[1]]
  y_axis_title <- hydro_parameter(parameter = parameter)[[3]]

  # Check for line_colours
  if(length(select_years) > length(line_colours)) {
    line_colours = rainbow(length(select_years))
  } else if(length(select_years) < length(line_colours)) {
    line_colours = line_colours[1:length(select_years)]
  }

  # Import data
  daily_stats <- hydro_calc_dayofyear(
    parameter = parameter,
    station_number = station_number,
    select_years = select_years,
    after_bennett = after_bennett,
    historic_min = historic_min,
    historic_max = historic_max,
    water_year_start = water_year_start
  )

  # Format data into single year for plotting purposes
  daily_stats$DayofYear <- as.Date(daily_stats$DayofYear, origin = "1899-12-31")

  # Filter data to bounds of start and end time frames
  daily_stats <- dplyr::filter(daily_stats,
                               DayofYear >= as.Date(paste("1900", start_month, start_day, sep = "-")) &
                                 DayofYear <= as.Date(paste("1900", end_month, end_day, sep = "-")))

  # Plot the graph
  plot <- ggplot2::ggplot(daily_stats, ggplot2::aes(x = DayofYear, y = Value)) +
    ggplot2::theme_classic() +
    ggplot2::labs(title = paste0(station$STATION_NAME, " (", station$STATION_NUMBER, ")"),
                  x = "Month", y = y_axis_title) +
    ggplot2::scale_x_date(date_breaks = "1 months",
                          labels = scales::date_format("%b")) +
    ggplot2::theme(legend.position = legend_position,
                   legend.text = ggplot2::element_text(size = legend_text_size)) +
    ggplot2::scale_colour_manual(name = "",
                                 values = line_colours,
                                 na.translate = FALSE) # eliminates 'NA' from legend

  if (historic == TRUE) {
    plot <- plot +
      ggplot2::geom_ribbon(ggplot2::aes(ymin = Min, ymax = Max, fill = "Min - Max")) +
      ggplot2::geom_ribbon(ggplot2::aes(ymin = P10, ymax = P90, fill = "10th - 90th Percentile")) + #ER - added this sept 6 2024
      ggplot2::geom_ribbon(ggplot2::aes(ymin = P25, ymax = P75, fill = "Average Range")) +
      ggplot2::geom_point(ggplot2::aes(colour = factor(Year)), shape = 19, size = point_size) +
      ggplot2::geom_line(ggplot2::aes(colour = factor(Year)), linewidth = line_size) +
      ggplot2::scale_fill_manual(name = "",
                                 breaks = c("Average Range", "10th - 90th Percentile", "Min - Max"),
                                 values = c("gray75", "gray85", "gray95"))
  } else {
    plot <- plot +
      ggplot2::geom_point(ggplot2::aes(colour = factor(Year)), shape = 19, size = point_size) +
      ggplot2::geom_line(ggplot2::aes(colour = factor(Year)), linewidth = line_size)
  }

  if (log_scale == TRUE & parameter == "Flow") {
    plot <- plot +
      ggplot2::scale_y_continuous(trans = 'log10')
  }

  if ((is.na(y_min) == F) && (is.na(y_max) == F)) {
    plot <- plot +
      ggplot2::ylim(y_min, y_max)
  }

  if(station_number == "07OB002") {
    plot <- plot +
      ggplot2::scale_y_continuous(breaks = seq(156, 157.9, by = 0.1),
                              labels = c("156.0", rep("", 4),
                                         "156.5", rep("", 4),
                                         "157.0", rep("", 4),
                                         "157.5", rep("", 4)))
  }

  if((station_number == "10PA001") && (parameter == "Level")) {
    plot <- plot +
      ggplot2::ylim(5.8, 7.6)
  }

  if(save == TRUE) {
  ggplot2::ggsave(paste0(file_name, ".", extension), plot = plot, device = extension,
                  path = ifelse(exists("save_path"), save_path, getwd()),
                  scale = 1, width = plot_width, height = plot_height, units = c("cm"), dpi = dpi)
  }

  plot

}


