#' clim_plot_annual
#' Function to plot annual climate data as a boxplot
#' @param site A character vector of site names
#' @param parameter The input parameter
#' @param start_year The start year
#' @param end_year The end year
#' @param select_year The year selected to be examined
#' @param water_year_start The month number indicating the start of the year
#' @param max_missing_days The maximum missing days allowed for summarized data
#' @param y_min The lower limit of plot y-axis
#' @param y_max The upper limit of plot y-axis
#' @param select_year_point_size The geom_point size of select year points
#' @param historic_point_size The geom_point size of historic years
#' @param legend_position The position of the legend on the plot
#' @param save Option to save plot - TRUE or FALSE
#' @param plot_width Width of the plot in cm
#' @param plot_height Height of the plot in cm
#' @param dpi dots per inch resolution of plot
#' @param file_name Name of plot file if save = TRUE
#' @param extension File type of plot is save = TRUE

#' Returns a boxplot of annual climate data
#' @return A ggplot2 object of annual climate data
#' @export

#

clim_plot_annual <- function(
    site = c("Hay River", "Fort Smith", "Yellowknife"),
    parameter = "precip",
    select_year = lubridate::year(Sys.Date()),
    water_year_start = 10,
    start_year = 1950,
    end_year = 2024,
    max_missing_days = 3,
    y_min = NA,
    y_max = NA,
    select_year_point_size = 2,
    historic_point_size = 1,
    legend_position = c(0.1, 0.95),
    save = FALSE,
    plot_width = 18,
    plot_height = 11,
    dpi = 900,
    file_name = "Default climate plot",
    extension = "png"
)


{
  # added condition for if site == "all" April 2024 - MA
  if("all" %in% site ) {
    stop("Cannot plot all sites. Try inputting a few sites instead")
  }

   # Define variables
  parameter <- clim_parameter(parameter = parameter)[[1]]
  plot_title <- clim_parameter(parameter = parameter)[[2]]
  y_axis_title <- clim_parameter(parameter = parameter)[[3]]
  point_colour <- clim_parameter(parameter = parameter)[[4]]

  plot_data <- clim_calc_annual(
    site = site,
    parameter = parameter,
    start_year = start_year,
    end_year = end_year,
    select_year = select_year,
    water_year_start = water_year_start
  )

  # Filter summary_data to max_missing_days argument
  plot_data <- dplyr::filter(plot_data, MissingDays <= max_missing_days)

  # Choose a year to highlight on the plot
  plot_year <- plot_data$Year == select_year

  plot_data$Site <- ordered(plot_data$Site, site)

  plot <-  ggplot2::ggplot(plot_data, ggplot2::aes(x = Site, y = Value, group = Site)) +
    ggplot2::geom_boxplot(notch = F, outlier.shape = NA) +
    ggplot2::geom_jitter(data = plot_data[!plot_year, ], colour = "grey", alpha = 0.75, size = historic_point_size) +
    ggplot2::geom_point(data = plot_data[plot_year, ], ggplot2::aes(colour = "red"), alpha = 1, size = select_year_point_size) +
    ggplot2::theme_classic() +
    ggplot2::scale_colour_manual("", labels = paste(select_year), values = point_colour) +
    ggplot2::theme(legend.position = legend_position) +
    ggplot2::labs(title = plot_title,
                  x = "Location",
                  y = y_axis_title)


  if(!is.na(y_min) == T) {
    plot <- plot +
      ggplot2::ylim(y_min, y_max)
  }

  if(save == TRUE) {
    ggplot2::ggsave(paste0(file_name, ".", extension), plot = plot, device = extension,
                    path = ifelse(exists("save_path"), save_path, getwd()),
                    scale = 1, width = plot_width, height = plot_height, units = c("cm"), dpi = dpi)
  }

  plot

}
