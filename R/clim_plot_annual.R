#' clim_plot_annual
#'
#' Returns a boxplot of annual climate data
#' @return A ggplot2 object of annual climate data
#' @export

# Function to plot annual climate data as a boxplot

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