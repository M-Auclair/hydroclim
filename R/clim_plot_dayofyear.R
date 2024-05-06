#' clim_plot_dayofyear
#'
#' Returns a boxplot of climate data for each day of the year
#' @return A ggplot2 object of climate data
#' @export

# Function to plot climate data to the current date as a boxplot

# Function is still in development

clim_plot_dayofyear <- function(
  site = "Fort Simpson",
  parameter = "mean_temp",
  start_year = 1981,
  end_year = 2024,
  select_year = 2022
  )
  
{
  # Define variables
  parameter <- clim_parameter(parameter = parameter)[[1]]
  plot_title <- clim_parameter(parameter = parameter)[[2]]
  y_axis_title <- clim_parameter(parameter = parameter)[[3]]
  point_colour <- clim_parameter(parameter = parameter)[[4]]
  
  # Set working directory and load data
  data <- clim_compile_daily(
    site = site,
    parameter = parameter,
    start_year = 1981,
    end_year = 2010,
    select_year = select_year
  )

  data <- data %>%
    dplyr::mutate(dayofyear = lubridate::yday(Date)) %>% # Add julian day column
    dplyr::group_by(dayofyear) %>% # Group by julian day
    dplyr::mutate(prctile = (ecdf(Value)(Value))*100,
                  Max = max(Value, na.rm = TRUE),
                  Min = min(Value, na.rm = TRUE),
                  Q75 = quantile(Value, 0.75, na.rm = TRUE),
                  Q25 = quantile(Value, 0.25, na.rm = TRUE)) %>%
    subset(year == select_year)
  
  plot <- ggplot2::ggplot(data, ggplot2::aes(x = Date, y = Value)) + 
    ggplot2::geom_ribbon(ggplot2::aes(ymin = Min, ymax = Max, fill = "Min - Max")) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = Q25, ymax = Q75, fill = "Average Range")) +
    ggplot2::scale_fill_manual(name = "", 
                               values = c("Min - Max" = "gray85",
                                          "Average Range" = "gray75")) +
    ggplot2::theme_classic() +
    ggplot2::labs(title = paste(select_year, site, plot_title, sep = " "),
                  x = "Month", y = y_axis_title) +
    ggplot2::scale_x_date(date_breaks = "1 months",
                          labels = scales::date_format("%b")) +
    tidyquant::coord_x_date(xlim = c(paste(select_year, "-01-01", sep = ""),
                                     paste(select_year, "-12-31", sep = ""))) +
    ggplot2::geom_hline(yintercept = 0) +
    ggplot2::geom_line(ggplot2::aes(colour = factor(year))) +
    ggplot2::geom_point(ggplot2::aes(colour = factor(year)), shape = 19, size = 0.5) +
    ggplot2::scale_colour_manual(name = "", values = point_colour) + 
    ggplot2::theme(legend.position = "top")
  
  plot
  
  ggplot2::ggsave(paste("Yellowknife", select_year, "Temp.png", sep="_"), plot = plot, device = "png",
                  path = savepath_Hydrometric,
                  scale = 1, width = 20, height = 10, units = c("cm"), dpi = 300)
  
  plot
  
}



