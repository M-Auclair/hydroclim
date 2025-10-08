#' clim_plot_dayofyear
#'
#' Returns a boxplot of climate data for each day of the year
#' @return A ggplot2 object of climate data
#' @export

# Function to plot climate data to the current date as a boxplot

# Function is still in development

clim_plot_dayofyear <- function(
    site = "Fort Chipewyan",
    parameter = "rain",
    start_year = 1953,
    end_year = 2021,
    select_year = 2022,
    cumulative,
    plot_width,
    plot_height,
    y_min = NA,
    y_max = NA
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
    start_year = start_year,
    end_year = end_year,
    select_year = select_year
  )

  if(cumulative == TRUE){

    if (parameter == "rain"){
      plot_title = "Cumulative Rain"
      y_axis_title = "Cumulative Rain (mm)"
    }

    if (parameter == "total_precip"){
      plot_title = "Cumulative Precipitation"
      y_axis_title = "Cumulative Precipitation (mm)"
    }

    if (parameter == "SWE_mm"){
      plot_title = "Cumulative Snow Water Equivalent"
      y_axis_title = "Cumulative SWE (mm)"
    }

    data <- data %>%
      dplyr::group_by(year) %>%
      dplyr::mutate(Count = ifelse((is.na(Value)==T), 1, 0),
                    Missing = sum(Count)) %>%
      dplyr::filter(Missing <= 10)%>% # Group by year
      dplyr::filter(sum(!is.na(Value)) >= 350) # filter cumulative values for years with less than 10 missing days

    data <- data %>%
      dplyr::group_by(year) %>%
      dplyr::mutate(cum_value = cumsum(tidyr::replace_na(Value, 0)))

    data <- data %>%
      dplyr::mutate(dayofyear = lubridate::yday(Date)) %>% # Add julian day column
      dplyr::group_by(dayofyear) %>% # Group by julian day
      dplyr::mutate(prctile = (ecdf(cum_value)(cum_value))*100,
                    Max = max(cum_value, na.rm = TRUE),
                    Min = min(cum_value, na.rm = TRUE),
                    Q90 = quantile(cum_value, 0.90, na.rm = T),
                    Q75 = quantile(cum_value, 0.75, na.rm = TRUE),
                    Q25 = quantile(cum_value, 0.25, na.rm = TRUE),
                    Q10 = quantile(cum_value, 0.10, na.rm = T)) %>%
      subset(year == select_year)

    plot <- ggplot2::ggplot(data, ggplot2::aes(x = Date, y = cum_value)) +
      ggplot2::geom_ribbon(ggplot2::aes(ymin = Min, ymax = Max, fill = "Min - Max")) +
      ggplot2::geom_ribbon(ggplot2::aes(ymin = Q10, ymax = Q90, fill = "10th - 90th Percentile")) +
      ggplot2::geom_ribbon(ggplot2::aes(ymin = Q25, ymax = Q75, fill = "Average Range")) +
      ggplot2::scale_fill_manual(name = "",
                                 breaks = c("Average Range", "10th - 90th Percentile", "Min - Max"),
                                 values = c("gray75", "gray85", "gray95")) +
      ggplot2::theme_classic() +
      ggplot2::labs(title = paste(select_year, site, plot_title, sep = " "),
                    x = "Month", y = y_axis_title) +
      ggplot2::scale_x_date(date_breaks = "1 months",
                            labels = scales::date_format("%b"),
                            limits = as.Date(c(paste(select_year, "-01-01", sep = ""),
                                               paste(select_year, "-12-31", sep = "")))) +
      # ggplot2::geom_hline(yintercept = 0) +
      ggplot2::geom_line(ggplot2::aes(colour = factor(year))) +
      ggplot2::geom_point(ggplot2::aes(colour = factor(year)), shape = 19, size = 0.5) +
      ggplot2::coord_cartesian(ylim = c(y_min, y_max)) +
      ggplot2::scale_colour_manual(name = "", values = point_colour) +
      ggplot2::theme(legend.position = "top")

    plot

    ggplot2::ggsave(paste(site, select_year, plot_title,".png", sep="_"), plot = plot, device = "png",
                    path = ifelse(exists("save_path"), save_path, getwd()),
                    scale = 1, width = plot_width, height = plot_height, units = c("cm"), dpi = 300)

    plot
  } else {


    data <- data %>%
      dplyr::mutate(dayofyear = lubridate::yday(Date)) %>% # Add julian day column
      dplyr::group_by(dayofyear) %>% # Group by julian day
      dplyr::mutate(prctile = (ecdf(Value)(Value))*100,
                    Max = max(Value, na.rm = TRUE),
                    Min = min(Value, na.rm = TRUE),
                    Q90 = quantile(Value, 0.90, na.rm = T),
                    Q75 = quantile(Value, 0.75, na.rm = TRUE),
                    Q25 = quantile(Value, 0.25, na.rm = TRUE),
                    Q10 = quantile(Value, 0.10, na.rm = T)) %>%
      subset(year == select_year)

    plot <- ggplot2::ggplot(data, ggplot2::aes(x = Date, y = Value)) +
      ggplot2::geom_ribbon(ggplot2::aes(ymin = Min, ymax = Max, fill = "Min - Max")) +
      ggplot2::geom_ribbon(ggplot2::aes(ymin = Q10, ymax = Q90, fill = "10th - 90th Percentile")) +
      ggplot2::geom_ribbon(ggplot2::aes(ymin = Q25, ymax = Q75, fill = "Average Range")) +
      ggplot2::scale_fill_manual(name = "",
                                 breaks = c("Average Range", "10th - 90th Percentile", "Min - Max"),
                                 values = c("gray75", "gray85", "gray95")) +
      ggplot2::theme_classic() +
      ggplot2::labs(title = paste(select_year, site, plot_title, sep = " "),
                    x = "Month", y = y_axis_title) +
      ggplot2::scale_x_date(date_breaks = "1 months",
                            labels = scales::date_format("%b")) +
      # tidyquant::coord_x_date(xlim = c(paste(select_year, "-01-01", sep = ""),
      #                                  paste(select_year, "-12-31", sep = ""))) +
      # ggplot2::geom_hline(yintercept = 0) +
      ggplot2::geom_line(ggplot2::aes(colour = factor(year))) +
      ggplot2::geom_point(ggplot2::aes(colour = factor(year)), shape = 19, size = 0.5) +
      ggplot2::scale_colour_manual(name = "", values = point_colour) +
      ggplot2::theme(legend.position = "top")

    plot

    ggplot2::ggsave(paste(site, select_year, plot_title, ".png", sep="_"), plot = plot, device = "png",
                    path = ifelse(exists("save_path"), save_path, getwd()),
                    scale = 1, width = plot_width, height = plot_height, units = c("cm"), dpi = 300)

    plot
  }
}




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



