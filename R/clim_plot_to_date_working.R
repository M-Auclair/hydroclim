#' clim_plot_to_date_working
#'
#' Returns a boxplot of climate data up to a certain date
#' @return A ggplot2 object of climate data
#' @export

# Function to plot climate data to the current date as a boxplot

clim_plot_to_date_working <- function(
    site = c("Mackenzie"),
    parameter = "precip",
    select_year = lubridate::year(Sys.Date()),
    water_year_start = 4,
    start_year = 1950,
    end_year = 2025,
    end_date = Sys.Date(),
    max_missing_days = 10,
    y_min = NA,
    y_max = NA,
    plot_break = FALSE,
    select_year_point_size = 2,
    historic_point_size = 1,
    legend_position = c(0.1, 0.95),
    save = FALSE,
    plot_width = 16, #usually 18
    plot_height = 10, #usually 11
    dpi = 900,
    file_name = "adjusted climate plot",
    extension = "png",
    water_year
)

{
  # Define variables
  parameter <- clim_parameter(parameter = parameter)[[1]]
  plot_title <- clim_parameter(parameter = parameter)[[2]]
  y_axis_title <- clim_parameter(parameter = parameter)[[3]]
  point_colour <- clim_parameter(parameter = parameter)[[4]]

  # Manually adjust 'today' to the end_date
  today <- end_date

  plot_data <- clim_calc_daily(
    site = site,
    parameter = parameter,
    start_year = start_year,
    end_year = end_year,
    select_year = select_year,
    water_year_start = water_year_start
  )

  #plot_data <- dplyr::select(plot_data, -CalendarYear)

  # Remove NA values at end of year for each site

  plot_data <- dplyr::filter(plot_data, Date <= today)

  # Find current julian day of the water year (i.e. 1 = first day of water year)

  today_JD <- max(dplyr::select(dplyr::filter(plot_data, Date == today), DayofYear))

  # Filter out all data after current julian day

  plot_data <- dplyr::filter(plot_data, DayofYear < today_JD,
                             WaterYear > min(plot_data$WaterYear))

  # Filter summary_data to max_missing_days argument

  if(water_year == "TRUE"){

    plot_data <- dplyr::reframe(dplyr::group_by(plot_data, Site, WaterYear),
                                Value = sum(Value, na.rm = T),
                                MissingDays = sum(Count))
  }else{
    plot_data <- dplyr::reframe(dplyr::group_by(plot_data, Site, CalendarYear),
                                Value = sum(Value, na.rm = T),
                                MissingDays = sum(Count))
  }

  #use the line below to manually edit values
  # plot_data$MissingDays[plot_data$Site=="Fort Simpson"&plot_data$CalendarYear==2024] <- 0
  # plot_data$MissingDays[plot_data$Site=="Peace River"&plot_data$CalendarYear==2024] <- 0
  # plot_data$MissingDays[plot_data$Site=="Inuvik"&plot_data$CalendarYear==2024] <- 0
  # plot_data$MissingDays[plot_data$Site=="Athabasca"&plot_data$CalendarYear==2025] <- 0
  # plot_data$MissingDays[plot_data$Site=="Athabasca"&plot_data$WaterYear==2026] <- 0
  plot_data$MissingDays[plot_data$Site=="Inuvik"&plot_data$CalendarYear==2025] <- 0
  plot_data$MissingDays[plot_data$Site=="Fort Liard"&plot_data$CalendarYear==2025] <- 0


  plot_data <- dplyr::filter(plot_data, MissingDays <= max_missing_days)

  #use the line below to manually edit values
  # plot_data$Value[plot_data$Site=="Fort Simpson"&plot_data$CalendarYear==2024] <- 145.6 #ER - updated Nov 4 2024 (from April 1 2024) using a combination of FTS and ECCC values
  # plot_data$Value[plot_data$Site=="Inuvik"&plot_data$CalendarYear==2024] <- 246.4 #ER - updated Nov 4 2024 (from April 1 2024) using a combination of FTS and ECCC values
  # plot_data$Value[plot_data$Site=="Athabasca"&plot_data$WaterYear==2025] <- 125.1 #ER - updated April 11
  # plot_data$Value[plot_data$Site=="Athabasca"&plot_data$CalendarYear==2025] <- 83.8 #ER - updated June 4 for MTS presentation
  # plot_data$Value[plot_data$Site=="Athabasca"&plot_data$WaterYear==2024] <- 212.1 #ER - updated May 9 for MTS water level presentation
  # plot_data$Value[plot_data$Site=="Athabasca"&plot_data$WaterYear==2026] <- 83.8 #ER - updated June 4 for MTS water level presentation
  plot_data$Value[plot_data$Site=="Inuvik"&plot_data$CalendarYear==2025] <- 41 #Using Shell Lake FTS data - updated July 8th
  plot_data$Value[plot_data$Site=="Fort Liard"&plot_data$CalendarYear==2025] <- 53 #Using Fort Liard FTS data - updated July 8th


  # Choose a year to highlight on the plot

  if(water_year == "TRUE"){
    plot_year <- plot_data$WaterYear == select_year
  } else{
    plot_year <- plot_data$CalendarYear == select_year
  }

  plot_data$Site <- ordered(plot_data$Site, site)

  #You must change the title, subtitle and year in this plot
  plot <-  ggplot2::ggplot(plot_data, ggplot2::aes(x = Site, y = Value, group = Site)) +
    ggplot2::geom_boxplot(notch = F, outlier.shape = NA) +
    ggplot2::geom_jitter(data = plot_data[!plot_year, ], colour = "grey", alpha = 0.75, size = historic_point_size) +
    ggplot2::geom_point(data = plot_data[plot_year, ], ggplot2::aes(colour = "red"), alpha = 1, size = select_year_point_size) +
    ggplot2::theme_classic() +
    ggplot2::scale_colour_manual("", labels = paste("2025"), values = point_colour) +
    ggplot2::theme(legend.position = legend_position) +
    ggplot2::labs(title = paste0("Cumulative precipitation for select NWT communities", plot_title),
      subtitle = expression(paste("April 1"^st, " 2025 to August 1"^st, " 2025")),
      x = "Location",
      y = y_axis_title)

  if(!is.na(y_min) == TRUE) {
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

############################################################################

# if(plot_break = T) {
#
#   max_value <- roundUp(max(plot_data$Value), 10)
#   n <- length(plot_data$Value)
#   next_value <- sort(plot_data$Value, partial = n-1)[n-1]
#
#   plot <- plot +
#     ggbreak::scale_y_break(c(next_value + 5, max_value - 30), ticklabels = max_value) +
#     ggplot2::theme(legend.position = "top")
#
# }

