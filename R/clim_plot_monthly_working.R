#' clim_plot_monthly_working
#'
#' Returns a boxplot of monthly climate data
#' @return A ggplot2 object of monthly climate data
#' @export

# Function to plot monthly climate data as a boxplot

clim_plot_monthly_working <- function(
    site,
    parameter,
    select_year,
    water_year_start = 1,
    water_year_end = NA,
    water_year,
    start_year = 1950,
    end_year = 2025,
    max_missing_days = 3,
    y_min = NA,
    y_max = NA,
    select_year_point_size = 2,
    historic_point_size = 1,
    legend_position = c(0.1, 0.95),
    save = FALSE,
    plot_width = 16,
    plot_height = 10,
    dpi = 900,
    file_name = "Default climate plot",
    extension = "png"
)

{

  if(length(site) > 1) {
    stop("Only one site can be included for monthly plots. Try using annual plots instead")
  }

  # Define variables
  parameter <- clim_parameter(parameter = parameter)[[1]]
  plot_title <- clim_parameter(parameter = parameter)[[2]]
  y_axis_title <- clim_parameter(parameter = parameter)[[3]]
  point_colour <- clim_parameter(parameter = parameter)[[4]]

  summary_data <- clim_calc_monthly_working(
    site = site,
    parameter = parameter,
    start_year = start_year,
    end_year = end_year,
    select_year = select_year,
    water_year_start = water_year_start,
    water_year = water_year
  )

  plot_data <- summary_data

  #The line below is for manually editing Fort Simpson, Peace River and Inuvik precip
  if(unique(plot_data$Parameter) == "total_precip"){
    plot_data$MissingDays[plot_data$Site=="Fort Simpson"&plot_data$Year==2024&plot_data$MonthName =="Jun"] <- 0
    plot_data$MissingDays[plot_data$Site=="Fort Simpson"&plot_data$Year==2024&plot_data$MonthName =="Jul"] <- 0
    plot_data$MissingDays[plot_data$Site=="Peace River"&plot_data$Year==2024&plot_data$MonthName =="Jul"] <- 0
    plot_data$MissingDays[plot_data$Site=="Inuvik"&plot_data$Year==2024&plot_data$MonthName =="Jul"] <- 0
    plot_data$MissingDays[plot_data$Site=="Inuvik"&plot_data$Year==2025&plot_data$MonthName =="May"] <- 0
    plot_data$MissingDays[plot_data$Site=="Inuvik"&plot_data$Year==2025&plot_data$MonthName =="Jun"] <- 0
  }

  # Filter summary_data to max_missing_days argument
  plot_data <- dplyr::filter(plot_data, MissingDays <= max_missing_days)

  if(unique(plot_data$Parameter) == "total_precip"){
    #The line below is for manually editing Fort Simpson, Peace River and Inuvik precip
    plot_data$Value[plot_data$Site=="Fort Simpson"&plot_data$Year==2024&plot_data$MonthName == "Jun"] <- 32.1 #pulled from FTS stn
    plot_data$Value[plot_data$Site=="Fort Simpson"&plot_data$Year==2024&plot_data$MonthName == "Jul"] <- 59.6 #pulled from FTS stn
    plot_data$Value[plot_data$Site=="Peace River"&plot_data$Year==2024&plot_data$MonthName == "Jul"] <- 49.8 #pulled from ROMA
    plot_data$Value[plot_data$Site=="Inuvik"&plot_data$Year==2024&plot_data$MonthName == "Jul"] <- 50.6 #pulled from FTS stn
    plot_data$Value[plot_data$Site=="Inuvik"&plot_data$Year==2025&plot_data$MonthName =="May"] <- 10.7 #pulled from FTS stn
    plot_data$Value[plot_data$Site=="Inuvik"&plot_data$Year==2025&plot_data$MonthName =="June"] <- 18.3 #pulled from FTS stn
  }

  # Trim data to specific months
  if(!is.na(water_year_end)) {
    if(water_year_start > water_year_end) {
      months <- c(water_year_start:12, 1:water_year_end)
    } else if(water_year_start < water_year_end) {
      months <- (water_year_start:water_year_end)
    } else if (water_year_start == water_year_end) {
      months <- water_year_start
    }
    plot_data <- dplyr::filter(plot_data, Month %in% months)
  }

  # Choose a year to highlight on the plot
  plot_year <- plot_data$Year == select_year

  plot <-  ggplot2::ggplot(plot_data, ggplot2::aes(x = MonthName, y = Value)) + #editing: insert Year=Year in aes()
    ggplot2::geom_boxplot(notch = F, outlier.shape = NA, outlier.colour = NA) +
    ggplot2::geom_jitter(data = plot_data[!plot_year, ], colour = "grey", alpha = 0.75, size = historic_point_size) +
    ggplot2::geom_point(data = plot_data[plot_year, ], ggplot2::aes(colour="red"), alpha = 1, size = select_year_point_size) +
    ggplot2::theme_classic() +
    ggplot2::scale_colour_manual("", labels = c("2025"), values = point_colour) +
    ggplot2::theme(legend.position = legend_position) +
    ggplot2::labs(title = paste0(site, " ", plot_title),
                  x = "Month",
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
