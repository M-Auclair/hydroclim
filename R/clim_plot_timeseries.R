#' clim_plot_timeseries
#' Function to plot daily climate data as timeseries
#' @param site A character vector of site names
#' @param parameter The input parameter
#' @param data_int Interval of data to be plotted - daily, monthly, or annual
#' @param start_year The start year
#' @param end_year The end year
#' @param select_year Should be NA
#' @param water_year_start The month number indicating the start of the year
#' @param water_year_end The month number indicating the end of the year
#' @param max_missing_days The maximum missing days allowed for summarized data
#' @param zoom_x Option to zoom into plot: c(as.Date("yyyy-mm-dd"), as.Date("yyyy-mm-dd")) or NULL
#' @param zoom_y Option to zoom into plot: c(as.Date("yyyy-mm-dd"), as.Date("yyyy-mm-dd")) or NULL
#' @param line_colours Option to select colours for sites plotted: c("blue","red","green"...)
#' @param legend_position The position of the legend on the plot
#' @param line_size Linewidth
#' @param save_data Option to save data used to create plot: TRUE or FALSE
#' @param save_plot Option to save plot
#' @param file_name Name of plot file if save = TRUE
#' @param extension Plot file type extension: ex. "png"
#' @param plot_width Width of the plot in cm
#' @param plot_height Height of the plot in cm
#' @param dpi dots per inch resolution of plot
#'
#' @return A ggplot2 object of climate data
#' @export

#

clim_plot_timeseries <- function(
    site,
    parameter,
    data_int,
    start_year,
    end_year,
    select_year,
    water_year_start,
    water_year_end,
    max_missing_days,
    zoom_x = NULL,
    zoom_y = NULL,
    line_colours,
    legend_position,
    line_size,
    save_data,
    save_plot,
    file_name,
    extension,
    plot_width,
    plot_height,
    dpi
)

{    # Define variables
  parameter <- clim_parameter(parameter = parameter)[[1]]
  plot_title <- clim_parameter(parameter = parameter)[[2]]
  y_axis_title <- clim_parameter(parameter = parameter)[[3]]
  date_range <- paste(start_year, "to", end_year)

  # added condition for if site == "all" April 2024 - MA
  if("all" %in% site ) {
    stop("Cannot plot all sites. Try inputting a few sites instead")
  }

  ## Daily plotting option
  if(data_int=="daily" | data_int=="Daily" | data_int=="day" | data_int=="Day"){
    # Gather data
    plot_data <- clim_calc_daily(
      site = site,
      parameter = parameter,
      start_year = start_year,
      end_year = end_year,
      select_year = select_year,
      water_year_start = water_year_start
    )

    dplyr::as_tibble(plot_data)
    plot_data$Site <- ordered(plot_data$Site, site)

    # Site name for plot title
    plot_site <- ifelse(length(site) == 1, site, paste(site, collapse = " and ", sep = ", "))

    # daily time series
    for (i in seq_along(parameter)) {
      # Check if the parameter is one of the specified parameters for geom_bar
      if (parameter[i] %in% c("total_precip", "rain", "total_snow")) {
        # assign 0 to NA values for bar plots
        plot_data$Value <- replace(plot_data$Value, is.na(plot_data$Value), 0)

        # Create a ggplot for the current parameter with geom_bar
        plot <- ggplot2::ggplot(plot_data, ggplot2::aes(x = Date, y = Value, fill = as.factor(Site))) +
          ggplot2::geom_bar(stat = "identity", position = ggplot2::position_dodge2(preserve="single")) +
          ggplot2::labs(title = paste0(plot_title),
                        subtitle = paste0(date_range),
                        x = "Date",
                        y = y_axis_title, fill = NULL) +
          ggplot2::theme(legend.position = legend_position) +
          ggplot2::scale_fill_manual(name = "",
                                     values = line_colours,
                                     na.translate = FALSE) +# eliminates 'NA' from legend
          ggplot2::scale_x_date() +
          ggplot2::scale_y_continuous()+
          ggplot2::theme_classic()
      } else {
        # Create a ggplot for the current parameter with geom_line
        plot <- ggplot2::ggplot(plot_data, ggplot2::aes(x = Date, y = Value, color = as.factor(Site)), group=1) +
          ggplot2::geom_line(linewidth = line_size, na.rm=TRUE) +
          ggplot2::geom_blank(data = complete(plot_data, Date = seq(min(Date), max(Date), by = "day"), Site), aes(x = Date, y = Value))+
          ggplot2::labs(title = paste0(plot_title),
                        subtitle = paste0(date_range),
                        x = "Date",
                        y = y_axis_title, color = NULL) +
          ggplot2::theme(legend.position = legend_position) +
          ggplot2::scale_colour_manual(name = "",
                                       values = line_colours,
                                       na.translate = FALSE) +# eliminates 'NA' from legend
          ggplot2::scale_x_date() +
          ggplot2::theme_classic()
      }
      if (!is.null(zoom_x) || !is.null(zoom_y)) {
        plot <- plot +
          ggplot2::coord_cartesian(
            xlim = zoom_x,
            ylim = zoom_y
          )
      }

      print(plot)
    }
    # Create a tibble of data
    selected_data_tibble <- tibble::as_tibble(plot_data)

    if (save_data) {
      saveRDS(selected_data_tibble, file.path(data_path, paste0(file_name, ".rds")))
    }
    if(save_plot) {
      ggplot2::ggsave(paste0(file_name, ".", extension), plot = plot, device = extension,
                      path = ifelse(exists("save_path"), save_path, getwd()),
                      scale = 1, width = plot_width, height = plot_height, units = c("cm"), dpi = dpi)
    }

    return(selected_data_tibble)
  }

  ## Monthly plotting option
  if(data_int == "month" | data_int=="Month" | data_int=="monthly" | data_int=="Monthly"){
    # Gather data
    summary_data <- clim_calc_monthly(
      site = site,
      parameter = parameter,
      start_year = start_year,
      end_year = end_year,
      select_year = select_year,
      water_year_start = water_year_start
    )

    # Filter summary_data to max_missing_days argument
    plot_data <- dplyr::filter(summary_data, MissingDays <= max_missing_days)

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

    dplyr::as_tibble(plot_data)
    plot_data$YearMon <- sprintf("%04d-%02d", plot_data$Year, plot_data$Month)
    plot_data$YearMon <- lubridate::ym(plot_data$YearMon)
    plot_data$Site <- ordered(plot_data$Site, site)

    # Site name for plot title
    plot_site <- ifelse(length(site) == 1, site, paste(site, collapse = " and ", sep = ", "))


    # daily time series
    for (i in seq_along(parameter)) {
      # Check if the parameter is one of the specified parameters for geom_bar
      if (parameter[i] %in% c("total_precip", "rain", "total_snow")) {
        # assign 0 to NA values for bar plots
        plot_data$Value <- replace(plot_data$Value, is.na(plot_data$Value), 0)

        # Create a ggplot for the current parameter with geom_bar
        plot <- ggplot2::ggplot(plot_data, ggplot2::aes(x = YearMon, y = Value, fill = as.factor(Site))) +
          ggplot2::geom_bar(stat = "identity", position = ggplot2::position_dodge2(preserve="single")) +
          ggplot2::labs(title = paste0(" Monthly ", plot_title),
                        subtitle = paste0(date_range),
                        x = "",
                        y = y_axis_title, fill = NULL) +
          ggplot2::theme(legend.position = legend_position) +
          ggplot2::scale_fill_manual(name = "",
                                     values = line_colours) +# na.translate = FALSE eliminates 'NA' from legend
          ggplot2::scale_x_date() +
          ggplot2::theme_classic()
      } else {
        # Create a ggplot for the current parameter with geom_line
        plot <- ggplot2::ggplot(plot_data, ggplot2::aes(x = YearMon, y = Value, color = as.factor(Site))) +
          ggplot2::geom_line(linewidth = line_size) +
          ggplot2::labs(title = paste0(" Monthly ", plot_title),
                        subtitle = paste0(date_range),
                        x = "",
                        y = y_axis_title, color = NULL) +
          ggplot2::theme(legend.position = legend_position) +
          ggplot2::scale_colour_manual(name = "",
                                       values = line_colours,
                                       na.translate = FALSE) +# eliminates 'NA' from legend
          ggplot2::scale_x_date() +
          ggplot2::theme_classic()
      }
      if (!is.null(zoom_x) || !is.null(zoom_y)) {
        plot <- plot +
          ggplot2::coord_cartesian(
            xlim = zoom_x,
            ylim = zoom_y
          )
      }

      print(plot)
    }
    # Create a tibble of data
    selected_data_tibble <- tibble::as_tibble(plot_data)
    # remove day from yyyy-mm-dd format (01 was added for plotting purposes)
    selected_data_tibble$YearMon <- format(selected_data_tibble$YearMon, "%Y-%m")
    if (save_data) {
      saveRDS(selected_data_tibble, file.path(data_path, paste0(file_name, ".rds")))
    }
    if(save_plot) {
      ggplot2::ggsave(paste0(file_name, ".", extension), plot = plot, device = extension,
                      path = ifelse(exists("save_path"), save_path, getwd()),
                      scale = 1, width = plot_width, height = plot_height, units = c("cm"), dpi = dpi)
    }
    return(selected_data_tibble)
  }

  ## Annual plotting option
  if(data_int == "year" | data_int=="Year" | data_int=="annual" | data_int=="Annual" |
     data_int=="yearly" | data_int=="Yearly" | data_int=="annually" | data_int=="Annually"){
    # Gather data
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

    #plot_data$Date <- as.Date(plot_data$Date)

    plot_data$Site <- ordered(plot_data$Site, site)

    # Site name for plot title
    plot_site <- ifelse(length(site) == 1, site, paste(site, collapse = " and ", sep = ", "))

    # daily time series
    for (i in seq_along(parameter)) {
      # Check if the parameter is one of the specified parameters for geom_bar
      if (parameter[i] %in% c("total_precip", "rain", "total_snow")) {
        # assign 0 to NA values for bar plots
        plot_data$Value <- replace(plot_data$Value, is.na(plot_data$Value), 0)
        # Create a ggplot for the current parameter with geom_bar
        plot <- ggplot2::ggplot(plot_data, ggplot2::aes(x = Year, y = Value, fill = as.factor(Site))) +
          ggplot2::geom_bar(stat = "identity", position = ggplot2::position_dodge2(preserve="single")) +
          ggplot2::labs(title = paste0(" Annual ", plot_title),
                        subtitle = paste0(date_range),
                        x = "",
                        y = y_axis_title, fill = NULL) +
          ggplot2::theme(legend.position = legend_position) +
          ggplot2::scale_fill_manual(name = "",
                                     values = line_colours,
                                     na.translate = FALSE) +# eliminates 'NA' from legend
          ggplot2::theme_classic()
      } else {
        # Create a ggplot for the current parameter with geom_line
        plot <- ggplot2::ggplot(plot_data, ggplot2::aes(x = Year, y = Value, color = as.factor(Site))) +
          ggplot2::geom_line(linewidth = line_size) +
          ggplot2::labs(title = paste0(" Annual ", plot_title),
                        subtitle = paste0(date_range),
                        x = "",
                        y = y_axis_title, color = NULL) +
          ggplot2::theme(legend.position = legend_position) +
          ggplot2::scale_colour_manual(name = "",
                                       values = line_colours,
                                       na.translate = FALSE) +# eliminates 'NA' from legend
          ggplot2::theme_classic()
      }
      if (!is.null(zoom_x) || !is.null(zoom_y)) {
        plot <- plot +
          ggplot2::coord_cartesian(
            xlim = zoom_x,
            ylim = zoom_y
          )
      }

      print(plot)

      cat("One plot is output per parameter. See previous plot panes if multiple parameters were input.\n")
    }
    # Create a tibble of data
    selected_data_tibble <- tibble::as_tibble(plot_data)

    if (save_data) {
      saveRDS(selected_data_tibble, file.path(data_path, paste0(file_name, ".rds")))
    }
    if(save_plot) {
      ggplot2::ggsave(paste0(file_name, ".", extension), plot = plot, device = extension,
                      path = ifelse(exists("save_path"), save_path, getwd()),
                      scale = 1, width = plot_width, height = plot_height, units = c("cm"), dpi = dpi)
    }

    return(selected_data_tibble)
  }

  }
