#' clim_plot_dayofyear
#'
#' Returns a boxplot of climate data for each day of the year
#' @return A ggplot2 object of climate data
#' @export

# Function to plot climate data to the current date

clim_plot_dayofyear <- function(
    site = "Fort Chipewyan",
    parameter = "rain",
    start_year = 1953,
    end_year = 2021,
    select_years = c(2022, 2023),
    cumulative = T,
    plot_width = 18,
    plot_height = 11,
    y_min = NA,
    y_max = NA,
    line_size = 1,
    point_size = 0.5,
    water_year = FALSE,
    water_year_start = 10,
    point_colours
)

{
  
  # Define variables
  parameter <- clim_parameter(parameter = parameter)[[1]]
  plot_title <- clim_parameter(parameter = parameter)[[2]]
  y_axis_title <- clim_parameter(parameter = parameter)[[3]]
  point_colour <- clim_parameter(parameter = parameter)[[4]]
  
  if (dplyr::n_distinct(select_years) > 1){
    point_colour = point_colours
    ref_year <- max(select_years)
  } else {
    ref_year <- max(select_years)
  }
  
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
  }
  
  # Set working directory and load data
  data <- clim_calc_daily(
    site = site,
    parameter = parameter,
    start_year = start_year,
    end_year = end_year,
    select_years = select_years,
    water_year_start = water_year_start
  )
  
  today    <- Sys.Date() - 1L
  this_yr  <- lubridate::year(today)
  today_jd <- lubridate::yday(today)
  
    #For cumulative water year = TRUE first
    
    if(cumulative == TRUE && water_year == TRUE){
      
      data <- data %>%
        dplyr::filter(Date <= today)
    
      data_full <- data %>%
        dplyr::group_by(WaterYear) %>%
        dplyr::mutate(
          cum_value = cumsum(tidyr::replace_na(Value, 0))
        ) %>%
        dplyr::ungroup()
      
      data_stats <- data_full %>%
        dplyr::group_by(WaterYear) %>%
        dplyr::mutate(
          Missing = sum(Count),
          NonNA   = sum(!is.na(Value))
        ) %>%
        dplyr::ungroup() %>%
        dplyr::filter(
          WaterYear >= start_year,
          WaterYear <= end_year,     # only past years for normals
          Missing <= 10,
          NonNA   >= 350
        )
      
      origin_date <- as.Date(
        paste0(ref_year - 1, "-", sprintf("%02d", water_year_start), "-01")
      )
    
      #creating historical data IQR, 10-90th percentiles and min/max
      clim_stats <- data_stats %>%
        dplyr::group_by(DayofYear) %>%
        dplyr::summarise(
          Max = max(cum_value, na.rm = TRUE),
          Min = min(cum_value, na.rm = TRUE),
          Q90 = quantile(cum_value, 0.90, na.rm = TRUE),
          Q75 = quantile(cum_value, 0.75, na.rm = TRUE),
          Q50 = quantile(cum_value, 0.50, na.rm = TRUE),
          Q25 = quantile(cum_value, 0.25, na.rm = TRUE),
          Q10 = quantile(cum_value, 0.10, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        dplyr::mutate(
          Date_plot = origin_date + (DayofYear - 1L)
        )
    
    # --- 3. SELECT YEARS TO PLOT & MAP THEM TO THE SAME REFERENCE YEAR ---
      data_plot <- data_full %>%
        dplyr::filter(WaterYear %in% select_years) %>%
        dplyr::mutate(
          Date_plot = origin_date + (DayofYear - 1)
        )
    
      water_start <- origin_date
      water_end   <- origin_date + max(clim_stats$DayofYear, na.rm = TRUE) - 1
    
    plot <- ggplot2::ggplot() +
      # historical ribbons from clim_stats
      ggplot2::geom_ribbon(
        data = clim_stats,
        ggplot2::aes(x = Date_plot, ymin = Min, ymax = Max, fill = "Min - Max")
      ) +
      ggplot2::geom_ribbon(
        data = clim_stats,
        ggplot2::aes(x = Date_plot, ymin = Q10, ymax = Q90, fill = "10th - 90th Percentile")
      ) +
      ggplot2::geom_ribbon(
        data = clim_stats,
        ggplot2::aes(x = Date_plot, ymin = Q25, ymax = Q75, fill = "Average Range")
      ) +
      ggplot2::scale_fill_manual(
        name   = "",
        breaks = c("Average Range", "10th - 90th Percentile", "Min - Max"),
        values = c("gray75", "gray85", "gray95"),
        guide  = ggplot2::guide_legend(order = 1)   # <- fill legend first
      ) +
      ggplot2::geom_line(
        data = data_plot,
        ggplot2::aes(x = Date_plot, y = cum_value, colour = factor(WaterYear)),
        linewidth = line_size
      ) +
      ggplot2::geom_point(
        data = data_plot,
        ggplot2::aes(x = Date_plot, y = cum_value, colour = factor(WaterYear)),
        shape = 19,
        size  = point_size
      ) +
      ggplot2::scale_colour_manual(
        name   = "",                      # <- removes "Year"
        values = point_colour,
        guide  = ggplot2::guide_legend(order = 2)
      ) +
      ggplot2::theme_classic() +
      ggplot2::labs(
        title = paste(site,
                      plot_title,
                      sep = " "),
        x = "Month",
        y = y_axis_title
      ) +
      ggplot2::scale_x_date(
        date_labels = "%b",
        date_breaks = "1 month",
        expand = c(0.05, 0.05),
        limits = c(water_start, water_end)
      ) +
      ggplot2::theme(legend.position = "top")
    
    
    # --- 5. SAVE PLOT ---
    ggplot2::ggsave(
      paste(site,
            paste(select_years, collapse = "-"),
            plot_title,
            ".png",
            sep = "_"),
      plot   = plot,
      device = "png",
      path   = ifelse(exists("save_path"), save_path, getwd()),
      scale  = 1,
      width  = plot_width,
      height = plot_height,
      units  = "in",
      dpi    = 300
    )
    
    plot
    
    # --- 6. SUMMARY STATS PER YEAR (OPTIONAL) ---
    cumulative_precip_mm <- data_plot %>%
      dplyr::group_by(WaterYear) %>%
      dplyr::summarise(
        cum_precip = max(cum_value, na.rm = TRUE),
        norm       = max(clim_stats$Q50, na.rm = TRUE),
        percent_of_norm = round(cum_precip / norm * 100, 2),
        .groups = "drop"
      )
    }
  
  
    if(cumulative == TRUE && water_year == FALSE){
      
      data <- data %>%
        dplyr::filter(Date <= today)
      
      data_full <- data %>%
        dplyr::group_by(CalendarYear) %>%
        dplyr::mutate(                    
          cum_value = cumsum(tidyr::replace_na(Value, 0)),
          dayofyear = lubridate::yday(Date)
        ) %>%
        dplyr::ungroup()
    
      data_stats <- data_full %>%
        dplyr::group_by(CalendarYear) %>%
        dplyr::mutate(
          Missing = sum(Count),
          NonNA   = sum(!is.na(Value))
        ) %>%
        dplyr::ungroup() %>%
        dplyr::filter(
          CalendarYear >= start_year,
          CalendarYear <= end_year,    # only past years for normals
          Missing <= 10,
          NonNA   >= 350
        )
      
      data_plot <- data_full %>%
        dplyr::filter(CalendarYear %in% select_years) %>%
        dplyr::filter(
          (CalendarYear < this_yr) |
            (CalendarYear == this_yr & dayofyear <= today_jd)
        ) %>%
        dplyr::mutate(
          Date_plot = as.Date(dayofyear - 1L, origin = paste0(ref_year, "-01-01"))
        )
    
    #creating historical data IQR, 10-90th percentiles and min/max
    clim_stats <- data_stats %>%
      dplyr::group_by(dayofyear) %>%
      dplyr::summarise(
        Max = max(cum_value, na.rm = TRUE),
        Min = min(cum_value, na.rm = TRUE),
        Q90 = quantile(cum_value, 0.90, na.rm = TRUE),
        Q75 = quantile(cum_value, 0.75, na.rm = TRUE),
        Q50 = quantile(cum_value, 0.50, na.rm = TRUE),
        Q25 = quantile(cum_value, 0.25, na.rm = TRUE),
        Q10 = quantile(cum_value, 0.10, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      dplyr::mutate(
        Date_plot = as.Date(dayofyear - 1, origin = paste0(ref_year, "-01-01"))
      )
    
    # --- 3. SELECT YEARS TO PLOT & MAP THEM TO THE SAME REFERENCE YEAR ---
    data_plot <- data_full %>%
      dplyr::filter(CalendarYear %in% select_years) %>%
      dplyr::filter(
        (CalendarYear < this_yr) |                      # past years: full
          (CalendarYear == this_yr & dayofyear <= today_jd)  # current year: up to today
      ) %>%
      dplyr::mutate(
        Date_plot = as.Date(dayofyear - 1, origin = paste0(ref_year, "-01-01"))
      )
    
    plot <- ggplot2::ggplot() +
      # historical ribbons from clim_stats
      ggplot2::geom_ribbon(
        data = clim_stats,
        ggplot2::aes(x = Date_plot, ymin = Min, ymax = Max, fill = "Min - Max")
      ) +
      ggplot2::geom_ribbon(
        data = clim_stats,
        ggplot2::aes(x = Date_plot, ymin = Q10, ymax = Q90, fill = "10th - 90th Percentile")
      ) +
      ggplot2::geom_ribbon(
        data = clim_stats,
        ggplot2::aes(x = Date_plot, ymin = Q25, ymax = Q75, fill = "Average Range")
      ) +
      ggplot2::scale_fill_manual(
        name   = "",
        breaks = c("Average Range", "10th - 90th Percentile", "Min - Max"),
        values = c("gray75", "gray85", "gray95"),
        guide  = ggplot2::guide_legend(order = 1)   # <- fill legend first
      ) +
      ggplot2::geom_line(
        data = data_plot,
        ggplot2::aes(x = Date_plot, y = cum_value, colour = factor(CalendarYear)),
        linewidth = line_size
      ) +
      ggplot2::geom_point(
        data = data_plot,
        ggplot2::aes(x = Date_plot, y = cum_value, colour = factor(CalendarYear)),
        shape = 19,
        size  = point_size
      ) +
      ggplot2::scale_colour_manual(
        name   = "",                      # <- removes "Year"
        values = point_colour,
        guide  = ggplot2::guide_legend(order = 2)
      ) +
      ggplot2::theme_classic() +
      ggplot2::labs(
        title = paste(site,
                      plot_title,
                      sep = " "),
        x = "Month",
        y = y_axis_title
      ) +
      ggplot2::scale_x_date(
        date_labels = "%b",
        date_breaks = "1 month",
        expand = c(0.05, 0.05)
      ) +
      ggplot2::theme(legend.position = "top")
    
    
    # --- 5. SAVE PLOT ---
    ggplot2::ggsave(
      paste(site,
            paste(select_years, collapse = "-"),
            plot_title,
            ".png",
            sep = "_"),
      plot   = plot,
      device = "png",
      path   = ifelse(exists("save_path"), save_path, getwd()),
      scale  = 1,
      width  = plot_width,
      height = plot_height,
      units  = "in",
      dpi    = 300
    )
    
    plot
    
    # --- 6. SUMMARY STATS PER YEAR (OPTIONAL) ---
    cumulative_precip_mm <- data_plot %>%
      dplyr::group_by(CalendarYear) %>%
      dplyr::summarise(
        cum_precip = max(cum_value, na.rm = TRUE),
        norm       = max(clim_stats$Q50, na.rm = TRUE),
        percent_of_norm = round(cum_precip / norm * 100, 2),
        .groups = "drop"
      )
    } 
  
  
    if(cumulative == FALSE && water_year == FALSE){  
      
    data <- data %>%
      dplyr::filter(Date <= today) %>%
      dplyr::mutate(dayofyear = lubridate::yday(Date)) # Add julian day column
    
    clim_stats <- data %>%
      dplyr::group_by(dayofyear) %>% # Group by julian day
      dplyr::mutate(prctile = (ecdf(Value)(Value))*100,
                    Max = max(Value, na.rm = TRUE),
                    Min = min(Value, na.rm = TRUE),
                    Q90 = quantile(Value, 0.90, na.rm = T),
                    Q75 = quantile(Value, 0.75, na.rm = TRUE),
                    Q25 = quantile(Value, 0.25, na.rm = TRUE),
                    Q10 = quantile(Value, 0.10, na.rm = T),
                    .groups = "drop"
      ) %>%
      # Create a plotting Date in a single reference year so the x-axis is Jan–Dec once
      dplyr::mutate(
        Date_plot = as.Date(dayofyear - 1, origin = paste0(ref_year, "-01-01"))
      )
    
    # --- 3. SELECT YEARS TO PLOT & MAP THEM TO THE SAME REFERENCE YEAR ---
    data_plot <- data %>%
      dplyr::filter(CalendarYear %in% select_years) %>%
      dplyr::mutate(
        Date_plot = as.Date(dayofyear - 1, origin = paste0(ref_year, "-01-01"))
      )
    
    # --- 4. PLOT ---
    plot <- ggplot2::ggplot() +
      # historical ribbons from clim_stats
      ggplot2::geom_ribbon(
        data = clim_stats,
        ggplot2::aes(x = Date_plot, ymin = Min, ymax = Max, fill = "Min - Max")
      ) +
      ggplot2::geom_ribbon(
        data = clim_stats,
        ggplot2::aes(x = Date_plot, ymin = Q10, ymax = Q90, fill = "10th - 90th Percentile")
      ) +
      ggplot2::geom_ribbon(
        data = clim_stats,
        ggplot2::aes(x = Date_plot, ymin = Q25, ymax = Q75, fill = "Average Range")
      ) +
      ggplot2::scale_fill_manual(
        name   = "",
        breaks = c("Average Range", "10th - 90th Percentile", "Min - Max"),
        values = c("gray75", "gray85", "gray95"),
        guide  = ggplot2::guide_legend(order = 1)   # <- fill legend first
      ) +
      ggplot2::geom_line(
        data = data_plot,
        ggplot2::aes(x = Date_plot, y = Value, colour = factor(CalendarYear)),
        linewidth = line_size
      ) +
      ggplot2::geom_point(
        data = data_plot,
        ggplot2::aes(x = Date_plot, y = Value, colour = factor(CalendarYear)),
        shape = 19,
        size  = point_size
      ) +
      ggplot2::scale_colour_manual(
        name   = "",                      # <- removes "Year"
        values = point_colour,
        guide  = ggplot2::guide_legend(order = 2)
      ) +
      ggplot2::theme_classic() +
      ggplot2::labs(
        title = paste(site,
                      plot_title,
                      sep = " "),
        x = "Month",
        y = y_axis_title
      ) +
      ggplot2::scale_x_date(
        date_labels = "%b",
        date_breaks = "1 month",
        expand = c(0.05, 0.05)
      ) +
      ggplot2::theme(legend.position = "top")
    
    
    # --- 5. SAVE PLOT ---
    ggplot2::ggsave(
      paste(site,
            paste(select_years, collapse = "-"),
            plot_title,
            ".png",
            sep = "_"),
      plot   = plot,
      device = "png",
      path   = ifelse(exists("save_path"), save_path, getwd()),
      scale  = 1,
      width  = plot_width,
      height = plot_height,
      units  = "in",
      dpi    = 300
    )
    
    }

     
  
  plot
  
}



