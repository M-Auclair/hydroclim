#' clim_calc_monthly
#'
#' Returns monthly values of selected climate data
#' @return A tibble of monthly climate data
#' @export

# Function to export monthly climate data

clim_calc_monthly <- function(
    site,
    parameter,
    start_year,
    end_year,
    select_year = NA,
    water_year_start = 1
)


  
  
{

    # Define variables
  parameter <- clim_parameter(parameter = parameter)[[1]]
  param_operator <- clim_parameter(parameter = parameter)[[5]]

  if("all" %in% site ) {
    # Run a for loop to calculate data for each site 
    for(i in site) {
      summary_data <- clim_calc_daily(
        site = i,
        parameter = parameter,
        start_year = start_year,
        end_year = end_year,
        select_year = select_year,
        water_year_start = water_year_start
      )
      # Changed by MA, March 2024 - Ensure there is only one lat/lon per site (took from mapping fun)
      data_adj <- NULL
      data_parse <- summary_data %>%
        dplyr::group_by(Site) %>%
        dplyr:: mutate(lat = tail(lat, n = 1),
                       lon = tail(lon, n = 1))
      summary_data <- dplyr::bind_rows(data_adj, data_parse)
      
      # Summarize data
      summary_data <- dplyr::reframe(dplyr::group_by(summary_data, Site, lat, lon, Parameter, WaterYear, Month, MonthName),
                                     Value = param_operator(Value, na.rm = T),
                                     MissingDays = sum(Count))
      summary_data <- dplyr::rename(summary_data, Year = WaterYear)
      
    }
     
    
    dplyr::as_tibble(summary_data)
    
    
  } else{
  
  
  # Gather data 
  summary_data <- clim_calc_daily(
    site = site,
    parameter = parameter,
    start_year = start_year,
    end_year = end_year,
    select_year = select_year,
    water_year_start = water_year_start
  )
  
  # Summarize data
  summary_data <- dplyr::reframe(dplyr::group_by(summary_data, Site, Parameter, WaterYear, Month, MonthName),
                                 Value = param_operator(Value, na.rm = T),
                                 MissingDays = sum(Count))
  summary_data <- dplyr::rename(summary_data, Year = WaterYear)
  
  }
  
  
  dplyr::as_tibble(summary_data)
  
  
  
  
}

