#' clim_calc_daily
#'
#' Returns daily values of selected climate data in a tibble suitable to calculate monthly and annual summary data
#' @return A tibble of daily climate data
#' @export

# Function to produce daily climate data to be summarized

clim_calc_daily <- function(
    site,
    parameter,
    start_year,
    end_year,
    select_year,
    water_year_start
)
  
{
 
   # Define variables
  parameter <- clim_parameter(parameter = parameter)[[1]]
  param_operator <- clim_parameter(parameter = parameter)[[5]]
  
  # Create blank summary data frame
  summary_data <- data.frame()

  
  if("all" %in% site ){

    # Run a for loop to calculate data for each site and bind rows together
    for(i in site) {
      data <- clim_compile_daily(
        site = i,
        parameter = parameter,
        start_year = start_year,
        end_year = end_year,
        select_year = select_year
      )
      
      analysis_data <- analysis_prep(data = data, 
                                     water_year_start = water_year_start)
      analysis_data <- dplyr::select(analysis_data, -year)
      analysis_data <- dplyr::mutate(analysis_data, Parameter = parameter)
      
      # Infill Site NA values with Site name
      analysis_data <- dplyr::filter(analysis_data, lubridate::year(Date) >= start_year)
      #analysis_data$Site <- i
      
      # Code in missing days with 0 or 1
      analysis_data <- dplyr::mutate(analysis_data, Count = ifelse(is.na(Value), 1, 0))
      
      # Write to the summary_data dataframe
      summary_data <- dplyr::bind_rows(summary_data, analysis_data)
      
    }
   
    
    dplyr::as_tibble(summary_data)
    

    
  } else{
  
  
  
  # Run a for loop to calculate data for each site and bind rows together
    for(i in site) {
      data <- clim_compile_daily(
        site = i,
        parameter = parameter,
        start_year = start_year,
        end_year = end_year,
        select_year = select_year
      )
      
      analysis_data <- analysis_prep(data = data, 
                                     water_year_start = water_year_start)
      analysis_data <- dplyr::select(analysis_data, -year)
      analysis_data <- dplyr::mutate(analysis_data, Parameter = parameter)
      
      # Infill Site NA values with Site name
      analysis_data <- dplyr::filter(analysis_data, lubridate::year(Date) >= start_year)
      analysis_data$Site <- i
      
      # Code in missing days with 0 or 1
      analysis_data <- dplyr::mutate(analysis_data, Count = ifelse(is.na(Value), 1, 0))
      
      # Write to the summary_data dataframe
      summary_data <- dplyr::bind_rows(summary_data, analysis_data)

    }
   

    dplyr::as_tibble(summary_data)

  
}
}
