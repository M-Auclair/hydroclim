#' clim_compile_daily
#'
#' Returns daily values of selected climate data
#' @return A tibble of daily climate data
#' @export

clim_compile_daily <- function(
    site,
    parameter,
    start_year,
    end_year,
    select_year,
    rain_cutoff = 0
)



  
{

  # Import data locally
  #data <- readRDS(paste0(data_path, merged_data, ".rds")) # Ryan's OG code - for ECCC data
  #data <- readRDS(paste0(data_path, merged_data_clean, ".rds")) # MA edits - for all data
  
  
  ## MA Added site = all option (for map function) April 2024

  if ("all" %in% site  | "All" %in% site  ) {
    
    data <- readRDS(paste0(data_path, merged_data_clean, ".rds")) # MA edits - for all data
    
    # Define variables
    parameter <- clim_parameter(parameter = parameter)[[1]]
    
    # Create vector of years 
    if(!is.na(select_year) & !select_year %in% c(start_year:end_year)) {
      years <- as.numeric(c(start_year:end_year, select_year))
    } else {
      years <- c(start_year:end_year)
    }
    
    # Add SWE or rain column if necessary
    # MA changed from "mean_temp" to "t_air" Feb 13, 2024 to resolve error message
    if(parameter == "SWE") {
      data <- dplyr::mutate(data, SWE = ifelse(t_air >= rain_cutoff, 0, total_precip))
    } else if(parameter == "rain") {
      data <- dplyr::mutate(data, rain = ifelse(t_air < rain_cutoff, 0, total_precip))
    }
    
    # Filter to parameter 
    data <- dplyr::select(data, date, year, merged_name, lat, lon, all_of(parameter[[1]]))
    data <- dplyr::filter(data, year %in% years)
    
    # Change column names
    data <- dplyr::rename(data, Date = date, Site = merged_name, Value = parameter[[1]])
    
    dplyr::as_tibble(data)
    
    
  } else {
  
  
  
  data <- import_site_files(site, data_path) # MA edits new - to read in data based on sites
  
  # Define variables
  parameter <- clim_parameter(parameter = parameter)[[1]]
  
  # Create vector of years 
  if(!is.na(select_year) & !select_year %in% c(start_year:end_year)) {
    years <- as.numeric(c(start_year:end_year, select_year))
  } else {
    years <- c(start_year:end_year)
  }
  
  # Add SWE or rain column if necessary
  # MA changed from "mean_temp" to "t_air" Feb 13, 2024 to resolve error message
  if(parameter == "SWE") {
    data <- dplyr::mutate(data, SWE = ifelse(t_air >= 0, 0, total_precip))
  } else if(parameter == "rain") {
    data <- dplyr::mutate(data, rain = ifelse(t_air < 0, 0, total_precip))
  }
  
  # Filter to parameter and site
  data <- dplyr::select(data, date, year, merged_name, lat, lon, all_of(parameter[[1]]))
  data <- dplyr::filter(data, year %in% years, merged_name %in% site)
  
  # Change column names
  data <- dplyr::rename(data, Date = date, Site = merged_name, Value = parameter[[1]])
  
  dplyr::as_tibble(data)
  
}
}

