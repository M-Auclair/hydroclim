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
    select_years,
    rain_cutoff = 0,
    months = c(1:12)
)

{

  # Import data locally
  data <- readRDS(paste0(data_path, merged_data, ".rds")) # Ryan's OG code - for ECCC data
  #data <- readRDS(paste0(data_path, merged_data_clean, ".rds")) # MA edits - for all data

  if ("all" %in% site  | "All" %in% site  ) {

    data <- readRDS(paste0(data_path, merged_data, ".rds")) # MA edits - for all data

    # Define variables
    parameter <- clim_parameter(parameter = parameter)[[1]]

    # Build year vector (supports multiple select_years)
    base_years <- start_year:end_year

    if (!all(is.na(select_years))) {
      years <- sort(unique(c(base_years, select_years)))
    } else {
      years <- base_years
    }

    # Add SWE or rain column if necessary
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

  data <- import_site_data(site, data_path)

  # Define variables
  parameter <- clim_parameter(parameter = parameter)[[1]]

  # Create vector of years
  base_years <- start_year:end_year

  if (!all(is.na(select_years))) {
    years <- sort(unique(c(base_years, select_years)))
  } else {
    years <- base_years
  }

  # Add SWE or rain column if necessary
  if(parameter == "SWE_mm") {
    data <- dplyr::mutate(data, SWE_mm = ifelse(t_air >= 0, 0, total_precip))
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

