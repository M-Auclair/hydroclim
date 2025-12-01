#' clim_calc_daily
#' Function to produce daily climate data to be summarized
#' @param site A character vector of site names
#' @param parameter The input parameter
#' @param start_year The start year
#' @param end_year The end year
#' @param select_years The year selected to be examined
#' @param water_year_start The month number indicating the start of the year
#' Returns daily values of selected climate data in a tibble suitable to calculate monthly and annual summary data
#' @return A tibble of daily climate data
#' @export



clim_calc_daily <- function(
    site,
    parameter,
    start_year,
    end_year,
    select_years,
    water_year_start,
    months = c(1:12)
)

{

   # Define variables
  parameter <- clim_parameter(parameter = parameter)[[1]]
  param_operator <- clim_parameter(parameter = parameter)[[5]]

  # Create blank summary data frame
  summary_data <- data.frame()

  ## IMPORTANT: make sure we load enough calendar years to cover all select_years
  ## including the preceding year if using a water year that does not start in January.
  if (!all(is.na(select_years))) {
    max_sel <- max(select_years, na.rm = TRUE)

    # if water year starts in January, we just need that year
    # if it starts later (e.g. Oct), we also need the previous calendar year
    extra_year <- ifelse(water_year_start == 1, 0, 1)

    compile_end_year <- max(end_year, max_sel + extra_year)
  } else {
    compile_end_year <- end_year
  }

  if("all" %in% site ){

    # Run a for loop to calculate data for each site and bind rows together
    for(i in site) {
      data <- clim_compile_daily(
        site = i,
        parameter = parameter,
        start_year = start_year,
        end_year = compile_end_year,
        select_years = select_years
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
        end_year = compile_end_year,
        select_years = select_years,
        months = months
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
