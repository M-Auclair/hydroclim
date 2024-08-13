#' clim_calc_annual
#' Function to export monthly climate data
#'
#' @param site A character vector of site names
#' @param parameter The input parameter
#' @param start_year The start year
#' @param end_year The end year
#' @param select_year The year selected to be examined
#' @param water_year_start The month number indicating the start of the year

#' Returns annual values of selected climate data?
#' @return A tibble of annual climate data
#' @export


clim_calc_annual <- function(
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

  if("all" %in% site ) {
    # Run a for loop to calculate data for each site and bind rows together
    for(i in site) {
      summary_data <- clim_calc_daily(
        site = i,
        parameter = parameter,
        start_year = start_year,
        end_year = end_year,
        select_year = select_year,
        water_year_start = water_year_start
      )

    summary_data <- dplyr::reframe(dplyr::group_by(summary_data, Site, Parameter, WaterYear),
                                     Value = param_operator(Value, na.rm = T),
                                     MissingDays = sum(Count))
    summary_data <- dplyr::rename(summary_data, Year = WaterYear)

    }

    dplyr::as_tibble(summary_data)


    }else{


  # Gather data
  data <- clim_calc_daily(
    site = site,
    parameter = parameter,
    start_year = start_year,
    end_year = end_year,
    select_year = select_year,
    water_year_start = water_year_start
  )

  # Summarize data
  summary_data <- dplyr::reframe(dplyr::group_by(data, Site, Parameter, WaterYear),
                                 Value = param_operator(Value, na.rm = T),
                                 MissingDays = sum(Count))
  summary_data <- dplyr::rename(summary_data, Year = WaterYear)
    }

  dplyr::as_tibble(summary_data)



}


