#' hydro_calc_daily
#'
#' Calculates daily values of hydrometric data, along with summary stats for each day of year
#' @return A tibble of the daily values
#' @export

# Function to plot hydrometric data

hydro_calc_daily <- function(
  station_number,
  parameter,
  water_year_start = 1,
  start_date = "1900-01-01",
  end_date = Sys.Date(),
  timezone = "America/Edmonton", 
  realtime_dl = T
  )
  
{
  
  data <- hydro_compile_daily(
    station_number = station_number,
    parameter = parameter, 
    start_date = start_date,
    end_date = end_date,
    timezone = timezone, 
    realtime_dl = realtime_dl
  )

  analysis_data <- analysis_prep(data = data, 
                             water_year_start = water_year_start)
  
  # Remove Feb. 29 data
  analysis_data <- analysis_data[!(format(analysis_data$Date,"%m") == "02" & format(analysis_data$Date, "%d") == "29"), , drop = FALSE]
  
  # Change DayofYear column to account for no Feb. 29 data
  analysis_data <- dplyr::mutate(dplyr::group_by(analysis_data, WaterYear),
                             DayofYear = c(1:365))
  
  # Stop if all data is NA
  no_values_error(analysis_data$Value)
  
  dplyr::as_tibble(analysis_data)
  
}
  

  

