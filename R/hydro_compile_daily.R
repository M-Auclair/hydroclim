#' hydro_compile_daily
#'
#' Returns daily values of selected hydrometric data
#' @return A tibble of the daily values
#' @export

# Function to export daily hydrometric data

hydro_compile_daily <- function(
    station_number,
    parameter,
    start_date = "1900-01-01",
    end_date = Sys.Date(),
    timezone = "America/Edmonton",
    realtime_dl = T
    )
  
{
  
  # Define variables
  
  parameter <- hydro_parameter(parameter = parameter)[[1]]
  parameter.num <- hydro_parameter(parameter = parameter)[[2]]
  
  # This function will create two dataframes: 
  # 1) a historic dataframe (accessed from tidyhydat)
  # 2) a realtime dataframe (accessed from tidyhydat.ws)
  
  end.data.final <- tidyhydat::hy_daily(station_number = station_number)
  end.data.final <- dplyr::filter(end.data.final, Parameter == parameter)
  end.data.final <- as.Date(max(end.data.final$Date))
    
  if(start_date <= end.data.final) {
    data.final <- tidyhydat::hy_daily(station_number = station_number, 
                                      start_date = start_date, 
                                      end_date = end_date) 
    data.final <- dplyr::filter(data.final, Parameter == parameter)
    data.final <- dplyr::mutate(data.final, Data_Type = "Final")
  } else {
    data.final <- data.frame()
  }
  
  # Download real time data if needed
  
  if(end_date > max(data.final$Date) & realtime_dl == T) {
    
    suppressWarnings(
      data.realtime <- tidyhydat.ws::realtime_ws(
        station_number = station_number,
        parameters = as.numeric(parameter.num),
        start_date = max(data.final$Date) + 1,
        end_date = end_date,
        token = tidyhydat.ws::token_ws())
    )
    
    # Convert UTC to timezone (default is system timezone)
    data.realtime <- lubridate::with_tz(data.realtime, tzone = paste(timezone))
    data.realtime <- dplyr::rename(data.realtime, "Date_Time" = "Date")
    
    # Aggregate data as daily means and identify that data are provisional
    data.realtime <- dplyr::mutate(data.realtime, Date = as.Date(Date_Time))
    data.realtime <- dplyr::reframe(dplyr::group_by(data.realtime, Date),
                                    Value = mean(Value))
    data.realtime <- dplyr::mutate(data.realtime, 
                                   STATION_NUMBER = station_number,
                                   Parameter = parameter,
                                   Symbol = NA,
                                   Data_Type = "Provisional")
    
    data <- dplyr::bind_rows(data.final, data.realtime)
    
  } else {
    data <- data.final
  }
  
  dplyr::as_tibble(data)
  
}
