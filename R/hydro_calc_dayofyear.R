#' hydro_calc_dayofyear
#'
#' Calculates daily values of hydrometric data, along with summary stats for each day of year
#' @return A tibble of the daily values
#' @export

# Function to plot hydrometric data

hydro_calc_dayofyear <- function(
  station_number,
  parameter,
  select_years = lubridate::year(Sys.Date()),
  water_year_start = 1,
  after_bennett = FALSE,
  historic_min = NA,
  historic_max = NA
  )
  
{
  
  
  if(lubridate::year(Sys.Date() - 548) > max(select_years)) {
    realtime_dl = F
  } else {
    realtime_dl = T
  }
  
  data <- hydro_calc_daily(
    station_number = station_number,
    parameter = parameter,
    realtime_dl = realtime_dl
  )

  # Filter for selected years
  
  if(after_bennett == T) {
    historic_min = 1972
  }
  
  stat_data <- dplyr::filter(data, 
                             WaterYear >= ifelse(is.na(historic_min),
                                                            min(WaterYear),
                                                            historic_min) &
                               WaterYear <= ifelse(is.na(historic_max),
                                                   max(WaterYear),
                                                   historic_max))
  
  # Calculate annual statistics
  stat_data <- dplyr::reframe(dplyr::group_by(stat_data, STATION_NUMBER, DayofYear),
                              Max = max(Value, na.rm = T),
                              Min = min(Value, na.rm = T),
                              Median = median(Value, na.rm = T),
                              Mean = mean(Value, na.rm = T),
                              P95 = quantile(Value, 0.95, na.rm = T),
                              P90 = quantile(Value, 0.90, na.rm = T),
                              P75 = quantile(Value, 0.75, na.rm = T),
                              P50 = quantile(Value, 0.50, na.rm = T),
                              P25 = quantile(Value, 0.25, na.rm = T),
                              P10 = quantile(Value, 0.10, na.rm = T),
                              P05 = quantile(Value, 0.05, na.rm = T))  
  
  # Filter analysis_data to select_years argument
  
  select_analysis_data <- dplyr::filter(data, WaterYear %in% select_years)

  daily_stats <- base::merge(select_analysis_data, stat_data, by = c("DayofYear", "STATION_NUMBER"))
  daily_stats <- dplyr::arrange(daily_stats, Date, STATION_NUMBER, DayofYear)
  daily_stats <- dplyr::rename(daily_stats, Year = WaterYear)
  daily_stats <- dplyr::select(daily_stats, -c("CalendarYear", "Month", "MonthName"))
  
  dplyr::as_tibble(daily_stats)
  
}
  

  

