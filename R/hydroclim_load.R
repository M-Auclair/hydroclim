##############################################################################

# Defines parameter based on multiple potential user inputs

hydro_parameter <- function(parameter) 
  
{

if(grepl(paste0("(?i)", parameter), "Flows") == T | 
   grepl(paste0("(?i)", parameter), "Discharge") == T |
   grepl(paste0("(?i)", parameter), "Q") == T) {
  
  parameter <- "Flow"
  parameter_num <- 47
  y_axis_title <- expression(paste("Discharge (m"^3, " s"^-1,")"))
  
} else if(grepl(paste0("(?i)", parameter), "Water _ Levels") == T) {
  
  parameter <- "Level"
  parameter_num <- 46
  y_axis_title <- "Water Level (m)"
  
} else {
  
  stop("Parameter selection is invalid. Input must be 'Flows' or 'Levels'")
  
}
  as.list(c(parameter, parameter_num, y_axis_title))
  
}

##############################################################################


clim_parameter <- function(parameter) {
  
  
  if(grepl(paste0("(?i)", gsub("[ _]", "", parameter)), gsub("[ _]", "", "precipitation")) == T |
     grepl(paste0("(?i)", gsub("[ _]", "", parameter)), gsub("[ _]", "", "total_precip")) == T) {
    parameter <- "total_precip"
    plot_title <- "Total Precipitation"
    y_axis_title <- "Precipitation (mm)"
    point_colour <- "blue4"
    parameter_operator <- getFunction("sum")
    
  } else if (grepl(paste0("(?i)", gsub("[ _]", "", parameter)), gsub("[ _]", "", "rainfall")) == T |
             grepl(paste0("(?i)", gsub("[ _]", "", parameter)), gsub("[ _]", "", "rain")) == T |
             grepl(paste0("(?i)", gsub("[ _]", "", parameter)), gsub("[ _]", "", "total_rain")) == T) {
    parameter <- "rain"
    plot_title <- "Total Rain"
    y_axis_title <- "Rain (mm)"
    point_colour <- "blue4"
    parameter_operator <- getFunction("sum")
    
  } else if (grepl(paste0("(?i)", gsub("[ _]", "", parameter)), gsub("[ _]", "", "snowfall")) == T |
             grepl(paste0("(?i)", gsub("[ _]", "", parameter)), gsub("[ _]", "", "snow depth")) == T |
             grepl(paste0("(?i)", gsub("[ _]", "", parameter)), gsub("[ _]", "", "total_snow")) == T) {
    parameter <- "total_snow"
    plot_title <- "Total Snowfall"
    y_axis_title <- "Snowfall (cm)"
    point_colour <- "blue4"
    parameter_operator <- getFunction("sum")
    
  } else if (grepl(paste0("(?i)", gsub("[ _]", "", parameter)), gsub("[ _]", "", "swe")) == T |
             grepl(paste0("(?i)", gsub("[ _]", "", parameter)), gsub("[ _]", "", "swe_mm")) == T |
             grepl(paste0("(?i)", gsub("[ _]", "", parameter)), gsub("[ _]", "", "snow water equivalent")) == T) {
    parameter <- "SWE_mm"
    plot_title <- "Total Snow Water Equivalent (SWE)"
    y_axis_title <- "SWE (mm)"
    point_colour <- "blue4"
    parameter_operator <- getFunction("sum")
    
  } else if (grepl(paste0("(?i)", gsub("[ _]", "", parameter)), gsub("[ _]", "", "mean temperature")) == T |
             grepl(paste0("(?i)", parameter), "T_air") == T |
             grepl(paste0("(?i)", gsub("[ _]", "", parameter)), gsub("[ _]", "", "mean air temperature")) == T |
             grepl(paste0("(?i)", gsub("[ _]", "", parameter)), gsub("[ _]", "", "mean_temp")) == T) {
    parameter <- "t_air" # MA changed from "mean_temp" to resolve an error - Feb 13, 2024
    plot_title <- "Mean Air Temperatures"
    y_axis_title <- "Mean Temperature (\u00B0C)"
    point_colour <- "red"
    parameter_operator <- getFunction("mean")
    
  } else if (grepl(paste0("(?i)", gsub("[ _]", "", parameter)), gsub("[ _]", "", "minimum temperature")) == T |
             grepl(paste0("(?i)", gsub("[ _]", "", parameter)), gsub("[ _]", "", "min_temp")) == T){
    parameter <- "min_temp"
    plot_title <- "Min Air Temperatures"
    y_axis_title <- "Mean Minimum Temperature (\u00B0C)"
    point_colour <- "red"
    parameter_operator <- getFunction("mean")
    
  } else if (grepl(paste0("(?i)", gsub("[ _]", "", parameter)), gsub("[ _]", "", "maximum temperature")) == T |
             grepl(paste0("(?i)", gsub("[ _]", "", parameter)), gsub("[ _]", "", "max_temp")) == T) {
    parameter <- "max_temp"
    plot_title <- "Max Air Temperatures"
    y_axis_title <- "Mean Maximum Temperature (\u00B0C)"
    point_colour <- "red"
    parameter_operator <- getFunction("mean")
    
  } else if (grepl(paste0("(?i)", gsub("[ _]", "", parameter)), gsub("[ _]", "", "mean relative humidity")) == T |
             grepl(paste0("(?i)", parameter), "RH") == T |
             grepl(paste0("(?i)", gsub("[ _]", "", parameter)), gsub("[ _]", "", "rel_humidity")) == T) {
    parameter <- "RH" 
    plot_title <- "Relative Humidity"
    y_axis_title <- "Relative Humidity (\u0025)"
    point_colour <- "darkgreen"
    parameter_operator <- getFunction("mean")
    
  } else if (grepl(paste0("(?i)", gsub("[ _]", "", parameter)), gsub("[ _]", "", "wind speed")) == T |
             grepl(paste0("(?i)", gsub("[ _]", "", parameter)), gsub("[ _]", "", "wnd_spd")) == T ){
    parameter <- "wind_sp" 
    plot_title <- "Wind Speed"
    y_axis_title <- "Wind Speed (m/s)"
    point_colour <- "darkgreen"
    parameter_operator <- getFunction("mean")
    
   
  } else if (grepl(paste0("(?i)", gsub("[ _]", "", parameter)), gsub("[ _]", "", "wind direction")) == T |
             grepl(paste0("(?i)", gsub("[ _]", "", parameter)), gsub("[ _]", "", "wnd_dir")) == T ){
    parameter <- "wind_dir" 
    plot_title <- "Wind Direction"
    y_axis_title <- "Wind Direction (m/s)"
    point_colour <- "darkgreen"
    parameter_operator <- getFunction("mean")
  
    
  } else if (grepl(paste0("(?i)", gsub("[ _]", "", parameter)), gsub("[ _]", "", "net shortwave radiation")) == T |
             grepl(paste0("(?i)", gsub("[ _]", "", parameter)), gsub("[ _]", "", "net_sw")) == T ){
    parameter <- "net_sw" 
    plot_title <- "Net Shortwave Radiation"
    y_axis_title <- "Net Shortwave Radiation"
    point_colour <- "darkgreen"
    parameter_operator <- getFunction("mean")
    
    
  } else if (grepl(paste0("(?i)", gsub("[ _]", "", parameter)), gsub("[ _]", "", "net longwave radiation")) == T |
            grepl(paste0("(?i)", gsub("[ _]", "", parameter)), gsub("[ _]", "", "net_lw")) == T ){
    parameter <- "net_sw" 
    plot_title <- "Net Longwave Radiation"
    y_axis_title <- "Net Longwave Radiation"
    point_colour <- "darkgreen"
    parameter_operator <- getFunction("mean")
    
    
  } else if (grepl(paste0("(?i)", gsub("[ _]", "", parameter)), gsub("[ _]", "", "net radiation")) == T |
             grepl(paste0("(?i)", gsub("[ _]", "", parameter)), gsub("[ _]", "", "rn")) == T |
             grepl(paste0("(?i)", gsub("[ _]", "", parameter)), gsub("[ _]", "", "net_rad")) == T ){
    parameter <- "rn" 
    plot_title <- "Net Radiation"
    y_axis_title <- "Net Radiation"
    point_colour <- "darkgreen"
    parameter_operator <- getFunction("mean")
    
    
  } else if (grepl(paste0("(?i)", gsub("[ _]", "", parameter)), gsub("[ _]", "", "incoming shortwave radiation")) == T |
            grepl(paste0("(?i)", gsub("[ _]", "", parameter)), gsub("[ _]", "", "sw_in")) == T ){
    parameter <- "sw_in" 
    plot_title <- "Incoming Shortwave Radiation"
    y_axis_title <- "Incoming Shortwave Radiation"
    point_colour <- "darkgreen"
    parameter_operator <- getFunction("mean")
    
    
  } else {
    
    stop("Parameter selection is invalid. Input must be 'Precipitation', 'Rain',
         'Snow', 'SWE', 'Mean Temp', 'Max Temp', 'Min Temp', 'RH', 'Wind Speed', or 'Wind Direction ")
    
  }
  
  as.list(c(parameter, plot_title, y_axis_title, point_colour, parameter_operator))
  
}

##############################################################################

data_check <- function(parameter) {
  if (is.null(station_number))
    stop("Must select one of station_number arguments to supply data.", 
         call. = FALSE)
}

##############################################################################

# analysis_prep

analysis_prep <- function (data, water_year_start, date = FALSE) 
{
  data <- fasstr::fill_missing_dates(data = data, water_year_start = water_year_start)
  data <- fasstr::add_date_variables(data = data, water_year_start = water_year_start)
  if (date) {
    if (water_year_start == 1) {
      data$AnalysisDate <- as.Date(data$DayofYear, origin = "1899-12-31")
    }
    else if (water_year_start == 2) {
      data$AnalysisDate <- as.Date(data$DayofYear, origin = "1899-01-31")
    }
    else if (water_year_start == 3) {
      data$AnalysisDate <- as.Date(data$DayofYear, origin = "1899-02-28")
    }
    else if (water_year_start == 4) {
      data$AnalysisDate <- as.Date(data$DayofYear, origin = "1899-03-31")
    }
    else if (water_year_start == 5) {
      data$AnalysisDate <- as.Date(data$DayofYear, origin = "1899-04-30")
    }
    else if (water_year_start == 6) {
      data$AnalysisDate <- as.Date(data$DayofYear, origin = "1899-05-31")
    }
    else if (water_year_start == 7) {
      data$AnalysisDate <- as.Date(data$DayofYear, origin = "1899-06-30")
    }
    else if (water_year_start == 8) {
      data$AnalysisDate <- as.Date(data$DayofYear, origin = "1899-07-31")
    }
    else if (water_year_start == 9) {
      data$AnalysisDate <- as.Date(data$DayofYear, origin = "1899-08-31")
    }
    else if (water_year_start == 10) {
      data$AnalysisDate <- as.Date(data$DayofYear, origin = "1899-09-30")
    }
    else if (water_year_start == 11) {
      data$AnalysisDate <- as.Date(data$DayofYear, origin = "1899-10-31")
    }
    else if (water_year_start == 12) {
      data$AnalysisDate <- as.Date(data$DayofYear, origin = "1899-11-30")
    }
  }
  data
}

##############################################################################

no_values_error <- function (values) 
{
  if (all(is.na(values))) 
    stop("All daily values are NA, select or filter data for years with data.", 
         call. = FALSE)
}

##############################################################################

exclude_bennett <- function (after_bennett) 
{
  if(after_bennett == T) {
    historic_min = 1972
  }
  historic_min
}

##############################################################################
# Function to round a value up to the nearest number 

roundUp <- function(
    x,
    to = 10)
{
  to*(x%/%to + as.logical(x%%to))
}

##############################################################################

# Define `%>%` operator in current environment

`%>%` <- magrittr::`%>%`

##############################################################################

# Manually call tidyhydat.ws into active library
library(tidyhydat.ws)

##############################################################################




