

# function and all code below
map_station_data<- function(parameter,
                            select_year, 
                            start_year,
                            months,
                            max_missing_days, 
                            max_monthly_missing_days,
                            max_missing_months,
                            percent_of_normal,
                            years_of_record,
                            min_record_length,
                            circle_radius,
                            water_year,
                            water_year_start,
                            filename)

# 
# parameter <- "rain" #
# select_year <- 2022
# start_year <- 1999
# months <- c(1:12)
# max_missing_days <- 10
# max_monthly_missing_days <- 3
# max_missing_months <- 1
# percent_of_normal <- TRUE
# years_of_record <- 5
# min_record_length <- 10 # of nearby sites to pull normals from
# circle_radius <- 10
# water_year <- FALSE
# water_year_start <- 10
# filename <- NA



{
  `%>%` <- magrittr::`%>%` 
  `%!in%` = Negate(`%in%`)
  savepath_climate <- "C:/Users/maucl/Documents/NT_Hydrology/Figures/"
  
  # if(exists("cd_rds") == F) {
  
  ## NOTE: Commented out April 2024
  # merged_data = "Climate_data_clean_merged.rds"
  sites = "sites_map.csv"
  directory = "C:/Users/maucl/Documents/R_Scripts/Packages/nwtclimate/data/"
  # cd_rds <- readRDS(paste0(directory, merged_data))
  sites <- read.csv(paste0(directory, sites))
  # }
  
  # Added by MA April 2024 - attempt to use Ryan's annual calc to read in data to reduce redundancies
  parameter <- clim_parameter(parameter = parameter)[[1]] 
  
  site <- "all"

  winter_months <- c(1:4, 10:12)
  this.year <- lubridate::year(Sys.Date())
  this.month <- lubridate::month(Sys.Date())
  this.day <- lubridate::day(Sys.Date())
  
 
  
  newdf <- clim_calc_monthly(
    site = site,
    parameter = parameter,
    start_year = start_year,
    end_year = this.year, # note - assigning end_year as current year 
    select_year = select_year,
    water_year_start = water_year_start
  )

## commented out temporarily 
  # if(select_year == this.year & (this.month < max(months)) & water_year == FALSE) {
  #   stop("No data available. You have selected the current year, but with months in the future")
  # }
  # 
  if(percent_of_normal == T & parameter == "mean_temp" |
     percent_of_normal == T & parameter == "t_air") {
    legend_numbers <-  c(-100, -2, -1.5, -1, -0.5, 0, 0.5, 1, 1.5, 2, 100)
    legend_labels <-  c("< -2 °C", "-1.5 °C", "-1 °C", 
                        "-0.5 °C", "0 °C", "+0.5°C", 
                        "+1°C", "+1.5°C", "+2 °C", "> 2°C")
  } else if(percent_of_normal == T) {
    legend_numbers <-  c(0, 50, 70, 90, 110, 130, 150, 200, 10000)
    legend_labels <-  c("< 50", "50 - 69", "70 - 89", "99 - 109", "110 - 129", 
                        "130 - 149", "150 - 199", "> 200")
  } else {
    legend_numbers <- c(0, 30, 50, 70, 90, 110, 130, 150, 10000)
    legend_labels <-  c("< 30", "30 - 49", "50 - 69", "70 - 89", "90 - 109", 
                        "110 - 129", "130 - 149", "> 150")
  }
  
  
  # Changed by MA, March 2024 - Ensure there is only one lat/lon per site
  
  data_adj <- NULL
  data_parse <- newdf %>%
    dplyr::group_by(Site) %>%
    dplyr::filter(Month %in% months) 
  data_adj <- dplyr::bind_rows(data_adj, data_parse)
  
  # filter lat
  data <- data_adj %>%
    dplyr::filter(lat < 69.5)

  # summarize monthly values & ensure max monthly missing days meet requirement, else NA
  param_operator <- clim_parameter(parameter = parameter)[[5]] 

  ## changed df_summary_monthly - assign NA to Value if MissingDays < max_monthly_missing_days
  df_summary_monthly <- data %>%
    dplyr::group_by(Site, Year, Month) %>%
    dplyr::reframe(Value = ifelse(MissingDays > max_monthly_missing_days, NA, Value),
                   Site=Site, lat=lat, lon=lon, Parameter=Parameter, Year=Year, Month=Month,
                   MonthName=MonthName, MissingDays=MissingDays) 

  df_summary <- df_summary_monthly %>%
    dplyr::group_by(Site) %>%
    dplyr::reframe(record_length = dplyr::n_distinct(Year[!is.na(Value)]),
                   Year = Year,
                   Value = Value) %>%
    dplyr::group_by(Site, Year) %>%
    dplyr::reframe(value_normal = ifelse(sum(is.na(Value)) > max_missing_months, 
                                         NA, 
                                         param_operator(Value, na.rm = T)),
                   record_length = record_length)
  
  
  df_summary <- unique (df_summary)
  
  # remove NA normals, ensure record_length is accurate & greater than input years_of_record
  df_normal <- df_summary %>%
    dplyr::filter(!is.na(value_normal)) %>%
    dplyr::group_by(Site) %>%
    dplyr::summarize(value_normal = round(mean(value_normal), 1), 
                     record_length = length(Site),
                     .groups = "drop") %>%
    dplyr::filter(record_length >= years_of_record)
  
  station_list <- unique(df_normal$Site)
  
  # matched Site in 'data' to Site in 'df_normal'
  # get select year & summarize
  df_select_year <- data %>%
    dplyr::filter(Site %in% station_list) %>%
    dplyr::filter(Year == select_year) %>%
    dplyr::group_by(Site, lat, lon, Year) %>% # if sum of non-NA vals < maxmonthlymissingdays then valueselect year is NA
    dplyr::summarize(value_select_year = ifelse(
      sum(!is.na(Value)) < max_monthly_missing_days,
      NA,
      param_operator(Value, na.rm = TRUE)),
      .groups = "drop") %>%
    dplyr::mutate(value_select_year = round(value_select_year, 1))
  
  # filters df_summary to keep rows where Sites match station_list
  # assigns ranks and only keeps select_year rows 
  
df_rank <- df_summary %>%
    dplyr::filter(Site %in% station_list) %>%
    dplyr::mutate(value_rank = rank(value_normal, na.last = "keep")) %>%
    dplyr::filter(Year == select_year) %>%
    dplyr::select(Site, value_rank) 
  
  # bind dfs for value normal, select_year, and rank
  df <- dplyr::left_join(df_normal, df_select_year, by = "Site") %>%
    dplyr::left_join(., df_rank, by = "Site") 
  
  
  # Added by MA:
  # replace NA values in lat/lon with values from sites csv (lots of NA coords)
  # create merged_df 
  merged_df <- merge(df, sites, by.x = "Site", by.y = "Station", all.x = TRUE)
  # pull latitude and longitude from merged_df to fill in NA lat/lon
  {df <- df %>%
      dplyr::left_join(merged_df %>% dplyr::select(Site, latitude, longitude),
                       by = c("Site" = "Site")) %>%
      dplyr::mutate(
        lat = ifelse(is.na(lat), latitude, lat),
        lon = ifelse(is.na(lon), longitude, lon)
      ) %>%
      dplyr::select(-latitude, -longitude)}
  
  
  # Added by MA:
  # function to pull value_normal from next closest station if record length is <'min_record_length'
  # note: need "geosphere" package 
  update_value_normal <- function(df) {
    # Identify rows with record_length < input value for min_record_length in main fun
    to_update <- df$record_length < min_record_length
    
    # Filter rows with record_length >= input value
    nearby_sites <- df[df$record_length >= min_record_length, ]
    
    # Ensure the coordinates are numeric
    coords_to_update <- df[to_update, c("lon", "lat")]
    coords_nearby_sites <- as.matrix(nearby_sites[c("lon", "lat")])
    
    # Calculate distances for each row with record_length < input value
    distances_matrix <- geosphere::distm(
      coords_to_update,
      cbind(coords_nearby_sites[, "lon"], coords_nearby_sites[, "lat"]),
      fun = geosphere::distVincentySphere
    )
    
    # Find the index of the closest site for each row with record_length < 10
    closest_site_indices <- apply(distances_matrix, 1, which.min)
    
    # Update the value_normal column with nearby sites with record_length < 10
    df$value_normal[to_update] <- nearby_sites$value_normal[closest_site_indices]
    
    return(df)
  }
  df <- update_value_normal(df)
  
  # Override specific sites for rainfall
  if (parameter == "rain" | parameter == "rain_mm" | parameter == "Rainfall" | parameter == "total_rain") {
    # Override value_normal for specific sites
    # Create a vector of sites to replace and their corresponding replacement sites
    sites_to_replace <- c("Tulita","Whati","Sambaa Ke","Colville Lake","ITH FTS","Snare rapids","Dempster515","Dempster85","Snare rapids","Gameti",
                          "Trail valley")
    replacement_sites <- c("Norman Wells","Gameti","Scotty Creek","Fort Good Hope","Blueberry","Gameti","Peel","Peel", "Yellowknife","Yellowknife",
                           "Inuvik")
    
    for (i in seq_along(sites_to_replace)) {
      site_to_replace <- sites_to_replace[i]
      replacement_site <- replacement_sites[i]
      
      if (site_to_replace %in% df$Site) {
        cat("Replacing", site_to_replace, "with", replacement_site, "\n")
        
        # Identify rows corresponding to the site to replace
        site_to_replace_indices <- which(df$Site == site_to_replace)
        
        # Replace value_normal
        df$value_normal[site_to_replace_indices] <- replacement_site
      }
    }
    
    
  } else{
    cat("No override performed. Parameter is not rain")
  }
  
  
  df <- df
  df$value_normal <- as.numeric(df$value_normal)
  df$value_select_year <- as.numeric(df$value_select_year)
  
  if(parameter == "mean_temp"| parameter == "t_air") {
    df <- df %>%
      dplyr::mutate(value_percent_normal = round((value_select_year - value_normal), 1)) %>%
      dplyr::filter(!is.na(value_percent_normal)) %>%
      dplyr::rename(longitude = lon, latitude = lat)
  } else {
    df <- df %>%
      dplyr::mutate(value_percent_normal = round((value_select_year / value_normal) * 100, 1)) %>%
      dplyr::filter(!is.na(value_percent_normal)) %>%
      dplyr::rename(longitude = lon, latitude = lat)
  }
  
  if(percent_of_normal == T) {
    df$bin <- cut(df$value_percent_normal,
                  legend_numbers, 
                  include.lowest = T,
                  labels = legend_labels)
  } else {
    df$bin <- cut(df$value_select_year,
                  legend_numbers, 
                  include.lowest = T,
                  labels = legend_labels)
  }
  
  if(parameter == "mean_temp"| parameter == "t_air") {
    bin_colours <- leaflet::colorFactor(palette = "RdYlBu", df$bin, reverse = T)
  } else {
    bin_colours <- leaflet::colorFactor(palette = "RdYlBu", df$bin)
  }
  
  # Add NA values for empty bins so that the legend includes all bins
  new_values <- levels(df$bin)[!c(levels(df$bin) %in% unique(df$bin))]
  if(length(new_values) > 0) {
    
    m <- matrix(ncol = ncol(df) - 1, nrow = length(new_values))
    df_add <- data.frame(m, new_values)
    colnames(df_add) <- colnames(df)
    df <- dplyr::bind_rows(df, df_add)
    df$bin <- factor(df$bin, levels = legend_labels)
    
  }
  
  # if % of norm & parameter contains "Mean Air Temperature" etc, assigns legend units accordingly
  if(percent_of_normal == T & grepl(paste0("(?i)", parameter), "Mean Air Temperature") == T |
     percent_of_normal == T & grepl(paste0("(?i)", parameter), "mean_temp") == T | 
     percent_of_normal == T & grepl(paste0("(?i)", parameter), "t_air") == T ) {
    legend_units <- " Anomaly" #
  } else if(percent_of_normal == T) {
    legend_units <- " (% of normal)"
  } else if(percent_of_normal == F & grepl(paste0("(?i)", parameter), "Snow Depth") == T |
            percent_of_normal == F & grepl(paste0("(?i)", parameter), "snow_depth") == T |
            percent_of_normal == F & grepl(paste0("(?i)", parameter), "total_snow") == T) {
    legend_units <- " (cm)"
  } else if(percent_of_normal == F & grepl(paste0("(?i)", parameter), "Snow Depth") == F |
            percent_of_normal == F & grepl(paste0("(?i)", parameter), "snow_depth") == F |
            percent_of_normal == F & grepl(paste0("(?i)", parameter), "total_snow") == F) {
    legend_units <- " (mm)"
  } else if(percent_of_normal == F & grepl(paste0("(?i)", parameter), "Mean Air Temperature") == T |
            percent_of_normal == F & grepl(paste0("(?i)", parameter), "mean_temp") == T |
            percent_of_normal == F & grepl(paste0("(?i)", parameter), "t_air") == T) {
    legend_units <- " (°C)"
  }
  
  # Define parameters for plot titles:
  if(grepl(paste0("(?i)", parameter), "Precipitation") |
     grepl(paste0("(?i)", parameter), "total_precip"))  {
    legend_title <- paste0("Precipitation", legend_units)
    parameter <- "total_precip"
    variable <- "precipitation"
    normal_units <- " (mm)"
    operator <- "sum"
  } else if(grepl(paste0("(?i)", parameter), "SWE")) {
    legend_title <- paste0("SWE", legend_units)
    parameter <- "SWE_mm"
    variable <- "SWE"
    normal_units <- " (mm)"
    operator <- "sum"
  } else if(grepl(paste0("(?i)", parameter), "Snow Depth") |
            grepl(paste0("(?i)", parameter), "total_snow") |
            grepl(paste0("(?i)", parameter), "snow_depth")) {
    legend_title <- paste0("Snow Depth", legend_units)
    parameter <- "snow_depth_cm"
    variable <- "snow depth"
    normal_units <- " (cm)"
    operator <- "sum"
  } else if(grepl(paste0("(?i)", parameter), "Rainfall") |
            grepl(paste0("(?i)", parameter), "rain") |
            grepl(paste0("(?i)", parameter), "total_rain")) {
    legend_title <- paste0("Rain", legend_units)
    parameter <- "rain_mm"
    variable <- "rain"
    normal_units <- " (mm)"
    operator <- "sum"
  } else if(grepl(paste0("(?i)", parameter), "Mean Air Temperature") |
            grepl(paste0("(?i)", parameter), "mean_temp") |
            grepl(paste0("(?i)", parameter), "t_air")) {
    legend_title <- paste0("Temperature <br>", legend_units)
    parameter <- "mean_temp"
    variable <- "temperature"
    normal_units <- " (°C)"
    operator <- "mean"
  }
  
  
  
  
  
  # Define proj variable to project in Leaflet
  proj <- '+proj=longlat +datum=WGS84'
  NWT <- sf::st_read("C:/Users/maucl/Documents/Shapefiles/MackenzieRiverBasin_FDA.shp")
  NWT <- sf::st_transform(NWT, sp::CRS(proj))
  NWT <- sf::st_zm(NWT)
  
  map <- leaflet::leaflet() %>%
    leaflet::addPolygons(data = NWT, color = "black", weight = 2, opacity = 0.5, fillOpacity = 0, group = "Mackenzie Basin") %>%
    leaflet::addLayersControl(
      baseGroups = c("CartoDB", "Open Street Map", "ESRI Aerial"),
      overlayGroups = c("NWT Boundary"),
      options = leaflet::layersControlOptions(collapsed = T)) %>%
    leaflet::addProviderTiles(leaflet::providers$CartoDB.PositronNoLabels, group = "CartoDB") %>%
    leaflet::addProviderTiles(leaflet::providers$OpenStreetMap.Mapnik, group = "Open Street Map") %>% 
    leaflet::addProviderTiles(leaflet::providers$Esri.WorldImagery, group = "ESRI Aerial") %>%
    leaflet::addLayersControl(
      baseGroups = c("CartoDB", "Open Street Map", "ESRI Aerial"),
      options = leaflet::layersControlOptions(collapsed = T)) %>%
    leaflet::addCircleMarkers(data = df,
                              color = "black", 
                              fillColor = ~bin_colours(bin),
                              radius = circle_radius, 
                              label = ~Site,
                              weight = 1,
                              opacity = 0.8,
                              fillOpacity = 0.8,
                              popup =~ paste0("Station name: ", Site, "<br>",
                                              select_year, " ", variable, " ", legend_title, " ", value_percent_normal, "<br>",
                                              select_year, " ", variable, normal_units, ": ", value_select_year, "<br>",
                                              "Average ", variable, normal_units, " ", value_normal, "<br>",
                                              "Years of record: ", record_length, "<br>")) %>%
    leaflet::addLegend(position = 'bottomright', pal = bin_colours, values = df$bin,
                       title = paste0(select_year,"\n",legend_title),
                       opacity = 1)  %>%
    leaflet::addScaleBar()
  
  if(is.na(filename)) {
    filename <- paste0(select_year, "_")
  } 
  
  htmlwidgets::saveWidget(map, file = paste0(savepath_climate, filename, ".html"))
  
  map
 
  cat("Note: warning message - rows missing or invalid lat/lon means there are empty bins")
   
}

# Note: if warning message that data contains missing lat/lon values:
# ^ it means there are empty bins (ie. none of the selected data fit into one (or some) of the bins on map legend)