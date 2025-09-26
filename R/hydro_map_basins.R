# Creating a Leaflet Map with shapefiles and gauge locations

# Call on libraries

library(leaflet)
library(sf)
library(raster)
library(htmlwidgets)
library(RColorBrewer)

#set user
user <- paste0("C:/Users/", tolower(Sys.getenv("USERNAME")), "/Documents/")

hydro_map_basin <- function(
    station = "07SB002",
    sub_basin_delineate,
    zoom = 2,
    Mack_basin = T,
    NWT_border = T,
    user = user,
    save = T,
    communities,
    plot_communities = F,
    cum_precip,
    plot_legend,
    save_path,
    plot_gauges,
    select_year,
    end_year = 2025,
    water_year_start,
    start_year = 1950,
    water_year,
    end_date = Sys.Date(),
    max_missing_days = 11,
    adjust_manual_cum_precip = F,
    phantomjspath
)

{

  proj <- '+proj=longlat +datum=WGS84'

  # Create a list of shapefile for the basin of interest

  user_path <- paste0("C:/Users/",
                      tolower(Sys.getenv("USERNAME")),
                      "/Documents/Shapefiles/")

  # Load and filter weather stations located in NWT
  mack_stations <- weathercan::stations() %>%
    dplyr::filter(prov == "NT"|prov == "BC"|prov == "AB") %>%
    dplyr::distinct(station_name, lat, lon)  # Keep only necessary columns

  stations <- mack_stations %>%
    dplyr::filter(station_name != "HAY RIVER") %>%
    dplyr::mutate(
      # Remove trailing single letters or suffix words
      clean_name = station_name %>%
        stringr::str_remove("\\s(?:[A-Z]|hydro|climate|airport|station|weather)$") %>%
        stringr::str_to_title()  # Convert to title case
    ) %>%
    dplyr::group_by(clean_name) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup()

  plot_stations <- stations %>%
    dplyr::filter(clean_name %in% communities)

  # Create a data frame for manually added communities
  manual_communities <- tibble::tibble(
    clean_name = c("Jean Marie River", "Katlodeeche First Nation", "Fort McPherson", "Watson Lake"),
    lat = c(61.5250, 60.83010, 67.4324, 60.05843),  # Replace with correct coordinates if needed
    lon = c(-120.62888, -115.76930, -134.8788, -128.72217)
  )

  # Add manual communities if they are in the community_names argument
  manual_communities <- manual_communities %>%
    dplyr::filter(clean_name %in% communities)

  # Combine the automatic and manual community data
  plot_stations <- dplyr::bind_rows(plot_stations, manual_communities)

  #adjust column naming
  plot_stations <- dplyr::rename(plot_stations, "Site"="clean_name")
  plot_stations$Site[plot_stations$Site=="Fort Mcmurray"] <- "Fort McMurray"

  if (sub_basin_delineate == T) {
    basins_list <- lapply(station, function(stn) {
      path <- paste0(user_path, stringr::str_sub(stn, 1, 2), "/", stn, "/", stn, "_DrainageBasin_BassinDeDrainage.shp")
      if (file.exists(path)) {
        basin <- sf::st_read(path,
                             layer = paste0(stn, "_DrainageBasin_BassinDeDrainage"),
                             quiet = TRUE)
        basin <- sf::st_transform(basin, sp::CRS(proj))
        basin <- sf::st_zm(basin)
        basin <- sf::st_make_valid(basin)
        basin$Station <- stn  # Add a column to identify the station
        return(basin)
      } else {
        warning(paste("Missing shapefile for station:", stn))
        return(NULL)
      }
    })

    basin <- do.call(rbind, basins_list[!sapply(basins_list, is.null)])
  }

  #plot gauges on map if desired
  if(plot_gauges == T){
    gauges <- tidyhydat::hy_stations()
    gauges <- gauges %>%
      dplyr::filter(gauges$HYD_STATUS == "ACTIVE")
    gauges <- sf::st_as_sf(gauges, coords = c("LONGITUDE", "LATITUDE"), crs = proj)
    gauges <- sf::st_make_valid(gauges)
    gauges <- sf::st_intersection(gauges, basin)

  }

  # Read in a proper Mackenzie River shapefile (if desired)
  if(Mack_basin == T) {
    Mack <- sf::st_read("C:/Users/emma_riley/Documents/Shapefiles/MackenzieRiverBasin_FDA.shp",
                        layer = "MackenzieRiverBasin_FDA")
    Mack <- sf::st_transform(Mack, sp::CRS(proj))
    Mack <- sf::st_zm(Mack)
  }

  if(NWT_border == T) {
    NWT <- sf::st_read(paste0("C:/Users/",
                              tolower(Sys.getenv("USERNAME")),
                              "/Documents/Shapefiles/NWT_Boundary/NWT_ENR_BND_FND.shp"),
                       layer = "NWT_ENR_BND_FND")
    NWT <- sf::st_transform(NWT, sp::CRS(proj))
    NWT <- sf::st_zm(NWT)
  }

  if(sub_basin_delineate == T){
    factpal <- colorFactor(viridis::viridis(length(station)), basin$Station)
  }

  if(cum_precip == T){

    plot_data <- clim_calc_daily(
      site = Communities,
      parameter = "total_precip",
      start_year = start_year,
      end_year = end_year,
      select_year = select_year,
      water_year_start = water_year_start
    )

    # Manually adjust 'today' to the end_date
    today <- end_date

    # Remove NA values at end of year for each site

    plot_data <- dplyr::filter(plot_data, Date <= today)

    # Find current julian day of the water year (i.e. 1 = first day of water year)

    today_JD <- max(dplyr::select(dplyr::filter(plot_data, Date == today), DayofYear))

    # Filter out all data after current julian day

    plot_data <- dplyr::filter(plot_data, DayofYear < today_JD,
                               WaterYear > min(plot_data$WaterYear))

    # Filter summary_data to max_missing_days argument

    if(water_year == "TRUE"){

      plot_data <- dplyr::reframe(dplyr::group_by(plot_data, Site, WaterYear),
                                  Value = sum(Value, na.rm = T),
                                  MissingDays = sum(Count))

    }else{
      plot_data <- dplyr::reframe(dplyr::group_by(plot_data, Site, CalendarYear),
                                  Value = sum(Value, na.rm = T),
                                  MissingDays = sum(Count))
    }

    if(adjust_manual_cum_precip == T){
      #use the line below to manually edit values - this is based on water_year_start and end_date
      plot_data$MissingDays[plot_data$Site=="Fort Simpson"&plot_data$CalendarYear==2024] <- 0


      plot_data <- dplyr::filter(plot_data, MissingDays <= max_missing_days)

      #use the line below to manually edit values - this is based on water_year_start and end_date
      plot_data$Value[plot_data$Site=="Fort Simpson"&plot_data$CalendarYear==2024] <- 132.2 #ER - updated Sept 26 for WSS slides

    }

    plot_data <- dplyr::filter(plot_data, MissingDays <= max_missing_days)

    # Choose a year to highlight on the map

    if(water_year == "TRUE"){
      plot_year <- plot_data$WaterYear == select_year
    } else{
      plot_year <- plot_data$CalendarYear == select_year
    }

    # percent of normal calculation

    map_year_value <- plot_data[plot_year, ]
    map_historical <- dplyr::reframe(dplyr::group_by(plot_data[!plot_year, ], Site), Mean = mean(Value, na.rm = T))
    map_historical <- dplyr::filter(map_historical, Site %in% map_year_value$Site)
    map_data <- dplyr::left_join(map_year_value, map_historical, by="Site")
    map_data <- dplyr::mutate(map_data, percentnorm= Value/Mean*100)
    map_data <- dplyr::left_join(map_data, plot_stations, by="Site")

    #create colour bins for mapping

    map_data$bin <- cut(map_data$percentnorm,
                        breaks = c(0, 50, 70, 90, 110, 130, 150, 500),
                        include.lowest = T,
                        labels = c("<50%", "51 - 70%", "71 - 90%", "91 - 110%",
                                   "111 - 130%", "131 - 150%", "> 151%"))

    PerCol <- leaflet::colorFactor(palette = "RdYlBu", map_data$bin)

  }

  # Create map

  map <- leaflet::leaflet() %>%
    leaflet::addProviderTiles(leaflet::providers$CartoDB.PositronNoLabels, group = "CartoDB")

  if(sub_basin_delineate == T){
    map <- map %>%
      addPolygons(data = basin,
                  color = "darkgrey", #replaced "~factpal(Station)" with "black" or "darkgrey"
                  fillColor = "darkgrey", #replaced "~factpal(Station)" with "green" or "darkgrey"
                  weight = 2,
                  opacity = 1,
                  fillOpacity = 0.5,
                  group = "Basin")
  }

  if(plot_gauges == T){
    map <- map %>%
      leaflet::addCircleMarkers(data = gauges, radius = 4, fillOpacity = 1, opacity =1, fillColor = "#FFF212", color = "black", weight = 1, popup = ~paste0(gauges$STATION_NUMBER))
  }

  if(NWT_border == T) {
    map <- map %>%
      leaflet::addPolygons(data = NWT, color = "black", weight = 1, opacity = 0.7, fillOpacity = 0, group = "NWT Border")
  }

  if(Mack_basin == T) {
    map <- map %>%
      leaflet::addPolygons(data = Mack, color = "black", weight = 1, opacity = 0.5, fillOpacity = 0, group = "NWT Border")
  }

  if(plot_communities == T & cum_precip == F){
    map <- map %>%
      addCircleMarkers(
        data = plot_stations,
        ~lon, ~lat,
        radius = 5,
        color = "blue",
        fillColor = "blue",
        fillOpacity = 1,
        opacity = 1,
        popup = ~Site,
        group = "NWT Communities")
  }

  if(cum_precip == T){
    map <- map %>%
      addCircleMarkers(
        data = map_data,
        ~lon, ~lat,
        radius = 8,
        weight = 1,
        color = "black",  # Apply the color palette
        fillColor = ~PerCol(bin),
        fillOpacity = 1,
        opacity = 1,
        popup = ~paste0(Site, ": ", percentnorm, " (% normal)"),
        group = "NWT Communities"
      )
    #change legend elements below based on timeframe
    if (plot_legend == T){
      map <- map %>%
        addLegend(
          position = "bottomleft",
          colors = c("#D73027","#FDAE61","#FEE090","#FFFFBF","#E0F3F8","#91BFDB","#4575B4"),
          labels = c("< 50%", "51 - 70%", "71 - 90%", "91 - 110%",
                     "111 - 130%", "131 - 150%", "> 151%"),
          title = paste0("Cumulative Precipitation", "<br>", "April to Oct ", select_year,  "<br>", "(% of normal)"),
          opacity = 1
        )
    }

  }

  map <- map %>%
    leaflet::addScaleBar(position = "bottomright")

  if(save == T){
    Sys.setenv(PATH = phantomjspath)
    htmlwidgets::saveWidget(map, file = paste0(save_path, "/map.html"), selfcontained = TRUE)
    webshot::webshot(paste0(save_path, "/map.html"),
                     paste0(save_path, "/Hydro_map_basins", ".png"),
                     delay = 5, vwidth = 1000, vheight = 800, zoom = zoom)
  }

  map

}

#test usage
# hydro_map_basin(
#   station = c("07NB001"),
#   zoom = 2,
#   Mack_basin = T,
#   NWT_border = T,
#   save = T,
#   sub_basin_delineate = F,
#   communities = c(  "Mackenzie",
#                     "Peace River",
#                     "Fort St John",
#                     "High Level",
#                     "Fort Chipewyan",
#                     "Fort Smith",
#                     "Hay River",
#                     "Yellowknife",
#                     "Fort Simpson",
#                     "Norman Wells",
#                     "Inuvik",
#                     "Fort Liard",
#                     "Fort Nelson",
#                     "Watson Lake",
#                     "Athabasca",
#                     "Fort Mcmurray",
#                     "Fort Good Hope"),
#   plot_communities = F,
#   plot_gauges = F,
#   cum_precip = T, #plots cumulative precipitation
#   save_path = paste0(user, "NT_Hydrology/Figures"),
#   select_year = 2024,
#   water_year_start = 4,
#   water_year = F,
#   end_date = "2024-10-01",
#   plot_legend = T,
#   adjust_manual_cum_precip = T, #change this to true if you are manually adjusting precip values based on gapfilling from FTS stns
#   phantomjspath = "C:/Users/emma_riley/Documents/Modelling/Phantomjs/phantomjs/bin" #get phantomjs executable and save in similar directory
# )

