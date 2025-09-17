#Hydrometric filter function

hydro_filter = function(x)

{ `%!in%` = Negate(`%in%`)

x <- x %>%
  dplyr::mutate(Date = as.Date(Date),
                Value = ifelse(STATION_NUMBER == "07QC002" & Date < as.Date("2011-01-01") & Date > as.Date("1973-10-01"),
                               Value + 340.456, Value)) %>%
  dplyr::mutate(Date = as.Date(Date),
                Value = ifelse(STATION_NUMBER == "07NA001" & Date < as.Date("2014-01-01"),
                               Value - 204.829, Value)) %>%
  dplyr::mutate(Date = as.Date(Date),
                Value = ifelse(STATION_NUMBER == "07OB008" & Date == as.Date("2013-04-16"),
                               NA, Value)) %>%
  dplyr::mutate(Date = as.Date(Date),
                Value = ifelse(STATION_NUMBER == "07QD002" & Date < as.Date("1968-09-19"),
                               Value + 2.637, Value)) %>%
  dplyr::mutate(Date = as.Date(Date),
                Value = ifelse(STATION_NUMBER == "10MC010" & Date >= as.Date("1997-07-30") & Date <= as.Date("1998-11-30"),
                               NA, Value)) %>%
  dplyr::mutate(Date = as.Date(Date),
                Value = ifelse(STATION_NUMBER == "10MC010" & Date == as.Date("2015-01-04") | STATION_NUMBER == "10MC010" & Date == as.Date("2015-02-08"),
                               NA, Value)) %>%
  dplyr::mutate(Date = as.Date(Date),
                Value = ifelse(STATION_NUMBER == "10EC003" & Date >= as.Date("2014-12-09") & Date <= as.Date("2015-04-20"),
                               NA, Value)) %>%
  dplyr::mutate(Date = as.Date(Date),
                Value = ifelse(STATION_NUMBER == "07KC005" & Date <= as.Date("2013-12-31"),
                               Value - 204.004, Value)) %>%
  dplyr::mutate(Date = as.Date(Date),
                Value = ifelse(STATION_NUMBER == "07NA008" & Date <= as.Date("2013-12-31"),
                               Value - 204.8, Value)) %>%
  dplyr::mutate(Date = as.Date(Date),
                Value = ifelse(STATION_NUMBER == "07NA007" & Date <= as.Date("2013-12-31"),
                               Value - 204.372, Value)) %>%
  dplyr::mutate(Date = as.Date(Date),
                Value = ifelse(STATION_NUMBER == "07MD001" & Date <= as.Date("2013-12-31"),
                               Value - 204.005, Value)) %>%
  dplyr::mutate(Date = as.Date(Date),
                Value = ifelse(STATION_NUMBER == "07KF003" & Date <= as.Date("2013-12-31"),
                               Value - 203.996, Value)) %>%
  dplyr::mutate(Date = as.Date(Date),
                Value = ifelse(STATION_NUMBER == "07KF002" & Date <= as.Date("2013-12-31"),
                               Value - 204.087, Value)) %>%
  as_tibble()

}
