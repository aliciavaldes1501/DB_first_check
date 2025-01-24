threshold <- 0.001

selection <- db_updated %>%
  filter(`ReSurvey plot (Y/N)` == "Y") %>%
  mutate(Lon_updated = ifelse(is.na(Lon_prec),Longitude,Lon_prec),
         Lat_updated = ifelse(is.na(Lat_prec),Latitude,Lat_prec)) %>%
  group_by(RS_CODE, `ReSurvey site`, `ReSurvey plot`) %>%
  mutate(
    lon_range = ifelse(all(is.na(Lon_updated)), NA,
                       max(Lon_updated, na.rm = T) - 
                         min(Lon_updated, na.rm = T)),
    lat_range = ifelse(all(is.na(Lat_updated)), NA,
                       max(Lat_updated, na.rm = T) - 
                         min(Lat_updated, na.rm = T)),
    coordinates_equal = ifelse(is.na(Lon_updated) & is.na(Lat_updated), NA,
                               lon_range == 0 & lat_range == 0),
    coordinates_consistent = ifelse(is.na(Lon_updated) & is.na(Lat_updated), NA,
                                    lon_range < threshold & 
                                      lat_range < threshold)
  ) %>%
  ungroup() %>%
  select(-lon_range, -lat_range) %>% filter(coordinates_equal==FALSE) %>%
  arrange(`ReSurvey plot`) %>%
  #filter(RS_CODE == "CZ_0011")
  filter(RS_CODE == "CZ_0016" & Dataset == "CZ16_ObranskaStran")

nrow(selection)
print(selection, width = Inf)

# read_csv(here("output", "csv","issue3.csv"))

read_csv(here("output","csv","issue3.csv"))%>%filter(RS_CODE =="CH_0002", Dataset=="CH_ArableRes")

