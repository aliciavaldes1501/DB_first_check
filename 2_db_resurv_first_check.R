## -------------------------------------------------------------------------------
library(here)
library(readr)
library(tidyr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(stringr)
library(purrr)
library(readxl)


## -------------------------------------------------------------------------------
db_resurv<-read_tsv(here("data", "edited", "db_resurv.csv"))


## -------------------------------------------------------------------------------
problems<-problems(db_resurv)
sort(unique(problems$col))
names(db_resurv[c(7,13)])


## -------------------------------------------------------------------------------
db_resurv <- db_resurv %>%
  mutate(Lon_updated = ifelse(is.na(Lon_prec),Longitude,Lon_prec),
         Lat_updated = ifelse(is.na(Lat_prec),Latitude,Lat_prec))


## -------------------------------------------------------------------------------
print(db_resurv, width = Inf)


## -------------------------------------------------------------------------------
nrow(db_resurv%>%filter(is.na(RS_CODE)))
nrow(db_resurv%>%filter(is.na(`ReSurvey site`)))
nrow(db_resurv%>%filter(is.na(`ReSurvey plot`)))


## -------------------------------------------------------------------------------
write_csv(db_resurv %>% filter(is.na(`ReSurvey plot`)),
          here("output", "csv","issue1.csv"))


## -------------------------------------------------------------------------------
nrow(db_resurv%>%filter(is.na(Lon_updated) & is.na(Lat_updated )))


## -------------------------------------------------------------------------------
write_csv(db_resurv %>% filter(is.na(Lon_updated) & is.na(Lat_updated)),
          here("output", "csv","issue2.csv"))


## -------------------------------------------------------------------------------
nrow(db_resurv%>%
       filter(is.na(`ReSurvey plot`) & is.na(Lon_updated) & is.na(Lat_updated)))


## -------------------------------------------------------------------------------
# Define a threshold (e.g., 0.001 degrees for longitude/latitude differences)
threshold <- 0.001

db_resurv <- db_resurv %>%
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
  select(-lon_range, -lat_range)


## -------------------------------------------------------------------------------
write_csv(db_resurv %>% filter(coordinates_equal==FALSE),
          here("output", "csv","issue3.csv"))


## -------------------------------------------------------------------------------
db_resurv %>% 
  group_by(RS_CODE,`ReSurvey site`, `ReSurvey plot`) %>%
  summarize(is_equal = all(coordinates_equal),
            is_consistent = all(coordinates_consistent),
            .groups = "drop") %>%
  mutate(coordinate_status = case_when(
    is_equal ~ "Equal",
    !is_equal & is_consistent ~ "Consistent (< 0.001º)",
    !is_equal & !is_consistent ~ "Inconsistent (> 0.001º)")) %>%
  count(coordinate_status)%>%
  mutate(percentage = n / sum(n) * 100) %>%
  ggplot(aes(x = percentage, y = coordinate_status, fill = coordinate_status)) +
  geom_bar(stat = "identity") + 
  geom_text(aes(label = paste0(round(percentage, 1), "%")),
            position = position_stack(vjust = 0.5), size = 3) + 
  labs(x = "Percentage of Plots", y = NULL) +
  theme(axis.text.y = element_text(size = 12)) +
  coord_flip() + theme(legend.position = "none")
ggsave(filename=here("output", "figures","issue3.tiff"),
       width=10,height=7,units="cm",dpi=300)


## -------------------------------------------------------------------------------
count_resurveys <- db_resurv %>%
  # Convert dates to date format and get the year
  mutate(date = dmy(`Date of recording`), year = year(date)) %>%
  group_by(RS_CODE, `ReSurvey site`,
           # If ReSurvey plot is not NA, 
           # group by RS_CODE, `ReSurvey site`, `ReSurvey plot`
           `ReSurvey plot` = ifelse(is.na(`ReSurvey plot`), 
                                    NA_character_, `ReSurvey plot`),
           # If ReSurvey plot is NA, group by coordinates
           Lon_updated = ifelse(is.na(`ReSurvey plot`), Lon_updated, NA_real_),
           Lat_updated = ifelse(is.na(`ReSurvey plot`) , Lat_updated, NA_real_)
  ) %>%
  summarise(
    # Get how many different years for each unique group
    distinct_years=n_distinct(year), 
    # Get how many different dates for each unique group
    distinct_dates=n_distinct(date), .groups = "drop")


## -------------------------------------------------------------------------------
summary(count_resurveys$distinct_years)
sd(count_resurveys$distinct_years)


## -------------------------------------------------------------------------------
# For all data
ggplot(count_resurveys, aes(x = distinct_years)) + 
  geom_histogram(fill = "white", color = "black", bins = 55)+
  xlab("Number of ReSurvey observations (different years)") +
  ylab("Number of plots")
ggsave(filename=here("output", "figures","issue4.tiff"),
       width=11,height=7,units="cm",dpi=300)


## -------------------------------------------------------------------------------
nrow(count_resurveys%>%filter(distinct_years==1))
nrow(count_resurveys%>%filter(distinct_years==1))/nrow(count_resurveys)


## -------------------------------------------------------------------------------
write_csv(count_resurveys%>%filter(distinct_years==1),
          here("output", "csv","issue4.csv"))


## -------------------------------------------------------------------------------
db_resurv %>%
  filter(`Cover abundance scale`=="Presence/Absence") %>%
  distinct(Dataset)


## -------------------------------------------------------------------------------
ggplot(db_resurv %>% 
         mutate(pres_or_ab =ifelse(`Cover abundance scale`=="Presence/Absence",
                                   "Presence/Absence", "Abundance"),
                DK_Naturdata_Res = ifelse(Dataset == "DK_Naturdata_Res",
                                          "Y", "N")),
                aes(pres_or_ab, fill = DK_Naturdata_Res)) +
         geom_bar(aes(y = (..count..) / sum(..count..) * 100)) +
  labs(y = "Percentage", x = NULL)
ggsave(filename=here("output", "figures","issue5.tiff"),
       width=12,height=7,units="cm",dpi=300)


## -------------------------------------------------------------------------------
wrong_countries <- read_delim(here("data", "clean","wrong_countries.txt"),
                              delim = ";")


## -------------------------------------------------------------------------------
write_csv(wrong_countries, here("output", "csv","issue6.csv"))


## -------------------------------------------------------------------------------
ggplot(db_resurv, aes(`Cover abundance scale`)) +
         geom_bar(aes(y = (..count..) / sum(..count..) * 100)) +
  labs(y = "Percentage of ReSurvey observations", x = "Cover abundance scale") +
  coord_flip()
ggsave(filename=here("output", "figures","issue7.tiff"),
       width=18,height=10,units="cm",dpi=300)


## -------------------------------------------------------------------------------
db_resurv <- db_resurv %>%
  mutate(
    # Clean 'Expert System' column by removing "!" and replacing "~" with NA
    `Expert System` = case_when(
      `Expert System` == "~" ~ NA_character_,  # Replace "~" with NA
      TRUE ~ str_replace_all(`Expert System`, "!", "")  # Remove "!"
    )
  ) %>%
  # Separate the values in 'Expert System' into multiple columns
  separate(
    `Expert System`,
    into = c("EUNISa", "EUNISb", "EUNISc", "EUNISd"),
    sep = ",",
    extra = "drop",  # Drop extra values if there are more than columns
    fill = "right",   # Fill missing values with NA for cases with fewer values
    remove = FALSE    # Keep the original 'Expert System' column
  )


## -------------------------------------------------------------------------------
db_resurv <- db_resurv %>%
  mutate(
    # Count the number of non-NA values across the EUNIS columns
    n_EUNIS = rowSums(!is.na(select(., starts_with("EUNIS"))))
  )


## -------------------------------------------------------------------------------
ggplot(db_resurv, aes(n_EUNIS)) +
         geom_bar(aes(y = (..count..) / sum(..count..) * 100)) +
  labs(y = "Percentage of ReSurvey observations",
       x = "Number of differnt EUNIS codes assigned") + coord_flip()
ggplot(db_resurv %>% filter(n_EUNIS > 0), aes(n_EUNIS)) +
         geom_bar(aes(y = (..count..) / sum(..count..) * 100)) +
  labs(y = "Percentage of ReSurvey observations",
       x = "Number of differnt EUNIS codes assigned") + coord_flip()


## -------------------------------------------------------------------------------
db_resurv <- db_resurv %>%
  mutate(across(starts_with("EUNIS"), ~ case_when(
    . == "N16M" ~ "N16",
    . == "Sa" ~ "V4",
    . == "Sb" ~ "V5",
    . == "T1CT" ~ "T1C",
    . == "N15A" ~ "N15",
    TRUE ~ .
  )))


## -------------------------------------------------------------------------------
db_resurv <- db_resurv %>%
  mutate(
    # EUNISa levels
    EUNISa_1 = substr(EUNISa, 1, ifelse(str_starts(EUNISa, "MA"), 2, 1)),
    EUNISa_2 = ifelse(
      nchar(EUNISa) >= ifelse(str_starts(EUNISa, "MA"), 3, 2), 
      substr(EUNISa, 1, ifelse(str_starts(EUNISa, "MA"), 3, 2)),
      NA_character_
    ),
    EUNISa_3 = ifelse(
      nchar(EUNISa) >= ifelse(str_starts(EUNISa, "MA"), 4, 3), 
      substr(EUNISa, 1, ifelse(str_starts(EUNISa, "MA"), 4, 3)),
      NA_character_
      ),
    EUNISa_4 = ifelse(
      nchar(EUNISa) >= ifelse(str_starts(EUNISa, "MA"), 5, 4), 
      substr(EUNISa, 1, ifelse(str_starts(EUNISa, "MA"), 5, 4)),
      NA_character_
    ),
    
    # EUNISb levels
    EUNISb_1 = substr(EUNISb, 1, ifelse(str_starts(EUNISb, "MA"), 2, 1)),
    EUNISb_2 = ifelse(
      nchar(EUNISb) >= ifelse(str_starts(EUNISb, "MA"), 3, 2), 
      substr(EUNISb, 1, ifelse(str_starts(EUNISb, "MA"), 3, 2)),
      NA_character_
    ),
    EUNISb_3 = ifelse(
      nchar(EUNISb) >= ifelse(str_starts(EUNISb, "MA"), 4, 3), 
      substr(EUNISb, 1, ifelse(str_starts(EUNISb, "MA"), 4, 3)),
      NA_character_
    ),
    EUNISb_4 = ifelse(
      nchar(EUNISb) >= ifelse(str_starts(EUNISb, "MA"), 5, 4), 
      substr(EUNISb, 1, ifelse(str_starts(EUNISb, "MA"), 5, 4)),
      NA_character_
    ),
    
    # EUNISc levels
    EUNISc_1 = substr(EUNISc, 1, ifelse(str_starts(EUNISc, "MA"), 2, 1)),
    EUNISc_2 = ifelse(
      nchar(EUNISc) >= ifelse(str_starts(EUNISc, "MA"), 3, 2), 
      substr(EUNISc, 1, ifelse(str_starts(EUNISc, "MA"), 3, 2)),
      NA_character_
    ),
    EUNISc_3 = ifelse(
      nchar(EUNISc) >= ifelse(str_starts(EUNISc, "MA"), 4, 3), 
      substr(EUNISc, 1, ifelse(str_starts(EUNISc, "MA"), 4, 3)),
      NA_character_
    ),
    EUNISc_4 = ifelse(
      nchar(EUNISc) >= ifelse(str_starts(EUNISc, "MA"), 5, 4), 
      substr(EUNISc, 1, ifelse(str_starts(EUNISc, "MA"), 5, 4)),
      NA_character_
    ),
    
    # EUNISd levels
    EUNISd_1 = substr(EUNISd, 1, ifelse(str_starts(EUNISc, "MA"), 2, 1)),
    EUNISd_2 = ifelse(
      nchar(EUNISd) >= ifelse(str_starts(EUNISd, "MA"), 3, 2), 
      substr(EUNISd, 1, ifelse(str_starts(EUNISd, "MA"), 3, 2)),
      NA_character_
    ),
    EUNISd_3 = ifelse(
      nchar(EUNISd) >= ifelse(str_starts(EUNISd, "MA"), 4, 3), 
      substr(EUNISd, 1, ifelse(str_starts(EUNISd, "MA"), 4, 3)),
      NA_character_
    ),
    EUNISd_4 = ifelse(
      nchar(EUNISd) >= ifelse(str_starts(EUNISd, "MA"), 5, 4), 
      substr(EUNISd, 1, ifelse(str_starts(EUNISd, "MA"), 5, 4)),
      NA_character_
    )
  )


## -------------------------------------------------------------------------------
db_resurv <- db_resurv %>%
  mutate(
    EUNISa_1_descr = case_when(
      EUNISa_1 == "V" ~ "Vegetated man-made habitats",
      EUNISa_1 == "U" ~ "Inland habitats with no or little soil",
      EUNISa_1 == "T" ~ "Forests and other wooded land",
      EUNISa_1 == "S" ~ "Heathlands, scrub and tundra",
      EUNISa_1 == "R" ~ "Grasslands",
      EUNISa_1 == "Q" ~ "Wetlands",
      EUNISa_1 == "P" ~ "Inland waters",
      EUNISa_1 == "N" ~ "Coastal habitats",
      EUNISa_1 == "MA" ~ "Marine habitats",
      TRUE ~ NA_character_
    ),
    EUNISb_1_descr = case_when(
      EUNISb_1 == "V" ~ "Vegetated man-made habitats",
      EUNISb_1 == "U" ~ "Inland habitats with no or little soil",
      EUNISb_1 == "T" ~ "Forests and other wooded land",
      EUNISb_1 == "S" ~ "Heathlands, scrub and tundra",
      EUNISb_1 == "R" ~ "Grasslands",
      EUNISb_1 == "Q" ~ "Wetlands",
      EUNISb_1 == "P" ~ "Inland waters",
      EUNISb_1 == "N" ~ "Coastal habitats",
      EUNISb_1 == "MA" ~ "Marine habitats",
      TRUE ~ NA_character_
    ),
    EUNISc_1_descr = case_when(
      EUNISc_1 == "V" ~ "Vegetated man-made habitats",
      EUNISc_1 == "U" ~ "Inland habitats with no or little soil",
      EUNISc_1 == "T" ~ "Forests and other wooded land",
      EUNISc_1 == "S" ~ "Heathlands, scrub and tundra",
      EUNISc_1 == "R" ~ "Grasslands",
      EUNISc_1 == "Q" ~ "Wetlands",
      EUNISc_1 == "P" ~ "Inland waters",
      EUNISc_1 == "N" ~ "Coastal habitats",
      EUNISc_1 == "MA" ~ "Marine habitats",
      TRUE ~ NA_character_
    ),
    EUNISd_1_descr = case_when(
      EUNISd_1 == "V" ~ "Vegetated man-made habitats",
      EUNISd_1 == "U" ~ "Inland habitats with no or little soil",
      EUNISd_1 == "T" ~ "Forests and other wooded land",
      EUNISd_1 == "S" ~ "Heathlands, scrub and tundra",
      EUNISd_1 == "R" ~ "Grasslands",
      EUNISd_1 == "Q" ~ "Wetlands",
      EUNISd_1 == "P" ~ "Inland waters",
      EUNISd_1 == "N" ~ "Coastal habitats",
      EUNISd_1 == "MA" ~ "Marine habitats",
      TRUE ~ NA_character_
    )
  )


## -------------------------------------------------------------------------------
ggplot(db_resurv, aes(EUNISa_1_descr)) +
         geom_bar(aes(y = (..count..) / sum(..count..) * 100)) +
  labs(y = "Percentage of ReSurvey observations",
       x = "EUNIS level 1") + coord_flip()
ggplot(db_resurv %>% filter(!is.na(EUNISa_1_descr)), aes(EUNISa_1_descr)) +
         geom_bar(aes(y = (..count..) / sum(..count..) * 100)) +
  labs(y = "Percentage of ReSurvey observations",
       x = "EUNIS level 1") + coord_flip()
ggsave(filename=here("output", "figures","issue8.tiff"),
       width=18,height=10,units="cm",dpi=300)


## -------------------------------------------------------------------------------
ggplot(db_resurv, aes(`Manipulate (y/n)`)) +
         geom_bar(aes(y = (..count..) / sum(..count..) * 100)) +
  labs(y = "Percentage of ReSurvey observations",
       x = "Manipulation")
ggsave(filename=here("output", "figures","issue9.tiff"),
       width=10,height=8,units="cm",dpi=300)


## -------------------------------------------------------------------------------
write_csv(data.frame(unique(db_resurv$`Type of manipulation`)),
          here("output", "csv","issue9.csv"))


## -------------------------------------------------------------------------------
ggplot(db_resurv, aes(`Location method`)) +
         geom_bar(aes(y = (..count..) / sum(..count..) * 100)) +
  labs(y = "Percentage of ReSurvey observations",
       x = "Location method") + coord_flip()
ggsave(filename=here("output", "figures","issue10.tiff"),
       width=18,height=8,units="cm",dpi=300)


## -------------------------------------------------------------------------------
unique(db_resurv$RS_PROJTYP)


## -------------------------------------------------------------------------------
db_resurv <- db_resurv %>%
  mutate(RS_PROJTYP = recode(RS_PROJTYP,
                             "Resampling" = "resampling",
                             "Permanent (man)" = "permanent (man)"))


## -------------------------------------------------------------------------------
unique(db_resurv$RS_PROJTYP)


## -------------------------------------------------------------------------------
ggplot(db_resurv, aes(RS_PROJTYP, fill=`Manipulate (y/n)`)) +
         geom_bar(aes(y = (..count..) / sum(..count..) * 100)) +
  labs(y = "Percentage of ReSurvey observations",
       x = "Resurvey project type") + coord_flip() +
  theme(legend.position = "top")
ggsave(filename=here("output", "figures","issue11.tiff"),
       width=18,height=8,units="cm",dpi=300)


## -------------------------------------------------------------------------------
db_resurv %>% filter(!is.na(RS_DUPL)) %>% select(RS_CODE, RS_DUPL) %>%
  distinct()


## -------------------------------------------------------------------------------
db_resurv <- db_resurv %>%
  # Redefine precision_new, which was wrong
  mutate(precision_new = factor(ifelse(is.na(Lon_prec) & is.na(Lat_prec),
                                       0, 1)))


## -------------------------------------------------------------------------------
ggplot(db_resurv, aes(`Location uncertainty (m)`, fill = precision_new)) +
  geom_histogram( color = "black") +
  xlab("Location uncertainty (m)")
ggplot(db_resurv %>% filter(`Location uncertainty (m)` <= 500),
       aes(`Location uncertainty (m)`, fill = precision_new)) +
  geom_histogram(color = "black") +
  xlab("Location uncertainty (m) <= 500")
ggsave(filename=here("output", "figures","issue13_1.tiff"),
       width=18,height=8,units="cm",dpi=300)
ggplot(db_resurv %>% filter(`Location uncertainty (m)` > 500),
       aes(`Location uncertainty (m)`, fill = precision_new)) +
  geom_histogram(color = "black") +
  xlab("Location uncertainty (m) > 500")
ggsave(filename=here("output", "figures","issue13_2.tiff"),
       width=18,height=8,units="cm",dpi=300)


## -------------------------------------------------------------------------------
unique((db_resurv)$`Slope (°)`) %>% str_sort()


## -------------------------------------------------------------------------------
db_resurv <- db_resurv %>%
  mutate(
    # Some altitude values have a "-" after the number,
    # convert to numeric after removing that
    Altitude = as.numeric(gsub("-", "", Altitude)),
    # Some slope values are noted as "_" or "-", these should be NA,
    # otherwise convert to numeric
    `Slope (°)` = ifelse(`Slope (°)` == "_" | `Slope (°)` == "-",
                   NA, as.numeric(`Slope (°)`)),
    # Convert aspect values to numeric
    `Aspect (°)` = as.numeric(`Aspect (°)`)
    )


## -------------------------------------------------------------------------------
ggplot(db_resurv, aes(Altitude)) +
  geom_histogram(fill = "white", color = "black")
ggplot(db_resurv, aes(`Aspect (°)`)) +
  geom_histogram(fill = "white", color = "black")
ggplot(db_resurv, aes(`Slope (°)`)) +
  geom_histogram(fill = "white", color = "black")
ggsave(filename=here("output", "figures","issue8.tiff"),
       width=18,height=10,units="cm",dpi=300)


## -------------------------------------------------------------------------------
range(db_resurv$`Slope (°)`, na.rm=T)


## -------------------------------------------------------------------------------
db_resurv <- db_resurv %>%
  mutate(date = dmy(`Date of recording`), year = year(date))


## -------------------------------------------------------------------------------
ggplot(db_resurv, aes(year)) + geom_histogram(fill = "white", color = "black")


## -------------------------------------------------------------------------------
ggplot(db_resurv, aes(`Relevé area (m²)`)) +
  geom_histogram(fill = "white", color = "black")


## -------------------------------------------------------------------------------
nrow(db_resurv %>% filter(is.na(`Relevé area (m²)`)))


## -------------------------------------------------------------------------------
db_resurv %>%
  pivot_longer(cols = c(`Cover total (%)`, `Cover tree layer (%)`,
                        `Cover shrub layer (%)`, `Cover herb layer (%)`,
                        `Cover moss layer (%)`),
               names_to = "variable", values_to = "value") %>%
  ggplot(aes(x = value)) +
  geom_histogram(fill = "white", color = "black", bins = 10) +
  facet_wrap(~ variable, scales = "free") +
  labs(x = "Value", y = "Frequency")


## -------------------------------------------------------------------------------
db_resurv %>%
  reframe(across(c(`Cover total (%)`, `Cover tree layer (%)`,
                     `Cover shrub layer (%)`, `Cover herb layer (%)`,
                     `Cover moss layer (%)`), ~range(., na.rm = TRUE)))


## -------------------------------------------------------------------------------
ggplot(db_resurv, aes(`Mosses identified (Y/N)`)) + geom_bar()
ggplot(db_resurv, aes(`Lichens identified (Y/N)`)) + geom_bar()


## -------------------------------------------------------------------------------
db_Europa<- db_resurv %>%
  group_by(RS_CODE, `ReSurvey site`,
           # If ReSurvey plot is not NA, 
           # group by RS_CODE, `ReSurvey site`, `ReSurvey plot`
           `ReSurvey plot` = ifelse(is.na(`ReSurvey plot`), 
                                    NA_character_, `ReSurvey plot`),
           # If ReSurvey plot is NA, group by coordinates
           # Create a unique grouping variable that uses coordinates
           # only when conditions are met
           group_coords = ifelse(is.na(`ReSurvey plot`),
                                 paste(Lon_updated, Lat_updated), NA_character_)
  ) %>%
  # Add unique identifiers for each plot.
  # These are based on the unique combination of RS_CODE, ReSurvey site and 
  # ReSurvey plot (When ReSurvey plot is not NA)
  # and on the unique combination of RS_CODE, ReSurvey site 
  # and updated coordinates (When ReSurvey plot is NA)
  mutate(plot_unique_id = cur_group_id()) %>%
  select(PlotObservationID, Country, `Date of recording`, RS_CODE,
         `ReSurvey site`, `ReSurvey plot`, Lon_updated, Lat_updated,
         group_coords, `Expert System`, `Location method`, plot_unique_id) %>%
  ungroup() %>%
  # Convert dates to date format and get the year
  mutate(date = dmy(`Date of recording`), year = year(date)) %>%
  select(-`Date of recording`, -date, -group_coords) %>%
  # Add unique identifiers for each observation
  mutate(obs_unique_id = row_number())


## -------------------------------------------------------------------------------
print(db_Europa, width = Inf)


## -------------------------------------------------------------------------------
write_csv(db_Europa,here("data", "clean","db_Europa_20250107.csv"))


## -------------------------------------------------------------------------------
write_csv(db_Europa %>% 
            select(obs_unique_id, plot_unique_id, Lon_updated, Lat_updated,
                   year),
          here("data", "clean","db_Europa_20241210_short.csv"))


## -------------------------------------------------------------------------------
db_DK_J<-read_tsv(here("data", "raw",
                       "DK_Naturdata_Res_habitat_hab_codes_Jesper",
                  "DK_Naturdata_Res_habitat_hab_codes.txt"))


## -------------------------------------------------------------------------------
db_resurv <- db_resurv %>%
  # Keeping all obs in db_resurv but not all in db_DK_J
  left_join(db_DK_J %>% select(PlotObservationID, HabitatID))


## -------------------------------------------------------------------------------
print(db_resurv %>% distinct(HabitatID), n = 100)


## -------------------------------------------------------------------------------
write_csv(db_resurv %>% distinct(HabitatID),
          here("data", "clean","list_HabitatID_DK_resurv.csv"))


## -------------------------------------------------------------------------------
nrow(db_resurv %>% filter(is.na(`Expert System`)))/nrow(db_resurv)


## -------------------------------------------------------------------------------
nrow(db_resurv %>% filter(is.na(`Expert System`)&!is.na(HabitatID)))/nrow(db_resurv)


## -------------------------------------------------------------------------------
nrow(db_resurv %>% 
       filter(is.na(`Expert System`)&is.na(HabitatID)))/nrow(db_resurv)


## -------------------------------------------------------------------------------
nrow(db_resurv %>%
       filter(is.na(`Expert System`) &
                is.na(HabitatID) &
                `Cover abundance scale` == "Presence/Absence"))/
  nrow(db_resurv)


## -------------------------------------------------------------------------------
nrow(db_resurv %>%
       filter(is.na(`Expert System`) &
                is.na(HabitatID) &
                `Cover abundance scale` != "Presence/Absence"))/
  nrow(db_resurv)


## -------------------------------------------------------------------------------
db_resurv <- db_resurv %>%
  mutate(HabitatID = as.character(HabitatID)) %>%
  mutate(HabitatID = ifelse(HabitatID == "9998", "91D0",
                            ifelse(HabitatID == "9999", "91E0", HabitatID)))


## -------------------------------------------------------------------------------
correspondences<-read_excel(here("data", "edited",
                                 "correspondence_HabitatID_DK.xlsx"))


## -------------------------------------------------------------------------------
db_resurv <- db_resurv %>%
  # Keeping all obs in db_resurv but not all in db_DK_J
  left_join(correspondences %>% select(HabitatID, EUNIS))


## -------------------------------------------------------------------------------
db_resurv <- db_resurv %>%
  mutate(EUNIS = ifelse(EUNIS == "NA", NA, EUNIS))


## -------------------------------------------------------------------------------
db_resurv <- db_resurv %>%
  mutate(EUNISa =
           # If EUNIS (DK) is available, add as EUNISa
           ifelse(!is.na(EUNIS), EUNIS, 
                  # Otherwise keep EUNISa
                  EUNISa),
         EUNIS_assignation = ifelse(!is.na(EUNIS), "Info from DK",
                                    ifelse(is.na(EUNISa), "Not possible",
                                           "Expert system"))) %>%
  # Remove column EUNIS (DK)
  select(-EUNIS)


## -------------------------------------------------------------------------------
ggplot(db_resurv, aes(EUNIS_assignation)) +
         geom_bar(aes(y = (..count..) / sum(..count..) * 100)) +
  labs(y = "Percentage of ReSurvey observations",
       x = "EUNIS assignation")


## -------------------------------------------------------------------------------
write_csv(db_resurv %>%
            filter(is.na(`Expert System`)&is.na(HabitatID)),
          here("output", "csv","issue5.csv"))


## -------------------------------------------------------------------------------
db_resurv <- db_resurv %>%
  mutate(
    # EUNISa levels
    EUNISa_1 = substr(EUNISa, 1, ifelse(str_starts(EUNISa, "MA"), 2, 1)),
    EUNISa_2 = ifelse(
      nchar(EUNISa) >= ifelse(str_starts(EUNISa, "MA"), 3, 2), 
      substr(EUNISa, 1, ifelse(str_starts(EUNISa, "MA"), 3, 2)),
      NA_character_
    ),
    EUNISa_3 = ifelse(
      nchar(EUNISa) >= ifelse(str_starts(EUNISa, "MA"), 4, 3), 
      substr(EUNISa, 1, ifelse(str_starts(EUNISa, "MA"), 4, 3)),
      NA_character_
      ),
    EUNISa_4 = ifelse(
      nchar(EUNISa) >= ifelse(str_starts(EUNISa, "MA"), 5, 4), 
      substr(EUNISa, 1, ifelse(str_starts(EUNISa, "MA"), 5, 4)),
      NA_character_
    )
  ) %>%
  # Remove HabitatID column
  select(-HabitatID)


## -------------------------------------------------------------------------------
db_resurv <- db_resurv %>%
  mutate(
    EUNISa_1_descr = case_when(
      EUNISa_1 == "V" ~ "Vegetated man-made habitats",
      EUNISa_1 == "U" ~ "Inland habitats with no or little soil",
      EUNISa_1 == "T" ~ "Forests and other wooded land",
      EUNISa_1 == "S" ~ "Heathlands, scrub and tundra",
      EUNISa_1 == "R" ~ "Grasslands",
      EUNISa_1 == "Q" ~ "Wetlands",
      EUNISa_1 == "P" ~ "Inland waters",
      EUNISa_1 == "N" ~ "Coastal habitats",
      EUNISa_1 == "MA" ~ "Marine habitats",
      TRUE ~ NA_character_
    )
  )


## -------------------------------------------------------------------------------
db_resurv <- db_resurv %>%
  mutate(
    # Count the number of non-NA values across the EUNIS columns
    n_EUNIS = rowSums(!is.na(select(., EUNISa:EUNISd)))
  )


## -------------------------------------------------------------------------------
ggplot(db_resurv, aes(n_EUNIS)) +
         geom_bar(aes(y = (..count..) / sum(..count..) * 100)) +
  labs(y = "Percentage of ReSurvey observations",
       x = "Number of differnt EUNIS codes assigned") + coord_flip()
ggplot(db_resurv %>% filter(n_EUNIS > 0), aes(n_EUNIS)) +
         geom_bar(aes(y = (..count..) / sum(..count..) * 100)) +
  labs(y = "Percentage of ReSurvey observations",
       x = "Number of differnt EUNIS codes assigned") + coord_flip()


## -------------------------------------------------------------------------------
ggplot(db_resurv, aes(EUNISa_1_descr)) +
         geom_bar(aes(y = (..count..) / sum(..count..) * 100)) +
  labs(y = "Percentage of ReSurvey observations",
       x = "EUNIS level 1") + coord_flip()
ggplot(db_resurv %>% filter(!is.na(EUNISa_1_descr)), aes(EUNISa_1_descr)) +
         geom_bar(aes(y = (..count..) / sum(..count..) * 100)) +
  labs(y = "Percentage of ReSurvey observations",
       x = "EUNIS level 1") + coord_flip()


## -------------------------------------------------------------------------------
descriptions<-read_excel(here("data", "edited",
                                 "EUNIS-Habitats-2021-06-01_modified.xlsx"))


## -------------------------------------------------------------------------------
# Define the columns and corresponding description column names
eunis_cols <- c("EUNISa_2", "EUNISa_3", "EUNISa_4",
                "EUNISb_2", "EUNISb_3", "EUNISb_4", 
                "EUNISc_2", "EUNISc_3", "EUNISc_4",
                "EUNISd_2", "EUNISd_3", "EUNISd_4")

# Create corresponding description column names
descr_col_names <- paste0(eunis_cols, "_descr")

# Use reduce to loop through the columns and join dynamically based on level
db_resurv <- reduce(seq_along(eunis_cols), function(data, i) {
  # Extract level number from the column name (e.g., EUNISa_2 -> 2)
  level <- as.numeric(gsub("\\D", "", eunis_cols[i]))
  
  # Filter descriptions for the corresponding level
  descriptions_level <- descriptions %>%
    filter(level == level) %>%
    select(`EUNIS 2020 code`, `EUNIS-2021 habitat name`)
  
  # Perform the left_join and rename the column dynamically
  data %>%
    left_join(
      descriptions_level,
      by = setNames("EUNIS 2020 code", eunis_cols[i])
    ) %>%
    rename(!!descr_col_names[i] := `EUNIS-2021 habitat name`)
}, .init = db_resurv)


## -------------------------------------------------------------------------------
# Correct EUNISa levels 2-4 descriptions
db_resurv <- db_resurv %>%
  mutate(EUNISa_2_descr = 
           ifelse(!is.na(EUNISa_2_descr), EUNISa_2_descr,
                  case_when(
                    EUNISa_2 == "Pf" ~ "Fresh-water submerged vegetation",
                    EUNISa_2 == "Pj" ~ "Stonewort vegetation",
                    EUNISa_2 == "R4" ~ "Alpine and subalpine grasslands",
                    EUNISa_2 == "Pb" ~ "Calcareous spring and spring brook",
                    EUNISa_2 == "Qb" ~ "Wetlands",
                    EUNISa_2 == "R3" ~ "Seasonally wet and wet grasslands",
                    EUNISa_2 == "Qa" ~ "Mires",
                    EUNISa_2 == "Pa" ~ "Base-poor spring and spring brook",
                    EUNISa_2 == "Ph" ~ "Oligotrophic-water vegetation",
                    EUNISa_2 == "Pg" ~ "Fresh-water nymphaeid vegetation",
                    EUNISa_2 ==
                      "Pd" ~ "Fresh-water small pleustophyte vegetation",
                    EUNISa_2 == "Pc" ~ "Brackish-water vegetation",
                    EUNISa_2 ==
                      "Pe" ~ "Fresh-water large pleustophyte vegetation",
                    EUNISa_2 == "Pi" ~ "Dystrophic-water vegetation",
                    EUNISa_2 == "S1" ~ "Tundra",
                    EUNISa_2 ==
                      "U7" ~ "Unvegetated or sparsely vegetated gravel bars",
                    EUNISa_2 == "Q6" ~ "Periodically exposed shores",
                    TRUE ~ NA_character_)
                  ),
         EUNISa_3_descr = 
           ifelse(!is.na(EUNISa_3_descr), EUNISa_3_descr,
                  case_when(
                    EUNISa_3 =="U71" ~ "Unvegetated or sparsely vegetated gravel bar in montane and alpine regions",
                    EUNISa_3 =="Q61" ~ "Periodically exposed shore with stable, eutrophic sediments with pioneer or ephemeral vegetation",
                    EUNISa_3 =="Q62" ~ "Periodically exposed shore with stable, mesotrophic sediments with pioneer or ephemeral vegetation",
                    TRUE ~ NA_character_
                    ))
         )


## -------------------------------------------------------------------------------
# Correct EUNISb levels 2-4 descriptions
db_resurv <- db_resurv %>%
  mutate(EUNISb_2_descr = 
           ifelse(!is.na(EUNISb_2_descr), EUNISb_2_descr,
                  case_when(
                    EUNISb_2 == "Pj" ~ "Stonewort vegetation",
                    EUNISb_2 == "R4" ~ "Alpine and subalpine grasslands",
                    EUNISb_2 == "Pf" ~ "Fresh-water submerged vegetation",
                    TRUE ~ NA_character_)
                  )
         )


## -------------------------------------------------------------------------------
db_resurv %>% filter(EUNISa_1 == "Q") %>% distinct(EUNISa_2)


## -------------------------------------------------------------------------------
ggplot(db_resurv %>% filter(EUNISa_1 == "MA"), aes(EUNISa_2_descr)) +
         geom_bar(aes(y = (..count..) / sum(..count..) * 100)) +
  labs(y = "Percentage of ReSurvey observations",
       x = "EUNIS level 2") + coord_flip() +
  ggtitle(db_resurv %>% filter(EUNISa_1 == "MA") %>% distinct(EUNISa_1_descr))
ggsave(filename=here("output", "figures","MA_level2.tiff"),
       width=14,height=8,units="cm",dpi=300)
ggplot(db_resurv %>% filter(EUNISa_1 == "N"), aes(EUNISa_2_descr)) +
         geom_bar(aes(y = (..count..) / sum(..count..) * 100)) +
  labs(y = "Percentage of ReSurvey observations",
       x = "EUNIS level 2") + coord_flip() +
  ggtitle(db_resurv %>% filter(EUNISa_1 == "N") %>% distinct(EUNISa_1_descr))
ggsave(filename=here("output", "figures","N_level2.tiff"),
       width=14,height=8,units="cm",dpi=300)
ggplot(db_resurv %>% filter(EUNISa_1 == "P"), aes(EUNISa_2_descr)) +
         geom_bar(aes(y = (..count..) / sum(..count..) * 100)) +
  labs(y = "Percentage of ReSurvey observations",
       x = "EUNIS level 2") + coord_flip() +
  ggtitle(db_resurv %>% filter(EUNISa_1 == "P") %>% distinct(EUNISa_1_descr))
ggsave(filename=here("output", "figures","P_level2.tiff"),
       width=14,height=8,units="cm",dpi=300)
ggplot(db_resurv %>% filter(EUNISa_1 == "Q"), aes(EUNISa_2_descr)) +
         geom_bar(aes(y = (..count..) / sum(..count..) * 100)) +
  labs(y = "Percentage of ReSurvey observations",
       x = "EUNIS level 2") + coord_flip() +
  ggtitle(db_resurv %>% filter(EUNISa_1 == "Q") %>% distinct(EUNISa_1_descr))
ggsave(filename=here("output", "figures","Q_level2.tiff"),
       width=14,height=8,units="cm",dpi=300)
ggplot(db_resurv %>% filter(EUNISa_1 == "R"), aes(EUNISa_2_descr)) +
         geom_bar(aes(y = (..count..) / sum(..count..) * 100)) +
  labs(y = "Percentage of ReSurvey observations",
       x = "EUNIS level 2") + coord_flip() +
  ggtitle(db_resurv %>% filter(EUNISa_1 == "R") %>% distinct(EUNISa_1_descr))
ggsave(filename=here("output", "figures","R_level2.tiff"),
       width=14,height=8,units="cm",dpi=300)
ggplot(db_resurv %>% filter(EUNISa_1 == "S"), aes(EUNISa_2_descr)) +
         geom_bar(aes(y = (..count..) / sum(..count..) * 100)) +
  labs(y = "Percentage of ReSurvey observations",
       x = "EUNIS level 2") + coord_flip() +
  ggtitle(db_resurv %>% filter(EUNISa_1 == "S") %>% distinct(EUNISa_1_descr))
ggsave(filename=here("output", "figures","S_level2.tiff"),
       width=16,height=8,units="cm",dpi=300)
ggplot(db_resurv %>% filter(EUNISa_1 == "T"), aes(EUNISa_2_descr)) +
         geom_bar(aes(y = (..count..) / sum(..count..) * 100)) +
  labs(y = "Percentage of ReSurvey observations",
       x = "EUNIS level 2") + coord_flip() +
  ggtitle(db_resurv %>% filter(EUNISa_1 == "T") %>% distinct(EUNISa_1_descr))
ggsave(filename=here("output", "figures","T_level2.tiff"),
       width=14,height=8,units="cm",dpi=300)
ggplot(db_resurv %>% filter(EUNISa_1 == "U"), aes(EUNISa_2_descr)) +
         geom_bar(aes(y = (..count..) / sum(..count..) * 100)) +
  labs(y = "Percentage of ReSurvey observations",
       x = "EUNIS level 2") + coord_flip() +
  ggtitle(db_resurv %>% filter(EUNISa_1 == "U") %>% distinct(EUNISa_1_descr))
ggsave(filename=here("output", "figures","U_level2.tiff"),
       width=16,height=8,units="cm",dpi=300)
ggplot(db_resurv %>% filter(EUNISa_1 == "V"), aes(EUNISa_2_descr)) +
         geom_bar(aes(y = (..count..) / sum(..count..) * 100)) +
  labs(y = "Percentage of ReSurvey observations",
       x = "EUNIS level 2") + coord_flip() +
  ggtitle(db_resurv %>% filter(EUNISa_1 == "V") %>% distinct(EUNISa_1_descr))
ggsave(filename=here("output", "figures","V_level2.tiff"),
       width=14,height=8,units="cm",dpi=300)


## -------------------------------------------------------------------------------
write_tsv(db_resurv,here("data", "clean","db_resurv_clean.csv"))


## -------------------------------------------------------------------------------
sessionInfo()

