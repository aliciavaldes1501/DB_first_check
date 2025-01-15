## -------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = FALSE)


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
printall <- function(tibble) {
  print(tibble, width = Inf)
  }


## -------------------------------------------------------------------------------
db_resurv_clean<-read_tsv(
  here("data", "clean", "db_resurv_clean.csv"),
  col_types = cols(
    # Dynamically specify EUNIS columns as character
    .default = col_guess(),  # Default guessing for other columns
    EUNISa = col_character(),
    EUNISb = col_character(),
    EUNISc = col_character(),
    EUNISd = col_character(),
    EUNISa_1 = col_character(),
    EUNISa_2 = col_character(),
    EUNISa_3 = col_character(),
    EUNISa_4 = col_character(),
    EUNISb_1 = col_character(),
    EUNISb_2 = col_character(),
    EUNISb_3 = col_character(),
    EUNISb_4 = col_character(),
    EUNISc_1 = col_character(),
    EUNISc_2 = col_character(),
    EUNISc_3 = col_character(),
    EUNISc_4 = col_character(),
    EUNISd_1 = col_character(),
    EUNISd_2 = col_character(),
    EUNISd_3 = col_character(),
    EUNISd_4 = col_character(),
    EUNISa_1_descr = col_character(),
    EUNISc_1_descr = col_character(),
    EUNISd_1_descr = col_character(),
    EUNIS_assignation = col_character(),
    EUNISa_2_descr = col_character(),
    EUNISa_3_descr = col_character(),
    EUNISa_4_descr = col_character(),
    EUNISb_2_descr = col_character(),
    EUNISb_3_descr = col_character(),
    EUNISb_4_descr = col_character(),
    EUNISc_2_descr = col_character(),
    EUNISc_3_descr = col_character(),
    EUNISc_4_descr = col_character(),
    EUNISd_2_descr = col_character(),
    EUNISd_3_descr = col_character(),
    EUNISd_4_descr = col_character()
    )
  )
db_EVA_clean<-read_tsv(
  here("data", "clean", "db_EVA_clean.csv"),
  col_types = cols(
    # Dynamically specify EUNIS columns as character
    .default = col_guess(),  # Default guessing for other columns
    EUNISa = col_character(),
    EUNISb = col_character(),
    EUNISc = col_character(),
    EUNISd = col_character(),
    EUNISa_1 = col_character(),
    EUNISa_2 = col_character(),
    EUNISa_3 = col_character(),
    EUNISa_4 = col_character(),
    EUNISb_1 = col_character(),
    EUNISb_2 = col_character(),
    EUNISb_3 = col_character(),
    EUNISb_4 = col_character(),
    EUNISc_1 = col_character(),
    EUNISc_2 = col_character(),
    EUNISc_3 = col_character(),
    EUNISc_4 = col_character(),
    EUNISd_1 = col_character(),
    EUNISd_2 = col_character(),
    EUNISd_3 = col_character(),
    EUNISd_4 = col_character(),
    EUNISa_1_descr = col_character(),
    EUNIS_assignation = col_character(),
    EUNISa_2_descr = col_character(),
    EUNISa_3_descr = col_character(),
    EUNISa_4_descr = col_character(),
    EUNISb_2_descr = col_character(),
    EUNISb_3_descr = col_character(),
    EUNISb_4_descr = col_character(),
    EUNISc_2_descr = col_character(),
    EUNISc_3_descr = col_character(),
    EUNISc_4_descr = col_character(),
    EUNISd_2_descr = col_character(),
    EUNISd_3_descr = col_character(),
    EUNISd_4_descr = col_character()
  )
)


## -------------------------------------------------------------------------------
db_resurv_clean <- db_resurv_clean %>%
  select(PlotObservationID, Country, Altitude, `Aspect (°)`, `Slope (°)`,
         starts_with("EUNIS"), `ReSurvey project`, `ReSurvey site`,
         `ReSurvey plot`, `ReSurvey observation`, `Manipulate (y/n)`,
         `Type of manipulation`, `Location method`, RS_CODE, RS_PROJTYP, 
         RS_DUPL, `Location uncertainty (m)`, Dataset, precision_new,
         Lon_updated, Lat_updated, coordinates_equal, coordinates_consistent,
         n_EUNIS, date, year, EUNIS_assignation) %>%
  mutate(database = "EVA_RS")


## -------------------------------------------------------------------------------
db_EVA_clean <- db_EVA_clean %>%
  select(PlotObservationID, Country, Altitude, `Aspect (°)`, `Slope (°)`,
         starts_with("EUNIS"), `ReSurvey project`, `ReSurvey site`,
         `ReSurvey plot`, `ReSurvey observation`, `Manipulate (y/n)`,
         `Type of manipulation`, `Location method`, RS_CODE, RS_PROJTYP, 
         RS_DUPL, `Location uncertainty (m)`, Dataset, precision_new,
         Lon_updated, Lat_updated, # coordinates_equal, coordinates_consistent,
         n_EUNIS, date, year, EUNIS_assignation) %>%
  mutate(database = "EVA") %>%
  mutate(coordinates_equal = NA, coordinates_consistent = NA)


## -------------------------------------------------------------------------------
db_resurv_and_EVA <- bind_rows(db_resurv_clean, db_EVA_clean)


## -------------------------------------------------------------------------------
ggplot(db_resurv_and_EVA %>% 
         # Create column to show availability of EUNIS level 1 (Y/N)
         mutate(EUNIS_1_available = ifelse(is.na(EUNISa_1), "N", "Y")), 
       aes(EUNIS_1_available, fill = database)) +
  geom_bar() +
  labs(y = "Number of observations") +
  ggtitle("Availability of EUNIS level 1 in EVA + EVA ReSurvey")


## -------------------------------------------------------------------------------
ggplot(db_resurv_and_EVA, aes(EUNIS_assignation, fill = database)) +
  geom_bar() +
  labs(y = "Number of observations") +
  ggtitle("EUNIS assignation")


## -------------------------------------------------------------------------------
ggplot(db_resurv_and_EVA %>% 
         # Create column to show availability of coordinates
         mutate(coords_available = 
                  ifelse(is.na(Lon_updated) & is.na(Lat_updated), "N", "Y")),
       aes(coords_available, fill = database)) +
  geom_bar() +
  labs(y = "Number of observations") +
  ggtitle("Availability of coordinates")


## -------------------------------------------------------------------------------
ggplot(db_resurv_and_EVA %>% 
         # Create column to show availability of location uncertainty
         mutate(loc_uncertainty_available = 
                  ifelse(is.na(`Location uncertainty (m)`), "N", "Y")),
       aes(loc_uncertainty_available, fill = database)) +
  geom_bar() +
  labs(y = "Number of observations") +
  ggtitle("Availability of location uncertainty")


## -------------------------------------------------------------------------------
ggplot(db_resurv_and_EVA,
       aes(`Location uncertainty (m)`, fill = database)) +
  geom_histogram(aes(y = (..count..) / sum(..count..) * 100),
                 color = "black", binwidth = 10000) +
  xlab("Location uncertainty (m)") + ylab("% of observations") +
  ggtitle("Histogram with % of observations")


## -------------------------------------------------------------------------------
ggplot(db_resurv_and_EVA %>% 
         # Create column to show two categories for location uncertainty
         mutate(
           Location_uncertainty_range = case_when(
             `Location uncertainty (m)` <= 200 ~ "0–0.2 km",
             `Location uncertainty (m)` <= 500 ~ "0.2–0.5 km",
             `Location uncertainty (m)` <= 10000 ~ "0.5-10 km",
             is.na(`Location uncertainty (m)`) ~ "NA",
             TRUE ~ "10 km +"
             )
           ),
       aes(Location_uncertainty_range, fill = database)) +
  geom_bar(aes(y = (..count..) / sum(..count..) * 100)) +
  labs(y = "% of observations") +
  ggtitle("Range of location uncertainty")


## -------------------------------------------------------------------------------
ggplot(db_resurv_and_EVA %>%
         filter(`Location uncertainty (m)` < 10000),
       aes(`Location uncertainty (m)`, fill = database)) +
  geom_histogram(aes(y = (..count..) / sum(..count..) * 100), color = "black") +
  xlab("Location uncertainty (m)") + ylab("% of observations") +
  ggtitle("Histogram with % of observations (Loc. uncertainty < 10 km)")


## -------------------------------------------------------------------------------
ggplot(db_resurv_and_EVA %>%
         filter(`Location uncertainty (m)` < 500),
       aes(`Location uncertainty (m)`, fill = database)) +
  geom_histogram(aes(y = (..count..) / sum(..count..) * 100), color = "black") +
  xlab("Location uncertainty (m)") + ylab("% of observations") +
  ggtitle("Histogram with % of observations (Loc. uncertainty < 0.5 km)")


## -------------------------------------------------------------------------------
db_resurv_last <- db_resurv_and_EVA %>%
  filter(database == "EVA_RS") %>% # Keep only EVA ReSurvey
  # So far remove cases where ReSurvey plot is NA
  filter(!is.na(`ReSurvey plot`)) %>%
  # Define a group for each ReSurvey plot
  group_by(RS_CODE, `ReSurvey site`, `ReSurvey plot`) %>%
  # For each ReSurvey plot, keep only the last year
  slice_max(order_by = year, n = 1) %>%
  ungroup()  # Ungroup to return a regular tibble


## -------------------------------------------------------------------------------
db_resurv_mult_obs_per_year <- db_resurv_and_EVA %>%
  filter(database == "EVA_RS") %>%
  filter(!is.na(`ReSurvey plot`)) %>%
  group_by(RS_CODE, `ReSurvey site`, `ReSurvey plot`, `year`) %>%
  summarise(count = n()) %>%
  filter(count > 1) %>%
  ungroup() %>%
  left_join(db_resurv_and_EVA, 
             by = c("RS_CODE", "ReSurvey site", "ReSurvey plot", "year")) 


## -------------------------------------------------------------------------------
write_tsv(db_resurv_mult_obs_per_year %>%
            select(RS_CODE, `ReSurvey site`, `ReSurvey plot`,
                   `ReSurvey observation`, date, year, count,
                   EUNIS_assignation, EUNISa, EUNISb, EUNISc, EUNISd,
                   Lon_updated, Lat_updated, `Location uncertainty (m)`),
          here("data", "edited","db_resurv_mult_obs_per_year.csv"))


## -------------------------------------------------------------------------------
write_tsv(db_resurv_mult_obs_per_year,
          here("data", "edited","db_resurv_mult_obs_per_year_Ilona.csv"))


## -------------------------------------------------------------------------------
db_resurv_and_EVA_good <- db_resurv_and_EVA %>%
  filter(EUNIS_assignation != "Not possible" &
           `Location uncertainty (m)` < 500) 


## -------------------------------------------------------------------------------
nrow(db_resurv_and_EVA_good) / nrow(db_resurv_and_EVA)


## -------------------------------------------------------------------------------
nrow(db_resurv_and_EVA %>% filter(EUNIS_assignation != "Not possible")) /
  nrow(db_resurv_and_EVA)


## -------------------------------------------------------------------------------
db_resurv_mult_obs_per_year_good <- db_resurv_and_EVA_good %>%
  filter(database == "EVA_RS") %>%
  filter(!is.na(`ReSurvey plot`)) %>%
  group_by(RS_CODE, `ReSurvey site`, `ReSurvey plot`, `year`) %>%
  summarise(count = n()) %>%
  filter(count > 1) %>%
  ungroup() %>%
  # Add the column ReSurvey observation and EUNIS columns
  # to see how to proceed
  left_join(db_resurv_and_EVA, 
             by = c("RS_CODE", "ReSurvey site", "ReSurvey plot", "year")) %>%
  select(RS_CODE, `ReSurvey site`, `ReSurvey plot`,
         `ReSurvey observation`, date, year, count,
         EUNIS_assignation, EUNISa, EUNISb, EUNISc, EUNISd,
         Lon_updated, Lat_updated, `Location uncertainty (m)`)


## -------------------------------------------------------------------------------
db_resurv_and_EVA_good_no_multiples <- db_resurv_and_EVA_good %>%
  # Get the rows in db_resurv_and_EVA_good that do not have matching rows in
  # db_resurv_mult_obs_per_year_good based on the specified columns
  # (i.e. the rows that do not have multiple records for the same year)
  anti_join(
    db_resurv_mult_obs_per_year_good %>%
      select(RS_CODE, `ReSurvey site`, `ReSurvey plot`) %>%
      distinct(),
    by = c("RS_CODE", "ReSurvey site", "ReSurvey plot")
  )


## -------------------------------------------------------------------------------
nrow(db_resurv_and_EVA_good_no_multiples) / nrow(db_resurv_and_EVA)


## -------------------------------------------------------------------------------
sessionInfo()

