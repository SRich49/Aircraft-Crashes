# 01_clean.R
# Reads the raw plane crash data, checks data quality,
# and saves a cleaned version for the group's analysis.

# Run this line once if the packages aren't installed yet:
# install.packages(c("tidyverse", "janitor", "here"))

library(tidyverse)
library(janitor)   # cleans up column names
library(here)      # builds file paths that work on everyone's computer

# --- Read the data ---
# Treat blanks and "?" as missing. guess_max = Inf makes readr check
# every row before guessing column types.
raw <- read_csv(here("data", "raw", "planecrashdata.csv"),
                na = c("", "NA", "?", "N/A"),
                guess_max = Inf)

problems(raw)   # lists any rows that didn't parse correctly

# Standardize column names (lowercase, underscores)
crashes <- raw |> clean_names()

# Look at the structure before cleaning further
glimpse(crashes)

# --- Data quality: share of missing values per column ---
crashes |>
  summarise(across(everything(), ~ mean(is.na(.)))) |>
  pivot_longer(everything(), names_to = "column", values_to = "prop_missing") |>
  arrange(desc(prop_missing)) |>
  print(n = Inf)

# --- Consistency checks ---
# Do the fatality columns add up?
crashes |>
  filter(total_fatalities != crew_fatalities + passengers_fatalities + other_fatalities) |>
  nrow()

# Are there crashes with more deaths than people on board? (data errors)
crashes |>
  filter(crew_fatalities + passengers_fatalities > crew_on_board + passengers_on_board) |>
  nrow()

# --- Recode variables for the analysis ---
crashes <- crashes |>
  mutate(
    year      = year(timestamp),
    decade    = floor(year / 10) * 10,
    occupants           = crew_on_board + passengers_on_board,
    occupant_fatalities = crew_fatalities + passengers_fatalities,
    occupant_survivors  = occupants - occupant_fatalities,
    survival_rate       = occupant_survivors / occupants,
    outcome = case_when(
      occupant_fatalities == 0 ~ "No fatalities",
      occupant_survivors  == 0 ~ "No survivors",
      TRUE                     ~ "Some survivors"
    )
  )

# --- Look at the categories before grouping them ---
crashes |> count(flight_phase, sort = TRUE)
crashes |> count(flight_type,  sort = TRUE)
crashes |> count(region,       sort = TRUE)

# --- Drop columns that are empty or not useful ---
crashes <- crashes |>
  select(-survivors, -external_id, -hex_code, -image_url)

# --- Combine categories ---
crashes <- crashes |>
  mutate(
    # Merge duplicate spellings into four main phases
    phase = case_when(
      flight_phase %in% c("Takeoff (climb)", "Take off", "Initial climb") ~ "Takeoff",
      flight_phase %in% c("Landing (descent or approach)", "Landing",
                          "Approach") ~ "Landing",
      flight_phase %in% c("Flight", "En route", "Manoeuvring",
                          "Manoeuvring (airshow, firefighting, ag.ops.)") ~ "Flight",
      flight_phase %in% c("Taxiing", "Taxi", "Parking", "Standing",
                          "Pushback / towing") ~ "Ground",
      TRUE ~ NA_character_
    ),
    phase = factor(phase, levels = c("Ground", "Takeoff", "Flight", "Landing")),
    
    # Group 40 flight types into broader categories
    flight_category = case_when(
      is.na(flight_type) ~ NA_character_,
      flight_type %in% c("Scheduled Revenue Flight", "Passenger - Scheduled",
                         "Passenger") ~ "Scheduled passenger",
      str_detect(flight_type, regex("charter", ignore_case = TRUE)) ~ "Charter",
      flight_type %in% c("Cargo", "Postal (mail)") ~ "Cargo",
      flight_type %in% c("Military", "Bombing", "Survey / Patrol / Reconnaissance",
                         "Aerial patrol") ~ "Military",
      flight_type == "Training" ~ "Training",
      flight_type %in% c("Private", "Executive/Corporate/Business",
                         "Executive") ~ "Private/Business",
      flight_type %in% c("Positioning", "Ferry", "Ferry/positioning",
                         "Delivery") ~ "Ferry/Positioning",
      flight_type %in% c("Spraying (Agricultural)", "Agricultural", "Fire fighting",
                         "Aerial photography", "Geographical / Geophysical / Scientific",
                         "Meteorological / Weather", "Calibration", "Topographic",
                         "Cinematography") ~ "Aerial work",
      TRUE ~ "Other"
    ),
    
    # Combine very small regions and "World" into "Other"
    region_grp = case_when(
      region == "Caribbean" ~ "Central America",
      region %in% c("World", "Antarctica", "Russia & CIS", "Middle East") ~ "Other",
      TRUE ~ region
    ),
    
    # Aircraft age at the time of the crash
    aircraft_age = year - yom,
    aircraft_age = if_else(aircraft_age < 0, NA_real_, aircraft_age)
  )

# Check the groupings: every original category should land somewhere sensible
crashes |> count(flight_category, flight_type, sort = TRUE) |> print(n = Inf)

# --- Flag rows that can be used for survival analysis ---
crashes <- crashes |>
  mutate(
    valid_survival = !is.na(occupants) & occupants > 0 &
      !is.na(occupant_fatalities) &
      occupant_fatalities <= occupants
  )

crashes |> count(valid_survival)

# Is missing occupant data concentrated in certain decades?
crashes |>
  group_by(decade) |>
  summarise(n = n(), prop_invalid = mean(!valid_survival)) |>
  print(n = Inf)

# Is missing occupant data concentrated in certain flight types?
crashes |>
  count(flight_category, valid_survival) |>
  group_by(flight_category) |>
  mutate(prop = n / sum(n)) |>
  filter(!valid_survival) |>
  arrange(desc(prop))

# --- Save the cleaned data ---
dir.create(here("data", "clean"), showWarnings = FALSE)
write_csv(crashes, here("data", "clean", "crashes_clean.csv"))
