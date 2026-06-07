library(tidyverse)

# REMS csv from https://www.kaggle.com/datasets/deepcontractor/mars-rover-environmental-monitoring-station

raw <- read_csv(
  "data/REMS_Mars_Dataset.csv",
  na = "Value not available",
  show_col_types = FALSE
)

clean <- raw %>%
  rename(
    max_ground_temp = `max_ground_temp(°C)`,
    min_ground_temp = `min_ground_temp(°C)`,
    max_air_temp = `max_air_temp(°C)`,
    min_air_temp = `min_air_temp(°C)`,
    pressure = `mean_pressure(Pa)`,
    wind_speed = `wind_speed(m/h)`,
    humidity = `humidity(%)`,
    uv = UV_Radiation
  ) %>%
  mutate(
    earth_date = as.Date(str_extract(earth_date_time, "\\d{4}-\\d{2}-\\d{2}")),
    mars_month = as.integer(str_extract(mars_date_time, "(?<=Month )\\d+")),
    ls = as.numeric(str_extract(mars_date_time, "(?<=LS )\\d+")),
    sol = as.integer(str_extract(sol_number, "\\d+"))
  ) %>%
  select(sol, earth_date, mars_month, ls, max_air_temp, min_air_temp,
         max_ground_temp, min_ground_temp, pressure, wind_speed, humidity,
         uv, weather, sunrise, sunset) %>%
  arrange(sol)

write_csv(clean, "data/curiosity_rems_clean.csv")


# localized_interp.csv from https://an.rsl.wustl.edu/msl/AN/an3.aspx
loc_raw <- read_csv("data/localized_interp.csv", show_col_types = FALSE)

location <- loc_raw %>%
  filter(frame == "ROVER", sol >= 0) %>%
  group_by(sol) %>%
  slice_tail(n = 1) %>%
  ungroup() %>%
  select(sol, easting, northing, elevation,
         latitude = planetodetic_latitude, longitude) %>%
  arrange(sol)

write_csv(location, "data/curiosity_location_clean.csv")
