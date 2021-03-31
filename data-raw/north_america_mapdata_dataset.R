## code to prepare `north_america` dataset goes here

library(tidyverse)
library(broom)

# Get the map data for canada
canada <- rnaturalearth::ne_states(country = "canada") %>%
  tidy(x = ., region = "name_en") %>%
  mutate(group = as.numeric(as.factor(group)))

# Download map data for US by county
usa_county <- map_data(map = "county")
# Download state data
usa_state <- map_data(map = "state")

# Adjust the groups in the states
usa_state <- usa_state %>%
  mutate(group = group + max(canada$group))

# Adjust the groups in the counties
usa_county <- usa_county %>%
  mutate(group = group + max(usa_state$group))

# Tidy and combine
north_america_mapdata <- bind_rows(usa_state, usa_county, canada)


usethis::use_data(north_america_mapdata, overwrite = TRUE)
