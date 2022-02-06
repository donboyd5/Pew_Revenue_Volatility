
# prepare a data frame with lat long for the states + DC,
# with AK and HI as insets

# new -----
library(tidyverse)
library(stringr)
#  get the lat long file from someone else who has done it already 
# https://github.com/wmurphyrd/fiftystater
devtools::install_github("wmurphyrd/fiftystater")
library(fiftystater)

# take the file he created and put stabbr (state abbreviation) on it,
# for easy linkage to other data
glimpse(fifty_states)
count(fifty_states, id)

# create a data frame with state names and abbreviations
stdf <- tibble(id=c(state.name, "district of columbia") %>% str_to_lower(),
               stabbr=c(state.abb, "DC"))
stdf

states51 <- fifty_states %>%
  left_join(stdf, by="id") %>%
  as_tibble()
ht(states51)
count(states51, id, stabbr)

# do the same with the usmap file
# which only has 48 states + DC but has more records ??
states49 <- map_data("state") %>%
  left_join(stdf %>% rename(region=id), by="region") %>%
  as_tibble()
count(states49, region, stabbr)  

# make sure they look ok
states49 %>%
  ggplot(aes(x = long, y = lat, group = group, fill = stabbr)) +
  geom_polygon(color = "gray90", size = 0.1) + 
  guides(fill = "none") +
  geom_polygon(color = "gray90", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  guides(fill = "none")

states51 %>%
  ggplot(aes(x = long, y = lat, group = group, fill = stabbr)) +
  geom_polygon(color = "gray90", size = 0.1) + 
  guides(fill = "none") +
  geom_polygon(color = "gray90", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  guides(fill = "none")

save(states49, states51, file = here::here("data", "bmaps.RData"))


