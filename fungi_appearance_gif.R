library(data.table)
library(purrr)
library(plyr)
library(dplyr)
library(devtools)
library(ggplot2)
library(maps)
library(ggthemes)
library(purrr)
library(data.table)
library(tidyr)

install_github("thomasp85/gganimate")

library(gganimate)

###load fungi_data workspace
load("D:/R projects/plant_path_meta/fungi_mapping_data.RData")

locations_concat <- fungi_data %>%
  map(., 4) %>%
  rbindlist() %>%
  select(location, lon, lat, hosts, date) %>%
  arrange(location, date) %>%
  group_by(location, date) %>%
  add_tally() %>%
  distinct(location, date, n, .keep_all = T) %>%
  ungroup() %>%
  mutate_at(5, as.integer)

locations_anim_df <- locations_concat %>%
  tidyr::expand(., date, nesting(location, lon, lat)) %>%
  left_join(., locations_concat, by=c("location", "date")) %>%
  select(date, location, lon.x, lat.x, n) %>%
  replace_na(list(n=0)) %>% 
  group_by(location) %>%
  mutate(n_sum = cumsum(n)) %>%
  arrange(location, date) %>%
  select(-5) %>%
  rename(lon = lon.x, lat = lat.x)

world <- ggplot() +
  borders("world", colour = "gray85", fill = "gray80") +
  theme_map() + 
  geom_point(data = locations_anim_df, aes(x = lon, y = lat, size = ifelse(n_sum==0, NA, n_sum)),
             colour = 'purple', alpha = .5) +
  scale_size_continuous(range = c(0.5, 6), 
                        breaks = c(250, 500, 750, 1000)) +
  labs(title = "Year: {frame_time}", size = 'Documented Pathogens') +
  transition_time(date) +
  ease_aes("linear")

animate(world, nframes = 400, fps = 10, height = 604, width = 1000)
