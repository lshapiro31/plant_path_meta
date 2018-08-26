library(data.table)
library(purrr)
library(plyr)
library(dplyr)

###load fungi_data workspace

locations_concat <- fungi_data %>%
  map(., 4) %>%
  rbindlist() %>%
  select(location, lon, lat, hosts, date) %>%
  arrange(location, date) %>%
  group_by(location, date) %>%
  add_tally() %>%
  distinct(location, date, n, .keep_all = T)