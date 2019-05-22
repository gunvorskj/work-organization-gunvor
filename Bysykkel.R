library(tidyverse)

bysykkel <- read.csv('Sykkel.csv',sep = ";")

bysykkeltibble <- as_tibble(bysykkel)

##most popular start station####

bysykkeltibble %>% 
  count(start_station_name) %>% 
  arrange(desc (n)) %>% 
  view

###most popular end station####
bysykkeltibble %>% 
  count(end_station_name) %>% 
  arrange(desc (n)) %>% 
  view


###longest/shortest duration###
bysykkeltibble %>% 
  count(duration) %>% 
  arrange(desc (duration)) %>% 
  view

bysykkeltibble %>% 
  count(duration) %>% 
  arrange( (duration)) %>% 
  view

###most popular pair of start and end station###
bysykkeltibble %>% 
  count(start_station_name, end_station_name) %>% 
  arrange(desc(n)) %>% 
  view()

###plot the number of hires and returns###
bysykkeltibble %>% 
  gather(key = key, value = value, start_station_name, end_station_name) %>% 
  count(key, value) %>% 
  ggplot (aes(x = value, y = n, fill = key)) + geom_col(position=position_dodge())


###plot the distrubution of hire duration##
bysykkeltibble %>% 
 ggplot(aes(x = duration)) + geom_histogram() + xlim(10, 3000)

###median duration of each station###                                                 
bysykkeltibble %>% 
  group_by(start_station_name) %>%
  summarise(median_duration=median(duration))

###map this information###
bysykkeltibble %>% 
  filter(start_station_longitude<10) %>% 
  group_by(start_station_name, start_station_latitude, start_station_longitude) %>% 
  summarise(meddur = median(duration)) %>% 
  ggplot(aes(x = start_station_longitude, y = start_station_latitude, size = meddur)) + geom_point()


