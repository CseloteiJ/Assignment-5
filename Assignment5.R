library("tidyverse")
library("ggplot2")
library("ggmap")
library("maps")
library("mapdata")
library("devtools")
library("dplyr")
library("stringr")

philip = preproc %>%
  filter(preproc$country %in% c('Philippines'))
#filtered all the data that is only for the Philippines from the csv file

philip$date_start[0]
philip$date_end[0]
#checks how dates are formatted - poorly, so i'm changing them with:
philip$date_start <- as.Date(philip$date_start, '%Y-%m-%d')
philip$date_end <- as.Date(philip$date_end, '%Y-%m-%d')
#changes dates from factor to numerical

statebased = philip %>%
  filter(philip$type_of_violence %in% c('1'))
#select type of conflict to only show type 1, meaning state-based violence

early = statebased %>%
  filter(statebased$year %in% c('1991'))

late = statebased %>%
  filter(statebased$year %in% c('2015'))
#selected earliest and latest years, just to have a rough idea of the grand change in state-based violence

earlydeaths <- sum(early$best)
latedeaths <- sum(late$best)
#calculate aggregate death tolls in the 3 years respectively to be displayed as values

ggplot(data=statebased) +
  geom_line(mapping=aes(x=year, y=best), stat='summary', fun.y=sum) +
  geom_point(mapping = aes(x=year, y=best), stat='summary', fun.y=sum) +
  xlab("Time in years") +
  ylab("Number of deaths (best estimate)") +
  ggtitle("All deaths for each year", subtitle = NULL)
#plots sums of deaths for each year respectively (and labels title and axes)

map_philippines <- map_data("world") %>%
  filter(region == "Philippines")
#getting the map of the Philippines from the world data

ggplot(data = early) +
  geom_polygon(data = map_philippines, mapping = aes(x = long, y = lat, group = group), fill = NA, color = "black") + 
  coord_fixed(1) +
  geom_point(mapping = aes(x = longitude, y = latitude, color = best, size = best)) +
  labs(x = "longitude", y = "latitude", size = "best") +
  ggtitle("Deaths in 1991", subtitle = NULL)
#plots map of Philippines showing deaths in 1991, with scale and color

ggplot(data = late) +
  geom_polygon(data = map_philippines, mapping = aes(x = long, y = lat, group = group), fill = NA, color = "black") + 
  coord_fixed(1) +
  geom_point(mapping = aes(x = longitude, y = latitude, color = best, size = best)) +
  labs(x = "longitude", y = "latitude", size = "best") +
  ggtitle("Deaths in 2015", subtitle = NULL)
#plots map of Philippines showing deaths in 2015, with scale and color