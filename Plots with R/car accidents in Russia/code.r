library(tidyverse)
library(leaflet)
ac <- read_csv("https://raw.githubusercontent.com/agricolamz/DS_for_DH/master/data/MVC_Russia.csv")
ac %>%
  mutate(longitude = ifelse(longitude < 0,longitude + 360,longitude)) %>%
  leaflet() %>%
  addTiles() %>% 
  addCircles(lng = ~longitude,
             lat = ~latitude)
             
