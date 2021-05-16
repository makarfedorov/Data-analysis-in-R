# Data from: https://www.kaggle.com/nehalbirla/vehicle-dataset-from-cardekho
library(tidyverse)
cars <- read_csv("CAR DETAILS FROM CAR DEKHO.csv")
cars %>%
  mutate(lg_km = log(km_driven, base = 10)) %>%
  ggplot(aes(x = lg_km, y = selling_price))+
  geom_point()+
  geom_smooth(method = "lm")
