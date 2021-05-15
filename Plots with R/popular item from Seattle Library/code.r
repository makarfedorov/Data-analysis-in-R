library("tidyverse")
library("readxl")

full_file <- "seattle_public_library_checkouts.xlsx"
d2005 <- read_excel(full_file,sheet = 1)
d2006 <- read_excel(full_file,sheet = 2)
d2007 <- read_excel(full_file,sheet = 3)
d2008 <- read_excel(full_file,sheet = 4)
d2009 <- read_excel(full_file,sheet = 5)
d2010 <- read_excel(full_file,sheet = 6) 
d2011 <- read_excel(full_file,sheet = 7)
d2012 <- read_excel(full_file,sheet = 8)
d2013 <- read_excel(full_file,sheet = 9) 
d2014 <- read_excel(full_file,sheet = 10)
d2015 <- read_excel(full_file,sheet = 11) 
d2016 <- read_excel(full_file,sheet = 12)
d2017 <- read_excel(full_file,sheet = 13) 
d2018 <- read_excel(full_file,sheet = 14) 
d2019 <- read_excel(full_file,sheet = 15) 

d2005 %>%
  bind_rows(d2006) %>%
  bind_rows(d2007) %>%
  bind_rows(d2008) %>%
  bind_rows(d2009) %>%
  bind_rows(d2010) %>%
  bind_rows(d2011) %>%
  bind_rows(d2012) %>%
  bind_rows(d2013) %>%
  bind_rows(d2014) %>%
  bind_rows(d2015) %>%
  bind_rows(d2016) %>%
  bind_rows(d2017) %>%
  bind_rows(d2018) %>%
  bind_rows(d2019) %>%
  group_by(name,type) %>%
  summarise(n = sum(n)) %>%
  arrange(desc(n)) %>%
  ungroup() %>%
  slice(1:100) %>%
  mutate(name = fct_reorder(name, n)) %>%
  ggplot(aes(name,n,fill = type))+
  geom_col()+
  coord_flip()+
  labs(x = "",
       y = "",
       title = "Most popular physical item checkouts from Seattle Public Library",
       caption = "data from https://data.seattle.gov/dataset/Checkouts-by-Title-Physical-Items-/3h5r-qv5w")
