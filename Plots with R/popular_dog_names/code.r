library("tidyverse")

dog_names <- read_csv("https://raw.githubusercontent.com/r-classes/2019_2020_ds4dh_hw_2_dplyr_tidyr_ggplot2/master/data/dog_names.csv")

dog_names %>% 
  filter(sex != "Unknown") %>% 
  group_by(sex) %>%
  count(name) %>%
  top_n(20) %>% 
  mutate(name = fct_reorder(name, n)) %>% 
  ggplot(aes(n,name, fill = sex))+
  geom_col()+
  facet_wrap(~sex,scales = "free_y")+
  labs(x = "count",
       y = "",
       caption = "data from https://pudding.cool/2019/10/shelters/)")+
  scale_fill_brewer(palette="Dark2")
