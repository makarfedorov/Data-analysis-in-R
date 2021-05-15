library(tidyverse)
m_set <- read_csv("https://raw.githubusercontent.com/the-pudding/data/master/pockets/measurements.csv")  
m_set1 <- m_set %>%
  filter(style != "slim",style != "regular") %>%
  group_by(menWomen,style) %>%
  mutate(xminf = if_else(menWomen == "women",minWidthBack,-(minWidthBack)),
         xmaxf = if_else(menWomen == "women",minWidthFront,-(maxWidthFront)),
         yminf = - (minHeightBack),
         ymaxf = minHeightFront,
         n = "min")  
m_set2 <- m_set %>%           
  filter(style != "slim",style != "regular") %>%
  group_by(menWomen,style) %>%
  mutate(xminf = if_else(menWomen == "women",maxWidthBack,-(maxWidthBack)),
         xmaxf = if_else(menWomen == "women",maxWidthFront,-(maxWidthFront)),
         yminf = - (maxHeightBack),
         ymaxf = maxHeightFront,
         n = "max")
m_set3 <- m_set1 %>% 
  bind_rows(m_set2)
m_set3 %>% 
  group_by(style,n) %>%
  ggplot(aes(xmin = xmaxf,xmax = 0, ymin = yminf, ymax = ymaxf, color = menWomen))+
  scale_color_manual(values=c("orange", "darkgreen"))+
  geom_rect(fill = NA)+
  coord_equal() +
  geom_hline(yintercept = 0)+
  geom_vline(xintercept = 0)+
  #scale_x_discrete(breaks = waiver(),labels = labelsx)+
  scale_x_continuous(breaks = c(20,10,0,-10,-20), label = c("20","10","0","10","20"))+
  scale_y_continuous(breaks = c(30,20,10,0,-10,-20,-30),label = c("","20","10","0","10","20",""))+
  #facet_wrap(~n+style,nrow = 2,ncol = 3)
  facet_grid(n~style,as.table = TRUE)+
  labs(x = "width",
       y = "height",
       title = "Sizes of pockets",
       subtitle = "top quadrants are for front pockets,bottom quadrants are for
       baclk pockets,right quadrants are for women's cloth,left
       quadrants are for men's cloth",
       caption = "data from:https://pudding.cool/2018/08/pockets/")+
  theme(legend.position = "bottom",legend.title = element_blank())
  
