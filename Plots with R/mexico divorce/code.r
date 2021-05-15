
library(tidyverse)
library(lubridate)
dv <- read_csv("mexico_divorces_2000-2015.csv")
dv %>%
  group_by(DOB_partner_man) %>%
  rename(dob = DOB_partner_man) %>%
  mutate(horoscope_sign_m = case_when(
    dob %within% interval(make_date(year = year(dob), month = 1, day = 20),
                          make_date(year = year(dob), month = 2, day = 18)) ~ "Aquarius",
    dob %within% interval(make_date(year = year(dob), month = 2, day = 19),
                          make_date(year = year(dob), month = 3, day = 20)) ~ "Pisces",
    dob %within% interval(make_date(year = year(dob), month = 3, day = 21),
                          make_date(year = year(dob), month = 4, day = 19)) ~ "Aries",
    dob %within% interval(make_date(year = year(dob), month = 4, day = 20),
                          make_date(year = year(dob), month = 5, day = 20)) ~ "Taurus",
    dob %within% interval(make_date(year = year(dob), month = 5, day = 21),
                          make_date(year = year(dob), month = 6, day = 20)) ~ "Gemini",
    dob %within% interval(make_date(year = year(dob), month = 6, day = 21),
                          make_date(year = year(dob), month = 7, day = 22)) ~ "Cancer",
    dob %within% interval(make_date(year = year(dob), month = 7, day = 23),
                          make_date(year = year(dob), month = 8, day = 22)) ~ "Leo",
    dob %within% interval(make_date(year = year(dob), month = 8, day = 23),
                          make_date(year = year(dob), month = 9, day = 22)) ~ "Virgo",
    dob %within% interval(make_date(year = year(dob), month = 9, day = 23),
                          make_date(year = year(dob), month = 10, day = 22)) ~ "Libra",
    dob %within% interval(make_date(year = year(dob), month = 10, day = 23),
                          make_date(year = year(dob), month = 11, day = 22)) ~ "Scorpio",
    dob %within% interval(make_date(year = year(dob), month = 11, day = 23),
                          make_date(year = year(dob), month = 12, day = 21)) ~ "Sagittarius",
    dob %within% interval(make_date(year = year(dob), month = 12, day = 22),
                          make_date(year = year(dob), month = 12, day = 31)) ~ "Capricorn",
    dob %within% interval(make_date(year = year(dob), month = 1, day = 1),
                          make_date(year = year(dob), month = 1, day = 19)) ~ "Capricorn")) %>%
  group_by(DOB_partner_woman) %>%
  rename(dob_2 = DOB_partner_woman) %>%
  mutate(horoscope_sign_f = case_when(
    dob_2 %within% interval(make_date(year = year(dob_2), month = 1, day = 20),
                            make_date(year = year(dob_2), month = 2, day = 18)) ~ "Aquarius",
    dob_2 %within% interval(make_date(year = year(dob_2), month = 2, day = 19),
                            make_date(year = year(dob_2), month = 3, day = 20)) ~ "Pisces",
    dob_2 %within% interval(make_date(year = year(dob_2), month = 3, day = 21),
                            make_date(year = year(dob_2), month = 4, day = 19)) ~ "Aries",
    dob_2 %within% interval(make_date(year = year(dob_2), month = 4, day = 20),
                            make_date(year = year(dob_2), month = 5, day = 20)) ~ "Taurus",
    dob_2 %within% interval(make_date(year = year(dob_2), month = 5, day = 21),
                            make_date(year = year(dob_2), month = 6, day = 20)) ~ "Gemini",
    dob_2 %within% interval(make_date(year = year(dob_2), month = 6, day = 21),
                            make_date(year = year(dob_2), month = 7, day = 22)) ~ "Cancer",
    dob_2 %within% interval(make_date(year = year(dob_2), month = 7, day = 23),
                            make_date(year = year(dob_2), month = 8, day = 22)) ~ "Leo",
    dob_2 %within% interval(make_date(year = year(dob_2), month = 8, day = 23),
                            make_date(year = year(dob_2), month = 9, day = 22)) ~ "Virgo",
    dob_2 %within% interval(make_date(year = year(dob_2), month = 9, day = 23),
                            make_date(year = year(dob_2), month = 10, day = 22)) ~ "Libra",
    dob_2 %within% interval(make_date(year = year(dob_2), month = 10, day = 23),
                            make_date(year = year(dob_2), month = 11, day = 22)) ~ "Scorpio",
    dob_2 %within% interval(make_date(year = year(dob_2), month = 11, day = 23),
                            make_date(year = year(dob_2), month = 12, day = 21)) ~ "Sagittarius",
    dob_2 %within% interval(make_date(year = year(dob_2), month = 12, day = 22),
                            make_date(year = year(dob_2), month = 12, day = 31)) ~ "Capricorn",
    dob_2 %within% interval(make_date(year = year(dob_2), month = 1, day = 1),
                            make_date(year = year(dob_2), month = 1, day = 19)) ~ "Capricorn")) -> dv_2
dv_2 %>%
  group_by(Divorce_date, Date_of_marriage) %>%
  mutate(m_duration = difftime(dmy(Divorce_date), dmy(Date_of_marriage), units = "days")) -> dr_3
horoscope_sign <- c("Aquarius",
                     "Pisces",
                     "Aries",
                     "Taurus",
                     "Gemini",
                     "Cancer",
                     "Leo",
                     "Virgo",
                     "Libra",
                     "Scorpio",
                     "Sagittarius",
                     "Capricorn")
  
dr_3 %>%
  mutate(m_duration = as.numeric(m_duration)) %>%
  group_by(horoscope_sign_f,horoscope_sign_m) %>%
  mutate(mean = mean(c(m_duration))) -> dr_4 
dr_4 %>%
group_by(m_duration) %>%
  ggplot(aes(m_duration))+
  geom_histogram()+
  facet_grid(cols = vars(factor(horoscope_sign_f, levels = horoscope_sign)),
             
             rows = vars(factor(horoscope_sign_m, levels = horoscope_sign
                                
             )))+
  geom_vline(data = dr_4 , aes(xintercept = mean,color = "red"))+
  theme(axis.title.y = element_blank(),legend.position = "NULL") +
  labs(title = 'Horoscope sign of man (horizontal) and woman (vertical)',
       subtitle = 'Red line shows the mean average marriage duration',
       caption = 'Data from Mexican government',
       x = 'Marriage duration (days)')
