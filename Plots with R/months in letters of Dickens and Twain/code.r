library(gutenbergr)
library(tidytext)
letters <- gutenberg_download(c(3199,25852,25853, 25854), mirror = "http://mirrors.xmission.com/gutenberg/")
letters %>%
  slice(667:79017) -> letters2
letters2 %>%
  unnest_tokens(output = "word", input = text,to_lower = FALSE) %>%
  mutate(word = str_remove(word,"_")) %>%
  filter(word %in% month.name) %>%
  group_by(gutenberg_id) %>%
  mutate(author = if (gutenberg_id == 3199) {author = "Twain, Mark"} 
         else {author = "Dickens, Charles"}) %>%
  group_by(author) %>%
  count(word) %>% 
  group_by(word) %>%
  mutate(season = if(word %in% c("January","February","December")) {season = "1"}
         else if (word %in% c("March","April","May")) {season = "2"} 
         else if (word %in% c("June","July", "August")) {season = "3"} 
         else {season = "4"}) %>%
  mutate(f = which(month.name == word)) -> letters3
letters3 %>%
  View()
letters3 %>% 
  ggplot(aes(n,fct_reorder(word,f, .desc = TRUE), fill = season))+
  geom_col()+
  facet_wrap(~author, scales = "free_x")+
  theme(axis.title.y = element_blank(),legend.position = "NULL") +
  labs(title = "Month names in letters", x = "")
