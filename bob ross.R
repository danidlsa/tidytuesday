library(tidyverse)
install.packages("janitor")
library(janitor)
library(ggforce)
library(viridis)
library(extrafontdb)
library(extrafont)

windowsFonts(sans="Segoe UI")
loadfonts(device="win")
loadfonts(device="postscript")


bob_ross <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-08-06/bob-ross.csv")

t <- bob_ross %>% 
  janitor::clean_names() %>% 
  separate(episode, into = c("season", "episode"), sep = "E") %>% 
  mutate(season = str_extract(season, "[:digit:]+")) %>% 
  mutate_at(vars(season, episode), as.integer) %>% 
  select(-episode, -title) %>%
  gather(-season, key = "element", value = "count") %>%
  mutate(element = case_when(.$element %in% c("lake", "lakes") ~ "lake",
                             TRUE ~ as.character(.$element))) %>% 
  mutate(element = case_when(.$element %in% c("mountain", "mountains") ~ "mountain",
                             TRUE ~ as.character(.$element))) %>% 
  mutate(element = case_when(.$element %in% c("tree", "trees") ~ "tree",
                             TRUE ~ as.character(.$element)))

t <- aggregate(count~season+element, t, sum)
t <- subset(t, count>1)


p <- ggplot(t, aes(x=element, y=count, color=season)) + geom_jitter(size=2, alpha = 0.6, width = 0.15) +
  coord_flip() +
  scale_color_viridis("Season", option="magma", limits=c(0,31)) + geom_boxplot(alpha=0, color="black") +
  labs(x="Element", y="Count", title="Bob Ross' Paintings", subtitle="Elements that appear more than one time per season",
       caption="Visualization:@danidlsa") +
  theme_light() +
  theme(title=element_text(face="bold"))
p

ggsave("bob ross.png", plot = p, width = 13, height =16, units = "cm")
