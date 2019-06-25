library(tidyverse)
#install.packages("lubridate")
library(lubridate)
library(maps)
library(gganimate)
library(ggsci)
library(extrafontdb)
library(extrafont)
#font_import()

loadfonts(device = "win", quiet = TRUE)


ufo_sightings <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-06-25/ufo_sightings.csv")


ufo_sightings <- ufo_sightings %>% mutate(date_mdy=mdy_hm(date_time)) %>%
  mutate(year_sighting=year(date_mdy)) %>%
  mutate (encounter_length_minutes=encounter_length/60) %>%
  mutate(encounter_length_categories=ifelse(encounter_length_minutes<5, "Shorter than 5 minutes", 
                                            ifelse(encounter_length_minutes<10, "5 to 10 minutes",
                                                   "Longer than 10 minutes"))) %>% 
  filter(!is.na(encounter_length_categories))

p <- ggplot() + 
  borders("world", alpha=0.8, fill="white") +
  geom_point(data = ufo_sightings, aes(x = longitude, y = latitude, 
                                       col=encounter_length_categories)) +
  theme_dark() + 
  scale_color_rickandmorty(name="Length encounter", 
                           limits=c("Shorter than 5 minutes", 
                                    "5 to 10 minutes",
                                    "Longer than 10 minutes")) + 
  labs(title="UFO sightings since 1906",
       x="",
       y="",
       caption="Visualization: @danidlsa | Data Source: NUFORC") +
  theme(title=element_text(size=14, face="bold", family="Courier New"),
        legend.text=element_text(size=12, family="Courier New"),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        legend.position="bottom")


p_anim <- p + transition_time(round(year_sighting)) +
  shadow_mark(past = TRUE) +
  enter_fade() +
  exit_shrink() +
  ease_aes("linear") +
  shadow_mark(alpha = 0.6) + 
  labs(subtitle="Year={round(frame_time)}")
  

anim_save("UFO sightings.gif", p_anim, fps = 10, type = "cairo", width = 800, height = 500)


