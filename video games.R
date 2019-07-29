#devtools::install_github("moldach/vapoRwave")
video_games <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-07-30/video_games.csv")

library(tidyverse)
library(vapoRwave)
library(lubridate)
library(extrafont)
library(ggrepel)
#font_import()
loadfonts(device = "win", quiet = TRUE)

video_games <- video_games %>% mutate(date=mdy(release_date)) %>%
  mutate(year=year(date))

g1 <- ggplot(video_games, aes(x=metascore, y=average_playtime)) +
  geom_point(col="#79ADDC", size=3, alpha=.8) + 
  new_retro() +
  labs(x="MetaScore",
       y="Average Minutes played",
       title="Video Games Data",
       subtitle="Ratings and Average Minutes Played",
       caption="Visualization: @danidlsa | Data: @brightcdns") +
  theme(axis.title.y=element_text(angle=90),
        axis.title=element_text(size=14),
        axis.text=element_text(size=12)) +
  geom_text_repel(data = subset(video_games, average_playtime>=4000), aes(label = game),
                  size=4, color="#FA5F70FF")


png("video games_points.png", height=480, width=580)
g1
dev.off()

