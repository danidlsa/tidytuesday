sf_trees <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-28/sf_trees.csv')

library(tidyverse)

table (sf_trees$species)

palms <- sf_trees %>% filter(grepl("Palm", species)==TRUE | grepl("palm", species)==TRUE)

library(ggmap)
library(extrafont)
library(emojifont)

register_google(key = "***")

#Density map

sf <- get_map(location=c(lon = -122.439486, lat = 37.763850), zoom=12, 
                  maptype=c("roadmap"))

search_emoji("palm")

g1 <- ggmap(sf) + stat_density2d(
  aes(x = longitude, y = latitude, fill = ..level..), alpha=.1,
  size = 0.01, bins = 30, data = palms,
  geom = "polygon"
) +
  theme_void() +
  scale_fill_gradient(name="Number of palm trees", low="yellow", high="darkgreen") +
  labs(title=paste(emoji("palm_tree"), "Palm Trees in San Francisco", emoji("palm_tree")),
       subtitle="Density plot",
       caption="Data Source: San Francisco's open data portal | Visualization: @danidlsa") +
  theme(text=element_text(family="Segoe UI"),
        plot.title=element_text(size=14, face="bold", hjust=.5),
        plot.subtitle=element_text(size=12, face="bold", hjust=.5),
        plot.caption=element_text(size=11, hjust=.5),
        legend.title=element_text(size=12),
        legend.position="right")

g1

#Watercolor painting

sf_watercolor <- get_map(location=c(lon = -122.439486, lat = 37.763850), zoom=12, 
                    maptype=c("watercolor"))

ggmap(sf_watercolor) + geom_point(data=sf_trees,
  aes(x = longitude, y = latitude), color="#46a830", size=.1, alpha=.3
) +
  theme_void() +
  labs(title="Trees in San Francisco",
       subtitle="A watercolor painting",
       caption="Data Source: San Francisco's open data portal | Visualization: @danidlsa") +
  theme(plot.title=element_text(size=14, face="bold", hjust=.5, family="Akbar", color="#46a830"),
        plot.subtitle=element_text(size=12, hjust=.5, family="Akbar", color="#46a830"),
        plot.caption=element_text(size=10, family="Segoe UI", hjust=.5, color="#454444"))

