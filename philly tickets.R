tickets <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-12-03/tickets.csv")

vtable::vtable(tickets)
library(tidyverse)

if(!requireNamespace("devtools")) install.packages("devtools")
devtools::install_github("dkahle/ggmap", ref = "tidyup", force=TRUE)

library(ggmap)

register_google(key = "INSERT API KEY")

philly <- get_map(location=c(lon = -75.2, lat = 40), zoom=11, 
                  maptype=c("toner"))


library(viridis)
library(extrafont)

ggmap(philly) + stat_density2d(
  aes(x = lon, y = lat, fill = ..level.., alpha = 0.25),
  size = 0.01, bins = 30, data = tickets,
  geom = "polygon"
) +
  theme_void() +
  scale_fill_viridis(name="Number of parking tickets \nissued in 2017") +
  labs(title="Philadelphia Parking Violations",
       subtitle="Year 2017",
       caption="Data Source: Open Data Philly | Visualization: @danidlsa") +
  theme(text=element_text(family="Segoe UI"),
        plot.title=element_text(size=14, face="bold"),
        plot.subtitle=element_text(size=12, face="bold"),
        plot.caption=element_text(size=11),
        legend.title=element_text(size=12),
        legend.position="bottom") +
  scale_alpha(guide="none")

ggsave("tickets.png", height=14, width=18, units="cm")
