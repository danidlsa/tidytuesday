library(tidyverse)
#library(devtools)
#install_github("https://github.com/Ryo-N7/tvthemes")
library(tvthemes)
library(extrafont)
windowsFonts(sans="Akbar")
loadfonts(device="win")
loadfonts(device="postscript")

simpsons <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-08-27/simpsons-guests.csv")

themselves <- simpsons %>% filter(role=="Himself" | role=="Herself") %>%
  mutate(n=1) %>% 
  filter(season!="Movie")
ts <- aggregate(n~role+season, themselves, sum)

table(themselves$role)

g <- ggplot(ts, aes(x=as.numeric(season), y=n, fill=role)) + 
  geom_area(position="fill") +
  theme_simpsons(axis.text.size=8) + 
  scale_fill_simpsons(name="Role by gender:") +
  labs(x="Season",
       y="Ratio",
       title="Guest Stars playing themselves, by gender",
       subtitle="The Simpsons - Seasons 2-30",
       caption="@danidlsa for #TidyTuesday") +
  theme(legend.title=element_text(size=11),
        legend.text=element_text(size=11)) +
  annotate("text", x = 15, y = .2, 
             label = "Only 14% of all guest stars that played", 
             col="yellow", size=5) +
  annotate("text", x=15, y=.10, 
           label="themselves in The Simpsons are women!", 
           color="yellow", size=5)

ggsave("the simpsons_gender.png", g, height=10, width=15, units="cm")
