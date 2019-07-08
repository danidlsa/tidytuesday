wwc_outcomes <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-07-09/wwc_outcomes.csv")
squads <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-07-09/squads.csv")
codes <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-07-09/codes.csv")

library (tidyverse)
library(extrafontdb)
library(extrafont)

windowsFonts(sans="Elegance")
loadfonts(device="win")
loadfonts(device="postscript")

#I have installed FIFA's font, Elegance, before loading fonts.


us <- squads %>% filter(country=="US") %>%
  mutate(positions_x=ifelse(pos=="FW", 75,
                            ifelse(pos=="MF", 50,
                                   ifelse(pos=="DF", 25, 5)))) %>%
  mutate(positions_y=ifelse(player=="Alyssa Naeher", 50,
                            ifelse(player=="Kelley O'Hara", 85,
                                   ifelse(player=="Abby Dahlkemper", 63,
                                          ifelse(player=="Becky Sauerbrunn", 37,
                                                 ifelse(player=="Crystal Dunn", 15,
                                                        ifelse(player=="Rose Lavelle" | player=="Tobin Heath", 75,
                                                               ifelse(player=="Alex Morgan" | player=="Julie Ertz", 50, 
                                                                      ifelse(player=="Megan Rapinoe" | player=="Sam Mewis", 25, 0))))))))) %>%
  mutate(titular=ifelse(positions_y==0, 0, 1)) %>%
  filter(titular==1)
  
us

install.packages("ggsoccer")
library(ggsoccer)

png("us football team.png", height=820, width=574)

ggplot(us) +
  annotate_pitch(colour = "white",
                 fill   = "chartreuse4",
                 limits = FALSE) +
  geom_point(aes(x = positions_x, y = positions_y, size=caps),
             colour = "red") +
  theme_pitch() +
  theme(plot.background = element_rect(fill = "chartreuse4"),
        title = element_text(colour = "white", size=14, face="bold"),
        legend.title = element_text(colour="black", size=14), 
        legend.background = element_rect("chartreuse4"),
        legend.position="bottom",
        legend.text=element_text(size=12)) +
  coord_flip() +
  scale_size_continuous("Number of international games played:") +
  ggtitle("US football team - Women's World Cup 2019") + 
  geom_label(aes(label=player, x=positions_x, y=positions_y), 
                                     nudge_x = 6, size=5)

dev.off()

                            