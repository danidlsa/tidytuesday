horror_movies <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-10-22/horror_movies.csv")

library(tidyverse)
library(vtable)
library(ggforce)
library(ggdark)
library(extrafont)
library(extrafontdb)
library(grid)
library(OpenImageR)
library(cowplot)


windowsFonts(sans="Creepsville")
loadfonts(device="win")
loadfonts(device="postscript")

vtable(horror_movies)


table(horror_movies$genres)

hm <- horror_movies %>% mutate(comedy=ifelse(grepl("Comedy", genres)==T, 1, 0)) %>%
  mutate(drama=ifelse(grepl("Drama", genres)==T, 1, 0)) %>%
  mutate(c_d=ifelse(comedy==1 & drama==0, "Comedy",
                    ifelse(comedy==0 & drama==1, "Drama",
                           ifelse(comedy==0 & drama==0, "Neither Comedy \nnor Drama", 
                                  "Both Comedy \nand Drama"))))

mean_rating <- aggregate(review_rating~c_d, hm, median)                      

g <- ggplot(hm, aes(x=c_d, y=review_rating, color=review_rating)) + 
  geom_jitter(size=2, alpha = 0.6, width = 0.15) +
  scale_color_continuous(low="#ffd97f", high="#ff501f") +
  geom_boxplot(alpha=0, color="white", width=.5)  +
  labs(title="Some horror movies are also classified as comedies or dramas...",
       subtitle="Which genre combos get better reviews?",
       caption="@danidlsa - TidyTuesday",
       x="Genre: Horror plus...",
       y="Movie rating")  +
  dark_theme_test() +
  theme(text=element_text(color="lightgrey"),
        legend.position="",
        plot.title=element_text(size=17, color="#ffd97f", hjust = .5),
        plot.subtitle=element_text(face="italic", size=16, color="#ffd97f", hjust=.5),
        axis.text.x=element_text(size=14),
        axis.text.y=element_text(family="Arial"),
        axis.title.y=element_text(size=15, color="#ffd97f"),
        axis.title.x=element_text(size=15, color="#ffd97f"),
        plot.caption=element_text(size=14, color="#ffd97f")) +
  annotate("text", x = .6, y=6.8, label="5.8", 
           color="#ffd97f", size=5, family="Arial") +
  annotate("text", x=1.6, y=6.4, label="5.4", 
           color="#ffd97f" ,size=5, family="Arial") +
  annotate("text", x=2.6, y=6.5, label="5.5",
           color="#ffd97f", size=5, family="Arial") +
  annotate("text", x=3.6, y=5.8, label="4.8",
           color="#ffd97f", size=5, family="Arial") +
  geom_curve(aes(x=.6, xend=.75, y=6.6, yend=5.8), color="#ffd97f", arrow=arrow(length = unit(0.4, "cm")),
             curvature=.5) +
  geom_curve(aes(x=1.6, xend=1.75, y=6.2, yend=5.4), color="#ffd97f", arrow=arrow(length = unit(0.4, "cm")),
             curvature=.5) +
  geom_curve(aes(x=2.6, xend=2.75, y=6.3, yend=5.5), color="#ffd97f", arrow=arrow(length = unit(0.4, "cm")),
             curvature=.5) +
  geom_curve(aes(x=3.6, xend=3.75, y=5.6, yend=4.8), color="#ffd97f", arrow=arrow(length = unit(0.4, "cm")),
             curvature=.5)

pumpkin_comedy <- readImage("pumpkin-comedy.png")
pumpkin_drama <- readImage("pumpkin-drama.png")
pumpkin_both <- readImage("pumpkin-both.png")
pumpkin_neither <- readImage("pumpkin-neither.png")


pimage <- axis_canvas(g, axis = 'x') + 
  draw_image(pumpkin_both, x = .5, scale =1) +
  draw_image(pumpkin_comedy, x = 1.5, scale = 1) +
  draw_image(pumpkin_drama, x = 2.5, scale = 1) +
  draw_image(pumpkin_neither, x=3.5, scale=1)


g2 <- ggdraw(insert_xaxis_grob(g, pimage, position = "bottom"))

g2

ggsave("horror movies.png", g2, height=18, width=22, units="cm")
