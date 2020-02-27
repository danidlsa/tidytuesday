x <- readr::read_csv("https://raw.githubusercontent.com/WSJ/measles-data/master/state-overviews.csv")

vtable::vtable(x)


library(tidyverse)
library(ggrepel)
library(extrafont)
library(ggforce)

firstup <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}

windowsFonts(sans="Roboto")
loadfonts(device="win")
loadfonts(device="postscript")

  
xx <- subset(x, is.na(xrel)==F)

p <- ggplot(xx, aes(x=state, y=xrel)) + 
  geom_boxplot(alpha=.5, fill="grey80", color="#6b626b", width=.5) +
  geom_jitter(aes(color=xrel), size=1, width=.1) +
  scale_color_continuous(low="#9c8f9a", high="#242124") +
  coord_flip() +
  geom_label_repel(data=subset(x, xrel>15), aes(label=paste0(firstup(`county/district`),
                                                             "\n",round(xrel, digits=1), 
                                                             "%")), size=3, 
                   color="#6b626b") +
  ggthemes::theme_solarized() +
  theme(legend.position="",
        axis.title=element_text(size=11, face="bold", color="#6b626b"),
        axis.text=element_text(size=10),
        plot.title=element_text(size=14, hjust=0, face="bold", color="#6b626b"),
        plot.subtitle=element_text(size=12, hjust=0, color="#6b626b"),
        plot.caption=element_text(size=9, color="#6b626b")) +
  labs(x="State", y="% of students exempted from vaccination for religious reasons",
       title="Exemption from Measles Vaccination for Religious Reasons",
       subtitle="US States, 2017-2018",
       caption="Data: The Wall Street Journal | Visualization: @danidlsa")


averages <- aggregate(xrel~state, xx, mean)
median <- aggregate(xrel~state, xx, median)

mav <- averages %>% filter(xrel==max(xrel))
max(median$xrel)
mmed <- median %>% filter(xrel==max(xrel))
h <- mean(xx$xrel)

p2 <- p + geom_hline(yintercept=h, color="black") +
  annotate("text", x=3, y=12, label=paste0("Average for all States: ",
                                           round(h, digits=1),"%")) +
  geom_curve(aes(x=2.8, xend=1.5, y=12, yend=h), color="grey20", arrow=arrow(length = unit(0.4, "cm")),
             curvature=-.3) +
  annotate("text", x=9, y=20, label=paste0("Highest average: ", mav$state[1],", ",
                                           round(mav$xrel[1], digits=1),"%")) +
  geom_curve(aes(x=9, xend=12, y=12.5, yend=mav$xrel[1]), color="grey20", arrow=arrow(length = unit(0.4, "cm")),
             curvature=-.3) +
  annotate("text", x=19, y=20, label=paste0("Highest median value: ", mmed$state[1],", ",
                                            (round(mmed$xrel[1], digits=1)),"%")) +
  geom_curve(aes(x=18.8, xend=18, y=20, yend=mmed$xrel[1]), color="grey20", arrow=arrow(length = unit(0.4, "cm")),
             curvature=-.5) 
  
p2

ggsave(plot = p2, filename="measles.png", height=18, width=20, units="cm")
