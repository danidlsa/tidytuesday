emperors <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-08-13/emperors.csv")

library(tidyverse)
library(lubridate)
library(scales)
library(plotly)
library(htmlwidgets)
library(RColorBrewer)


#Tidying data 

positions <- c(0.5, -0.5, 1.0, -1.0, 1.5, -1.5)
directions <- c(1, -1)
text_offset <- 0.05

emperors <- emperors %>% mutate(position=rep(positions, length.out=length(emperors$reign_start))) %>%
  mutate(direction=rep(directions, length.out=length(emperors$reign_start))) %>%
  mutate(year=year(reign_start)) %>%
  mutate(text_position=direction*text_offset + position)

emperors <- emperors[with(emperors, order(reign_start, name)), ]

head(emperors)

#Timeline

timeline_plot<-ggplot(emperors,aes(x=reign_start,y=0, col=rise, label=name,
                                   text = paste('\nEmperor:', name,
                                                '\nReigns from:', reign_start,
                                                '\nto:', reign_end,
                                                '\nBorn in:', birth_prv,
                                                '\nRose to power:', rise,
                                                '\nCause of death:', cause))) +
  labs(col="How did they come to power",
       title="Roman Emperors: A Timeline",
       caption="Visualization:@danidlsa | Dataset: @geokaramanis",
       y="Year (start of reign)") +
  theme_classic() + 
  geom_hline(yintercept=0, color = "black", size=0.3) +
  geom_segment(aes(y=position,yend=0,xend=reign_start), color='grey', size=0.2) +
  geom_point(aes(y=0), size=3) + 
  theme(axis.line.y=element_blank(),
        axis.text.y=element_blank(),
        axis.title.x=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.x =element_blank(),
        axis.ticks.x =element_blank(),
        axis.line.x =element_blank(),
        legend.position="bottom",
        title=element_text(face="bold", size=14),
        legend.title=element_text(size=12, face="bold"),
        axis.title.y=element_text(size=10),
        plot.background = element_rect("cornsilk"),
        panel.grid.major=element_blank(),
        panel.background = element_rect("cornsilk"),
        legend.background = element_rect("cornsilk")
  ) +
  geom_text(aes(x=reign_start,y=0.1,label=year),size=2,vjust=0.5, 
            color='black', angle=45, check_overlap = TRUE) +
  geom_text(aes(y=text_position,label=name),size=2.5) +
  scale_color_brewer(palette = "Set1") 
print(timeline_plot)

#Saving plot

ggsave (filename = "roman emperors.png", plot = timeline_plot, height=11, width=40, units="cm")

#Plotly object

timeline_plotly <- ggplotly(timeline_plot, tooltip = "text")

saveWidget(timeline_plotly, 'roman_emperors.html')

api_create(timeline_plotly, filename = "roman_emperors")

#See the plot in: https://plot.ly/~dlsantos.daniela/1.embed 


