loans <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-11-26/loans.csv")

library(tidyverse)
library(viridis)
library(extrafont)
library(ggforce)
library(ggrepel)
library(ggdark)

loans <- loans %>% mutate(date=paste0(year,"-",quarter)) 


loans_max <- aggregate(wage_garnishments~date, loans, max) %>%
  mutate(maximum="Yes")
loans <- loans %>% left_join(loans_max, by=c("wage_garnishments", "date"))

ggplot(loans, aes(x=date, y=wage_garnishments)) +
  geom_jitter(size=2, alpha = 0.7, width = 0.15, aes(color=as.factor(year))) +
  geom_boxplot(alpha=0.2, width=.7, fill="grey25") +
  scale_color_viridis(discrete=T, option="inferno") +
  dark_theme_gray() +
  theme(text=element_text(family="Segoe UI", color="white"),
        legend.position="",
        axis.title=element_text(face="bold", size=12),
        plot.title=element_text(face="bold", size=16),
        plot.subtitle=element_text(face="italic", size=14),
        plot.caption=element_text(size=11)) +
  labs(x="Date (Year-Quarter)",
       y="USD",
       title="Wage Garnishment of Student Loans",
       subtitle="Each point represents an agency",
       caption="Visualization: @danidlsa") +
  geom_label_repel(data = subset(loans, maximum=="Yes"), 
                   aes(label = agency_name, color=agency_name),
                  size=3)

ggsave("wage garnishment.png", height=14, width=18, units="cm")
