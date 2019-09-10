library(tidyverse)
library(vtable)
library(tvthemes)
install.packages("statebins", repos = "https://cinc.rud.is")
library(statebins)
library(extrafont)
install.packages("jcolors")
library(jcolors)

windowsFonts(sans="Titillium Web")
loadfonts(device="win")
loadfonts(device="postscript")

tx_injuries <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-09-10/tx_injuries.csv")

safer_parks <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-09-10/saferparks.csv")

table(safer_parks$acc_state)
table(safer_parks$gender)
table(safer_parks$age_youngest)

#Tidying data
vtable(safer_parks)

accidents_states <- aggregate(num_injured~acc_state, safer_parks, sum)

colnames(accidents_states)[2] <- "Number of people injured"

#Map - statebins library

map <- ggplot(accidents_states, aes(state=acc_state, fill=`Number of people injured`)) +
  geom_statebins() +
  coord_equal() +
  scale_fill_jcolors_contin(palette = "pal12") +
  theme_void() +
  labs(title="Amusement Park Injuries - USA",
       caption="Visualization: @danidlsa 
                Data Source: data.world + Safer Parks database",
       subtitle="Count by State") +
  theme(plot.title=element_text(face="bold", size=16),
        legend.title=element_text(face="bold", size=13),
        legend.text=element_text(size=11),
        plot.caption=element_text(size=11),
        plot.margin=unit(c(0,1,0,1), "cm"),
        plot.background = element_rect(color = NA, fill = "#238443"),
        panel.background = element_rect(color = NA, fill = "#238443"),
        legend.background = element_rect(color = "black", fill = "#CCFFBB", linetype = "solid"),
        legend.position="bottom",
        legend.margin = margin(10,10,10,10),
        plot.subtitle = element_text(size=14, face="bold"))


ggsave("parks map.png", map, height=15, width=25, units="cm")

#Line plot - gender x age 
#Gender is only assigned as F or M when num_injured==1, or when all of the people injured are of the same gender)
#Age is measured through a proxy: age of the youngest person injured in the accident
safer_parks <- safer_parks %>% filter(gender=="F" | gender=="M") %>%
  mutate(Gender=ifelse(gender=="F", "Women", "Men"))


gender_age <- ggplot(safer_parks, aes(x=age_youngest, col=Gender)) + 
  geom_line(stat="count", size=1.2) +
  theme_parksAndRec() +
  scale_color_parksAndRec("Gender") +
  labs(x="Age", y="Count", title="People injured in Amusement Parks",
       subtitle="Age and Gender",
       caption="Visualization: @danidlsa 
                Data Source: data.world + Safer Parks database") +
  theme(plot.title=element_text(face="bold", size=16),
        plot.caption=element_text(size=11),
        plot.subtitle=element_text(face="bold", size=14),
        legend.title=element_text(face="bold", size=12),
        legend.text=element_text(size=12))



ggsave("parks_gender_age.png", gender_age, height=12, width=18, units="cm")

