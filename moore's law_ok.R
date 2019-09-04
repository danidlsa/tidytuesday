
ram <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-09-03/ram.csv")

install.packages('vtable')
library(vtable)
vtable(ram)
library(tidyverse)
library(cowplot)
install.packages("ggdark")
library(ggdark)
library(extrafontdb)
library(extrafont)

windowsFonts(sans="VCR OSD Mono")
loadfonts(device="win")
loadfonts(device="postscript")

options(scipen=999)
ram <- ram %>%  mutate(capacity_kb=ifelse(bit_units=="Bits",
                                          capacity_bits*0.001,
                                          ifelse(bit_units=="kb",
                                                 capacity_bits,
                                                 ifelse(bit_units=="Mb",
                                                        capacity_bits*1000,
                                                        capacity_bits*1e+6))))


vtable(ram)
g1 <- ggplot(ram, aes(x=date_of_introduction, y=capacity_kb)) +
  geom_point(col="lightgreen") + stat_smooth(method="glm", se=FALSE, col="khaki") +
  scale_y_log10() +
  labs(x="Date of introduction",
       y="Capacity (kilobits)",
       subtitle="By date of introduction") +
  dark_theme_gray() +
  theme(title=element_text(size=13))

g2 <- ggplot(ram, aes(x=transistor_count, y=capacity_kb)) +
  geom_line(col="khaki", size=1) + 
  dark_theme_gray() +
  scale_y_log10(breaks=c(1,10000,10000000)) +
  labs(x="Transistor count",
       y="Capacity (kilobits)",
       subtitle="By number of transistors") +
  theme(title=element_text(size=13))

grid_1 <- plot_grid(g1, g2, ncol=2)

title_gg <- ggplot() + labs(title="Evolution of RAM capacity",
                            subtitle="Logarithmic models") +
  dark_theme_light() + theme(title=element_text(size=17))
caption_gg <- ggplot() + labs(caption="Visualization: @danidlsa | Dataset: @R4DS") +
  dark_theme_light() + theme(title=element_text(size=15))


grid_final <- plot_grid(title_gg, grid_1, caption_gg, ncol=1, rel_heights = c(0.15, 1, 0.05))

ggsave(filename = "ram data.png", grid_final, height=16, width=28, units="cm")

