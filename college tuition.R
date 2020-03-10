library(tidyverse)
library(extrafont)
windowsFonts(sans="Roboto")
loadfonts(device="win")
loadfonts(device="postscript")

diversity_school <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-10/diversity_school.csv')

salary_potential <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-10/salary_potential.csv')

diversity_school <- diversity_school %>% filter(category=="Women")

both <- salary_potential %>% left_join(diversity_school, by="name") %>%
  filter(is.na(total_enrollment)==F) %>% 
  mutate(women_percentage=enrollment/total_enrollment*100)

library(corrgram)
library(GGally)

vtable::vtable(both)
cor(both %>% select(make_world_better_percent, stem_percent,
                    early_career_pay, total_enrollment,
                    women_percentage), use="complete.obs")
b <- both %>% select(stem_percent,make_world_better_percent,
                     early_career_pay, total_enrollment,
                     women_percentage)

g <- ggcorr(b, method=c("complete", "pearson"), nbreaks = 5, palette="RdBu",
            label=TRUE, label_size=4, label_alpha = TRUE, label_round=2, hjust=0.75,
            size=4, color="grey20", layout.exp = 1) +
  labs(title="Some correlations when talking about colleges",
       subtitle="US 2014-2019",
       caption="Data Source: US Department of Education | Visualization: @danidlsa")+
  theme(plot.title=element_text(face="bold", size=14, hjust=.5),
        plot.subtitle=element_text(face="bold", size=12, hjust=.5),
        plot.caption=element_text(size=10, face="italic"),
        legend.position="")
g

ggsave("college.png", height=12, width=16, units="cm")
