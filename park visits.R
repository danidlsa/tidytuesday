library(tidyverse)
library(vtable)
library(GGally)
library(extrafont)
windowsFonts(sans="Segoe UI")
loadfonts(device="win")
loadfonts(device="postscript")

park_visits <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-09-17/national_parks.csv")
gas_price <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-09-17/gas_price.csv")

park_visits <- park_visits %>% filter(year>=min(gas_price$year))

visits_by_park <- park_visits %>% filter(year!="Total") %>% 
  mutate(year=as.numeric(year)) %>%
  select(year, region, visitors)

visits_by_park <- aggregate(visitors~year+region, visits_by_park, sum)
visits_by_park <- visits_by_park %>% 
  spread(key="region", value="visitors")

gas_price <- gas_price %>% mutate(price_year_before=lag(gas_constant)) %>%
  mutate(variation=gas_constant-price_year_before)

visits_by_park <- visits_by_park %>% left_join(gas_price, by="year")

vtable(visits_by_park)
visits_by_park <- visits_by_park %>%
  mutate(AK_lag=lag(AK)) %>%
  mutate(IM_lag=lag(IM)) %>%
  mutate(MW_lag=lag(MW)) %>%
  mutate(NC_lag=lag(NC)) %>%
  mutate(NE_lag=lag(NE)) %>%
  mutate(NT_lag=lag(NT)) %>%
  mutate(PW_lag=lag(PW)) %>%
  mutate(SE_lag=lag(SE)) %>%
  mutate(AK_variation=AK-AK_lag) %>%
  mutate(IM_variation=IM-IM_lag) %>%
  mutate(MW_variation=MW-MW_lag) %>%
  mutate(NC_variation=NC-NC_lag) %>%
  mutate(NE_variation=NE-NE_lag) %>%
  mutate(NT_variation=NT-NT_lag) %>%
  mutate(PW_variation=PW-PW_lag) %>%
  mutate(SE_variation=SE-SE_lag)

vtable(visits_by_park)

df_final <- visits_by_park %>% select(-year, -(AK:price_year_before), -(AK_lag:SE_lag))  

cor(df_final, use="complete.obs")

colnames(df_final)[1:9] <- c("Gas price", "Alaska", "Intermountain", "Midwest", "National Capital", "Northeast", "NT", "Pacific West", "Southeast")

g <- ggcorr(df_final, method=c("complete", "pearson"), nbreaks = 5, palette="RdBu",
       label=TRUE, label_size=4, label_alpha = TRUE, label_round=2, hjust=0.75,
       size=4, color="grey20", layout.exp = 1) +
  labs(title="Correlation between gas price anual variation and the number of visitors  \nin National US Parks over the years",
       subtitle="By region",
       caption="Visualization: @danidlsa")+
  theme(plot.title=element_text(face="bold", size=14),
        plot.subtitle=element_text(face="bold", size=12),
        plot.caption=element_text(size=10))

ggsave("parks.png",g, height=15, width=20, units="cm")
