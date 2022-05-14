rm(list=ls())

library("eurostat")
library(dplyr) 
library(ggplot2)

ct <- c("AT","BE","BG","CH","CY","CZ","DE","DK","EE","EL","ES","FI","FR","HR","HU","IE","IS","IT","LI","LT","LU","LV","MT","NL","NO","PL","PT","RO","SE","SI","SK","UK")

#dat <- get_eurostat(id="prc_hicp_manr")

#dat_2022 <- filter(dat,unit=="RCH_A", coicop=="CP00", geo %in% ct,time=="2022-01-01") #geo==ct)) & indic_is==ty))#time="2019-01-01")# & indic_is=ty)

dat <- get_eurostat(id="lfsq_urgan")

dat_2022 <- filter(dat,unit=="PC",citizen=="TOTAL", age=="Y15-64", sex=="T", geo %in% ct,time=="2021-01-01") #geo==ct)) & indic_is==ty))#time="2019-01-01")# & indic_is=ty)



mapdata <- get_eurostat_geospatial(nuts_level=0) %>%
  right_join(dat_2022) %>% 
  mutate(cat = cut_to_classes(values, n=10, decimals=1))
#head(select(mapdata,geo,values,cat),3)

p <- ggplot(mapdata,aes(fill=cat)) +
  scale_fill_brewer(palette="OrRd") + #Blues
  geom_sf(color = alpha("white",1/3), alpha = .9) +
  xlim(c(-12,44)) + ylim(c(35,70)) +
  theme_minimal()+
  labs(title = "Inflation rate, April 2022",
       subtitle="Avg. number of life births per woman",
       fill = "%")
png('/Users/simonwey/Repos/gateway.one/graphics/MAP_ELQ_15-24.png')
print(p)
dev.off()