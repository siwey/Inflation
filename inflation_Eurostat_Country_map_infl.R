rm(list=ls())

library("eurostat")
library(dplyr) 
library(ggplot2)

ct <- c("AT","BE","BG","CH","CY","CZ","DE","DK","EE","EL","ES","FI","FR","HR","HU","IE","IS","IT","LI","LU","LT","LV","MT","NL","NO","PL","PT","RO","SE","SI","SK","UK")

#dat <- get_eurostat(id="prc_hicp_manr")

#dat_2022 <- filter(dat,unit=="RCH_A", coicop=="CP00", geo %in% ct,time=="2021-01-01") #geo==ct)) & indic_is==ty))#time="2019-01-01")# & indic_is=ty)

dat <- get_eurostat(id="lfsa_urgacob")

dat_2022 <- filter(dat,unit=="PC", c_birth=="TOTAL",age=="Y55-64", sex=="T", geo %in% ct,time=="2021-01-01") #geo==ct)) & indic_is==ty))#time="2019-01-01")# & indic_is=ty)



mapdata <- get_eurostat_geospatial(nuts_level=0) %>%
  right_join(dat_2022) %>% 
  mutate(cat = cut_to_classes(values, n=10, decimals=0))
#head(select(mapdata,geo,values,cat),3)

p <- ggplot(mapdata,aes(fill=cat)) +
  scale_fill_brewer(palette="OrRd",na.value="black") + #BluesB "OrRd"
  geom_sf(color = alpha("white",1/3), alpha = .9) +
#  geom_point(data = mapdata, aes(size="NA"), shape =NA, colour = "red")+
#  guides(size=guide_legend("NA", override.aes=list(shape=15, size = 10)))+
  xlim(c(-15,44)) + ylim(c(35,70)) +
  theme_minimal()+
  labs(title = "",
       subtitle="",
       fill = "%") 
pdf('/Users/simonwey/Repos/Inflation/graphics/MAP_ELQ_55-64.pdf')
print(p)
dev.off()