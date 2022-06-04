rm(list=ls())

library("eurostat")
library(dplyr) 
library(ggplot2)


ct <- c("AU","BE","BG","CH","CY","CZ","DE","DK","EE","EL","ES","FI","FR","HR","HU","IE","IS","IT","LI","LU","LT","LV","MT","NL","NO","PL","PT","RO","SE","SI","SK","UK")
ct2 <- c("AU","BE","BG","CH","CY","CZ","DE","DK","EE","EL","ES","FI","FR","HR","HU","IE","IT","LI","LT","LV","NL","NO","PL","PT","RO","SE","SI","SK","UK")


dat <- get_eurostat(id="prc_hicp_manr")

dat_2020 <- filter(dat,unit=="RCH_A", coicop=="CP00", geo %in% ct,time=="2022-04-01") #geo==ct)) & indic_is==ty))#time="2019-01-01")# & indic_is=ty)


mapdata <- get_eurostat_geospatial(nuts_level=0) %>%
  right_join(dat_2020) %>% 
  mutate(cat = cut_to_classes(values, n=10, decimals=0))

#head(select(mapdata,geo,values,cat),3)

p <- ggplot(mapdata,aes(fill=cat)) +
  scale_fill_brewer(palette="OrRd",na.value="black") + #BluesB "OrRd"
  geom_sf(color = alpha("white",1/3), alpha = .9) +
  #legend("bottomleft", legdtxt, horiz = F, fill = plotclr, box.lty=0) +
#  geom_point(data = mapdata, aes(size="NA"), shape =NA, colour = "red")+
#  guides(size=guide_legend("NA", override.aes=list(shape=15, size = 10)))+
  xlim(c(-20,35)) + ylim(c(35,70)) +
  theme_minimal()+
  labs(title = "",
       subtitle="",
       fill = "%") 
pdf('~/Repos/Inflation/graphics/MAP_Inflation_EU_202204.pdf')
print(p)
dev.off()