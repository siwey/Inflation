library("eurostat")
library(dplyr) 
library(ggplot2)


ct <- c("AU","AT","BE","BG","CH","CY","CZ","DE","DK","EE","EL","ES","FI","FR","HR","HU","IE","IS","IT","LI","LU","LT","LV","MT","NL","NO","PL","PT","RO","SE","SI","SK","UK")
ct2 <- c("AU","BE","BG","CH","CY","CZ","DE","DK","EE","EL","ES","FI","FR","HR","HU","IE","IT","LI","LT","LV","NL","NO","PL","PT","RO","SE","SI","SK","UK")
ct3 <- c("CH","DE","FR","IT","AT","EU27_2020","LT")
z <- c("NRG","CP011","ELC_GAS","CP0114","CP0451")


dat <- get_eurostat(id="prc_hicp_manr")

dat_2020 <- filter(dat,unit=="RCH_A", coicop %in% z, geo =="CH",time>="2022-04-01") #geo==ct)) & indic_is==ty))#time="2019-01-01")# & indic_is=ty)


sel <- dat_2020 %>%
  filter(time >= 2020
         #MEASURE == "STE"
  ) |>
  # let's focus on cols location, time value, because none
  # of the other cols contain information, i.e., all their values
  # are the same
  
  select(coicop, values)

sel$coicop[sel$coicop=="CP0114"] <- "Milch, Käse und Eier"
sel$coicop[sel$coicop=="NRG"] <- "Energie"
sel$coicop[sel$coicop=="ELC_GAS"] <- "Elektrizität und Gas"
sel$coicop[sel$coicop=="CP011"] <- "Essen"
sel$coicop[sel$coicop=="CP0451"] <- "Elektrizität"





ggplot(data=sel,aes(x=reorder(coicop,values), y=values,fill=coicop)) +
  geom_col() +
  coord_flip() +
  scale_fill_manual(values = c("#8E44AD", "#F1C40F","#34495E","#138D75","#E74C3C","black","green"))+
  labs(#title = "Erwerbslosenrate",
    subtitle = "Quelle: Eurostat",
    y = "",
    x = "") +
  theme_minimal()+
  ggtitle("Wachstum Nahrungsmittelpreise April 2022")+
  theme(legend.position="",legend.title=element_blank()) + 
  #coord_fixed(ratio=60) +  
  theme(text = element_text(size = 15))+
  scale_y_continuous(labels = function(x) paste0(x*1, "%")) 

