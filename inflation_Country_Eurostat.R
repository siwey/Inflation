rm(list=ls())

library("eurostat")
library(dplyr) 
library(ggplot2)
#query <- search_eurostat(pattern="digital skills", fixed=FALSE)
#a <- get_eurostat("isoc_sk_dskl_i")

cat <- c("CH","EU27_2020")#AT","DE","CH","FR","IT","EA19")
ty <- c("TOT_X_NRG","CP00")#c("AP","NRG","CP00","CP0113")  #  ,time_format="num"

dat <- get_eurostat(id="prc_hicp_manr") #,filters==list(indic_is="I_DSK_AB"))#,ind_type="Y16-24"))#,,unit="PC_IND",geo=ct))#,age="Y15-19",sex="T"))
d <- filter(dat,unit=="RCH_A", coicop %in% ty, geo== "CH") #geo==ct)) & indic_is==ty))#time="2019-01-01")# & indic_is=ty)
#h <- subset(d, geo=="CH" & sex=="T")

d$geo[d$geo=="AT"] <- "Österreich"
d$geo[d$geo=="CH"] <- "Schweiz"
d$geo[d$geo=="DE"] <- "Deutschland"
d$geo[d$geo=="FR"] <- "Frankreich"
d$geo[d$geo=="IT"] <- "Italien"


ggplot(d, aes(x=time, y=values, color = coicop, label=coicop)) +
  geom_line(data = d, aes(x = time, y = values),size=.6)+
  #  geom_text(data=dat %>% group_by(geo)%>% filter(time==max(time)),size=2.6) + 
  # theme(legend.position="left")+
#  scale_color_manual(values = c("#8E44AD", "#F1C40F","#34495E","#138D75","#E74C3C","#F8C471"))+
  labs(#title = "Erwerbslosenrate",
    subtitle = "Quelle: Eurostat",
    y = "",
    x = "") +
  theme_minimal()+
  theme(legend.position="top",legend.title=element_blank()) + 
  coord_fixed(ratio=500) +  
  ggtitle("Inflationsraten - excluding energy")+
  theme(text = element_text(size = 15))+
  scale_y_continuous(labels = function(x) paste0(x*1, "%")) 
#png('/Users/simonwey/Repos/gateway.one/graphics/infl_country.png')
#print(p)
#dev.off()
