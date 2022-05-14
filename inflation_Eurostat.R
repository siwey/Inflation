rm(list=ls())

library("eurostat")
library(dplyr) 
library(ggplot2)
#query <- search_eurostat(pattern="digital skills", fixed=FALSE)
#a <- get_eurostat("isoc_sk_dskl_i")

cat <- c("AT","DE","CH","FR","IT")
ty <- c("NRG","CP00","CP0452","CP0451")  #  ,time_format="num"
t <- c("BAD","VBAD")#"GOOD","VGOOD")

dat <- get_eurostat(id="prc_hicp_manr") #,filters==list(indic_is="I_DSK_AB"))#,ind_type="Y16-24"))#,,unit="PC_IND",geo=ct))#,age="Y15-19",sex="T"))
d <- filter(dat,unit=="RCH_A", coicop %in% ty, geo =="DE",time>="2018-01-01") #geo==ct)) & indic_is==ty))#time="2019-01-01")# & indic_is=ty)
#h <- subset(d, geo=="CH" & sex=="T")

h$levels[h$levels=="B_VB"] <- "Schlecht bis sehr schlecht"
h$levels[h$levels=="VBAD"] <- "sehr schlecht"
h$levels[h$levels=="VGOOD"] <- "sehr gut"
h$levels[h$levels=="BAD"] <- "schlecht"
h$levels[h$levels=="GOOD"] <- "gut"
h$levels[h$leveaaaaaals=="VG_G"] <- "gut bis sehr gut"


p <- ggplot(d, aes(x=time, y=values, color = coicop, label=coicop)) +
  geom_line(data = d, aes(x = time, y = values),size=1.)+
  #  geom_text(data=dat %>% group_by(geo)%>% filter(time==max(time)),size=2.6) + 
  # theme(legend.position="left")+
#  scale_color_manual(values = c("#8E44AD", "#F1C40F","#34495E","#138D75","#E74C3C","#F8C471"))+
  labs(#title = "Erwerbslosenrate",
    subtitle = "Quelle: Eurostat",
    y = "",
    x = "") +
  theme_minimal()+
  theme(legend.position="top",legend.title=element_blank()) + 
  coord_fixed(ratio=10) +  
  ggtitle("Wahrgenommene Gesundheit von Jugendlichen")+
  theme(text = element_text(size = 15))+
  scale_y_continuous(labels = function(x) paste0(x*1, "%")) 
png('/Users/simonwey/Repos/gateway.one/graphics/infl.png')
print(p)
dev.off()
