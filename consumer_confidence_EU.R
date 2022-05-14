rm(list=ls())


library("eurostat")
library(tidyverse)
library(ggplot2)
library(echarts4r)

dataset <- "ei_bssi_m_r2"

dstruc <- get_data_structure(dataset)
#df <- data.frame(dstruc$SUBJECT$id,dstruc$SUBJECT$label)

ct <- c("EU27_2020","DE")
vf <- c("BS-ESI-I","BS-CSMCI-BAL")

#filter_list <- list(c("CHE","DEU","USA","EU27_2020"),c('CPALTT01','CPGRLE01'),'GY','M')#, 'CP00', 'GP')#, 'M', 'AVGRW')#, 'P1M') #c("AUT","ITA","CHE","DEU","FRA")
filter_list <- list("EU27_2020")#,c('CPALTT01','CPGRLE01','CPGREN01','CP010000','CP070200'),'GY','M')#, 'CP00', 'GP')#, 'M', 'AVGRW')#, 'P1M') #c("AUT","ITA","CHE","DEU","FRA")
dat <- get_eurostat(id=dataset)

dat_restr <- filter(dat,indic== "BS-CSMCI-BAL", s_adj=="SA", geo %in% ct,time>="2010-01-01") #geo==ct)) & indic_is==ty))#time="2019-01-01")# & indic_is=ty)
dat_restr$geo[dat_restr$geo=="DE"] <- "Deutschland"
dat_restr$geo[dat_restr$geo=="EU27_2020"] <- "EU"

sel <- dat_restr %>%
  filter(time >= 2010-01
         #obsTime <= 2022-01
         #MEASURE == "STE"
  ) |>
  # let's focus on cols location, time value, because none
  # of the other cols contain information, i.e., all their values
  # are the same
  
  select(time, geo, values)

wide_out <- sel %>%
  pivot_wider(names_from = geo,
              values_from = values)

wide_out |>
  e_charts(time) |>
  e_line(EU, name = "EU-27",lineStyle=list(width=2.0),symbol='none') |>
  e_line(Deutschland, name = "Deutschland",lineStyle=list(width=2.0),symbol='none') |>
  # e_line(USA, name = "USA",lineStyle=list(width=2.0),symbol='none') |>
  #  e_line(EU27_2020, name = "EU-27",lineStyle=list(width=2.0),symbol='none') |>
  e_text_style(fontSize=20) |>
  e_tooltip(formatter = htmlwidgets::JS("
      function(params){
        return('<strong>' + params.name + 
                '</strong><br />' + 
                parseFloat((params.value[1] * 10) / 10).toFixed(0) +'%') 
                }")
  ) |> 
  #e_color(color = c("red", "blue","black","#00FF00")) |>
  e_legend(orient = 'horizontal', top = 30) |>
  #e_format_y_axis(suffix="%") |>
  e_axis_stagger() |>
  e_x_axis(axisLabel = list(interval = 8, rotate = 45,fontSize=15)) |> #40
  e_y_axis(axisLabel=list(fontSize=20)) |>
  e_format_y_axis(suffix="%") |>
  e_toolbox_feature("saveAsImage")


p <- ggplot(dat_restr, aes(x=time, y=values, color = geo, label=geo)) +
  geom_line(data = dat_restr, aes(x = time, y = values,colour=geo),size=1.)+
  #  geom_text(data=dat %>% group_by(geo)%>% filter(time==max(time)),size=2.6) + 
  # theme(legend.position="left")+
  scale_color_manual(values = c("#8E44AD", "#F1C40F","#34495E","#138D75","#E74C3C","#F8C471"))+
  labs(#title = "Erwerbslosenrate",
    #subtitle = "Quelle: Eurostat",
    y = "",
    x = "") +
  #ggtitle("NEET-Rate internationaler Vergleich")+
  theme_minimal()+
  theme(legend.position="top",legend.title=element_blank()) + 
  coord_fixed(ratio=50) +  
  theme(text = element_text(size = 15))+
  scale_y_continuous(labels = function(x) paste0(x*1, "%"))
pdf('~/Repos/Inflation/graphics/consumer_conf.pdf')
print(p)
dev.off()