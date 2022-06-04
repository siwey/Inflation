rm(list=ls())


library(OECD)
library(tidyverse)
library(ggplot2)
library(echarts4r)

dataset <- "PRICES_CPI"

dstruc <- get_data_structure(dataset)
df <- data.frame(dstruc$SUBJECT$id,dstruc$SUBJECT$label)

#filter_list <- list(c("CHE","DEU","USA","EU27_2020"),c('CPALTT01','CPGRLE01'),'GY','M')#, 'CP00', 'GP')#, 'M', 'AVGRW')#, 'P1M') #c("AUT","ITA","CHE","DEU","FRA")
filter_list <- list("CHE",c('CPALTT01','CPGRLE01','CPGREN01','CP010000','CP070200'),'GY','M')#, 'CP00', 'GP')#, 'M', 'AVGRW')#, 'P1M') #c("AUT","ITA","CHE","DEU","FRA")
de <- get_dataset(dataset,filter=filter_list)

sel <- de %>%
  filter(obsTime >= 2010-01
         #obsTime <= 2022-01
         #MEASURE == "STE"
  ) |>
  # let's focus on cols location, time value, because none
  # of the other cols contain information, i.e., all their values
  # are the same
  
  select(obsTime, SUBJECT, obsValue)

wide_out <- sel %>%
  pivot_wider(names_from = SUBJECT,
              values_from = obsValue)

wide_out |>
  e_charts(obsTime) |>
  e_line(CPALTT01, name = "Total",lineStyle=list(width=1.5),symbol='none') |>
  e_line(CPGRLE01, name = "Total, ohne Energie und Nahrungsmittel",lineStyle=list(width=1.5),symbol='none') |>
  e_line(CPGREN01, name = "Energie",lineStyle=list(width=1.5),symbol='none') |>
  e_line(CP010000, name = "Essen",lineStyle=list(width=1.5),symbol='none') |>
  e_line(CP070200, name = "Treibstoff für personlichen Verkehr",lineStyle=list(width=1.5),symbol='none') |>
#  e_line(FRA, name = "Frankreich") |>
  #e_tooltip(trigger="a
  #e_text_style(fontSize=30) |>
  #e_format_x_axis(suffix = "%") |>
  e_tooltip(formatter = htmlwidgets::JS("
      function(params){
        return('<strong>' + params.name + 
                '</strong><br />' + 
                parseFloat((params.value[1] * 10) / 10).toFixed(0) +'%') 
                }")
  ) |>
  e_legend(orient = 'horizontal', top = 30) |>
  #e_format_y_axis(suffix="%") |>
  e_axis_stagger() |>
  
  #  formatter = e_axis_formatter(fon)
  #)
  
  e_x_axis(axisLabel=list(fontSize=20)) |>
  e_y_axis(axisLabel=list(fontSize=20)) |>
  e_format_y_axis(suffix="%") 