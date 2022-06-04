rm(list=ls())


library(OECD)
library(tidyverse)
library(ggplot2)
library(echarts4r)
library(zoo)

dataset <- "PRICES_CPI"

#dstruc <- get_data_structure(dataset)
#df <- data.frame(dstruc$MEASURE$id,dstruc$MEASURE$label)

filter_list <- list(c("CHE","DEU","EU27_2020","USA","GBR"),'CPALTT01','GY','a')#, 'CP00', 'GP')#, 'M', 'AVGRW')#, 'P1M') #c("AUT","ITA","CHE","DEU","FRA")
filter_list2 <- list(c("CHE","DEU","EU27_2020","USA"),'CPALTT01','GY','A')#, 'CP00', 'GP')#, 'M', 'AVGRW')#, 'P1M') #c("AUT","ITA","CHE","DEU","FRA")

de <- get_dataset(dataset,filter=filter_list)

sel <- de %>%
  filter(obsTime >= 2000
         #MEASURE == "STE"
  ) |>
  # let's focus on cols location, time value, because none
  # of the other cols contain information, i.e., all their values
  # are the same
  
  select(obsTime, LOCATION, obsValue)

wide_out <- sel %>%
  pivot_wider(names_from = LOCATION,
              values_from = obsValue)



wide_out |>
  e_charts(obsTime) |>
  e_line(CHE, name = "Schweiz",lineStyle=list(width=2.0),symbol='none') |>
  e_line(DEU, name = "Deutschland",lineStyle=list(width=2.0),symbol='none') |>
#  e_line(GBR, name = "Grossbritannien",lineStyle=list(width=2.0),symbol='none',color="purple") |>
  e_line(USA, name = "USA",lineStyle=list(width=2.0),symbol='none') |>
  e_line(EU27_2020, name = "EU-27",lineStyle=list(width=2.0),symbol='none') |>
  e_text_style(fontSize=20) |>
  e_title("Inflation (2010-01 bis 2022-04)", "Quelle: Eurostat") |>
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
   e_x_axis(axisLabel = list(interval = 2, rotate = 45,fontSize=15)) |> #40
  e_y_axis(axisLabel=list(fontSize=20)) |>
  e_format_y_axis(suffix="%") |>
  e_toolbox_feature("saveAsImage")
