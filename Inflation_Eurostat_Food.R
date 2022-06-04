library("eurostat")
library(dplyr) 
library(ggplot2)


ct <- c("AU","AT","BE","BG","CH","CY","CZ","DE","DK","EE","EL","ES","FI","FR","HR","HU","IE","IS","IT","LI","LU","LT","LV","MT","NL","NO","PL","PT","RO","SE","SI","SK","UK")
ct2 <- c("AU","BE","BG","CH","CY","CZ","DE","DK","EE","EL","ES","FI","FR","HR","HU","IE","IT","LI","LT","LV","NL","NO","PL","PT","RO","SE","SI","SK","UK")

z <- c("CP00","TOT_X_NRG_FOOD","CP011")


dat <- get_eurostat(id="prc_hicp_manr")

dat_2020 <- filter(dat,unit=="RCH_A", coicop =="CP011", geo %in% ct,time>="2010-03-01") #geo==ct)) & indic_is==ty))#time="2019-01-01")# & indic_is=ty)


sel <- dat_2020 %>%
  filter(time >= 2020
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
  #e_line(LT, name = "Litauen",lineStyle=list(width=2.0),symbol='none') |>
  e_line(AT, name = "Österreich",lineStyle=list(width=2.0),symbol='none') |>
  e_line(IT, name = "Italien",lineStyle=list(width=2.0),symbol='none') |>
  e_line(DE, name = "Deutschland",lineStyle=list(width=2.0),symbol='none') |>
  e_line(FR, name = "Frankreich",lineStyle=list(width=2.0),symbol='none') |>
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
