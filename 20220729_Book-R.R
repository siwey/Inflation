rm(list=ls())

library(OECD)
library(tidyverse)
library(ggplot2)
library(echarts4r)

dataset <- "PRICES_CPI"

dstruc <- get_data_structure(dataset)
df <- data.frame(dstruc$SUBJECT$id,dstruc$SUBJECT$label)

#filter_list <- list(c("CHE","DEU","USA","EU27_2020"),c('CPALTT01','CPGRLE01'),'GY','M')#, 'CP00', 'GP')#, 'M', 'AVGRW')#, 'P1M') #c("AUT","ITA","CHE","DEU","FRA")
filter_list <- list("CHE",c('CPALTT01','CPGRLE01','CPGREN01','CP010000','CP070200'),'GY')#, 'CP00', 'GP')#, 'M', 'AVGRW')#, 'P1M') #c("AUT","ITA","CHE","DEU","FRA")
de <- get_dataset(dataset,filter=filter_list)

ggplot(data=de) +
  stat_count(mapping=aes(x=SUBJECT,color=SUBJECT))
mpg %>% count(manufacturer)
ggplot(data=mpg) +
  stat_count(mapping=aes(x=manufacturer, fill=model), position="fill") +
  coord_flip()
ggplot(data=mpg) +
  geom_boxplot(mapping=aes(x=manufacturer))

ggplot(data=de) +
  stat_count(mapping=aes(x=SUBJECT,fill=FREQUENCY),position="dodge")

demo <- tribble(
  ~cut, ~freq,
  "Fair", 1610,
  "EHal", 100,
  "EHal", 1000,
  "hdk", 2,
)

ggplot(data=demo) +
  geom_bar(mapping=aes(x=cut, y=stat(prop)), group=1)

bar <- ggplot(data=de) +
  geom_bar(
    mapping=aes(x=FREQUENCY,fill=FREQUENCY),
    show.legend=FALSE,
    width=1
  ) +
  theme(aspect.ratio=1) +
  labs(x=NULL, y=NULL)

bar + coord_flip()
bar + coord_polar()

ggplot(data=mpg, aes(x=cty, y=hwy)) +
  geom_point() +
  geom_smooth(method="lm",se=FALSE)
  
p <- ggplot(data=mtcars, aes(mpg, wt,color=wt)) +
  geom_point()+
  facet_wrap(~cyl)
mean_wt <- data.frame(cyl=c(4,6,8), wt=c(2.28, 3.11, 4.00))
p+geom_hline(aes(yintercept=wt,color=wt),mean_wt)

ggplot(data = <DATA>) + 
  <GEOM_FUNCTION>(
    mapping = aes(<MAPPINGS>),
    stat = <STAT>, 
    position = <POSITION>
  ) +
  <COORDINATE_FUNCTION> +
  <FACET_FUNCTION>

library(nycflights13)
library(tidyverse)

Jan1 <-filter(flights, month==1, day==1)
filter(flights, month==11 | month==12)
nov_dec <- flights(flights,filter, month %in% c(11,12))
filter(flights,!(arr_delay>120 | dep_delay > 120))

df <- tibble(x=c(1,NA,3))
filter(df,x>1)
filter(df,is.na(x) | x>1)
filter(flights, arr_delay >= 120)
filter(flights, dest=="IAH" | dest=="HOU")
length(which(flights$dep_time==1))  
sum(is.na(flights$dep_time))
a <- arrange(flights,year,month,day)
a <- arrange(flights, desc(dep_delay))

a <- select(flights,year, dep_delay)
select(flights, year:carrier)
select(flights, -(year:day))
select(flights, contains("time"))
rename(flights, Year=year)
select(flights, dep_time, arr_time, everything())
select(flights, year, year, month, day)

vars <- c("year", "month", "day", "dep_delay", "arr_delay")
select(flights,any_of(vars))

select(flights, contains("TIME"))

flights_small <- select(flights,
                        year:day, 
                        ends_with("delay"),
                        distance, 
                        air_time
)

flights_small <- mutate(flights_small, 
       gain= dep_delay-arr_delay, 
       speed=distance/air_time*60,
       hours=air_time/60,
        gain_per_hour=gain/hours
)

flights_small_tr <- transmute(flights_small, 
                        gain= dep_delay-arr_delay, 
                        speed=distance/air_time*60,
                        hours=air_time/60,
                        gain_per_hour=gain/hours
)


a <-  arrange(flights_small,desc(speed))


summarize(flights, delay=mean(dep_delay,na.rm=TRUE))

by_day <- group_by(flights, year, month, day)

t <- summarize(by_day, delay=mean(dep_delay,na.rm=TRUE))

by_dest <- group_by(flights, dest)

delay <- summarize(by_dest,
                  count= n(),
                  dist=mean(distance,na.rm=TRUE),
                  delay=mean(arr_delay, na.rm=TRUE)
)

delay <- filter(delay, count>20,dest !="HNL")
ggplot(data=delay, mapping=aes(x=dist, y=delay)) +
  geom_point(aes(size=count), alpha=1/3) +
  geom_smooth(se=FALSE)

rm(list=ls())


delays <- flights %>% #Then
  group_by(dest) %>%
  summarize(
    count=n(),
    dist=mean(distance, na.rm=TRUE),
    delay=mean(arr_delay,na.rm=TRUE)
  ) %>%
  filter(count>20, dest!="HNL")

ggplot(data=delays, mapping=aes(x=dist, y=delay, label=dest)) +
  geom_point(aes(size=count), alpha=1/3) +
  geom_smooth(se=FALSE) +
  geom_text()

a <- flights %>% 
  group_by(year, month) %>%
  summarize(mean=mean(dep_delay,na.rm=TRUE))
  
not_cancelled <- flights %>%
  filter(!is.na(dep_delay), !is.na(arr_delay))

delays <- not_cancelled %>%
  group_by(tailnum) %>%
  summarize(
    delay = mean(arr_delay)
  )

ggplot(data=delays, mapping=aes(x=delay)) +
  geom_freqpoly(binwidth=10)

delays <- not_cancelled %>%
  group_by(tailnum) %>%
  summarize(
    delay=mean(arr_delay,na.rm=TRUE),
    n=n()
  )

ggplot(data=delays, mapping=aes(x=n, y=delay)) +
  geom_point(alpha=1/10)

delays %>%
  filter(n>25) %>%
  ggplot(mapping=aes(x=n, y=delay)) +
    geom_point(alpha=1/10)
  
library(Lahman)

batting <- as_tibble(Lahman::Batting)
batters <-  batting %>%
  group_by(playerID) %>%
  summarize(
    ba=sum(H,na.rm=TRUE) / sum(AB, na.rm=TRUE),
    ab=sum(AB,na.rm=TRUE)
  )

batters %>%
  filter(ab>100) %>%
  ggplot(mapping=aes(x=ab,y=ba)) +
    geom_point() +
    geom_smooth(se=FALSE)


batters %>%
  arrange(desc(ba))

not_cancelled %>% 
  group_by(year, month, day) %>%
  summarize(
    avg_delay1=mean(arr_delay),
    avg_delay2=mean(arr_delay[arr_delay>0])
  )

not_cancelled %>%
  group_by(dest) %>%
  summarize(distance_sd=sd(distance)) %>%
  arrange(desc(distance_sd))

not_cancelled %>%
  group_by(year,month,day) %>%
  summarise(
    first=min(dep_time),
    last=max(dep_time)
  )

not_cancelled %>%
  group_by(year, month,day) %>%
  summarize(
    first_dep=first(dep_time),
    last_dep=last(dep_time)
  )

carr <- not_cancelled %>%
  group_by(dest)  %>%
  summarise(carriers=n_distinct(carrier)) %>%
  arrange(desc(carriers))

a <- not_cancelled %>%
  count(dest)
  
not_cancelled %>%
  count(tailnum, wt=distance)
 
not_cancelled %>%
  group_by(year,month, day) %>%
  summarise(n_earl=sum(dep_time<500))

not_cancelled %>%
  group_by(year,month,day) %>%
  summarize(hour_prop=mean(arr_delay>60)) %>%
  ggplot(mapping=aes(x=year,y=hour_prop)) +
  geom_point()
  

daily <- group_by(flights,year,month,day)
(per_day <- summarise(daily,flights=n()))
(per_month <- summarise(per_day,flights=sum(flights)))
(per_year <- summarise(per_month,flights=sum(flights)))

daily %>%
  ungroup() %>%
  summarise(flights=n())

flights %>%
  group_by(year,month) %>%
  ggplot(mapping=aes(x=air_time, y=arr_delay)) +
  geom_point()
  

