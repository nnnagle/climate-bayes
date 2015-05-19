library(dplyr)
library(ggplot2)
library(scales)
load('~/Dropbox/git_root/climate-bayes/data/CRU.Rdata')

# filter to North America
# Extract distinct coords in north america
inst.meta <- inst.meta %>% mutate(lon=-lon) %>% 
  filter(lat < 70 & lat>25 & lon < -50 & lon>-165 & elev!=-999)

inst.meta %>%
  ggplot(aes(x=lon, y=lat)) + geom_point(aes(color=elev))+coord_map(projection = 'mollweide')

stations.df2 <- stations.df[stations.df$station %in% inst.meta$station,]

stations.df2 <- inner_join( stations.df, inst.meta) %>% filter(elev>-900)

stations.df2 %>% filter(year==1900) %>%
  ggplot(aes(x=lon, y=lat, color=sat))+geom_point()+facet_wrap(~month)+
  scale_color_gradient2(low=muted('blue'), high=muted('red'))

# Plot time series for 5 degree cells
stations.df2 %>% mutate(lat=factor(round(lat/10)*10, levels=rev(c(30,40,50,60,70))), lon=round(lon/10)*10) %>% 
  filter(year>1850) %>% group_by(lat,lon)%>%
  ggplot(aes(x=year, y=sat))+geom_smooth(formula=y~s(x,k=30), method='gam') + facet_grid(lat~lon)
  
stations.df2 %>% mutate(lat=factor(round(lat/10)*10, levels=rev(c(30,40,50,60,70))), lon=round(lon/10)*10) %>% 
  filter(year>1850) %>% group_by(lat,lon, year)%>%summarize(sat=mean(sat)) %>% 
  ggplot(aes(x=year,y=sat))+geom_line()+facet_grid(lat~lon)
  
stations.df2 %>% mutate(lat=round(lat/5)*5, lon=round(lon/5)*5) %>% 
  filter(year>1850) %>% group_by(lat,lon, month)%>%summarize(sat=mean(sat)) %>%
  ggplot() + geom_tile(aes(x=lon, y=lat, fill=sat)) + facet_wrap(~month) +
  scale_fill_gradient2(low=muted('blue'), high=muted('red'))

stations.df2 %>% mutate(lat=factor(round(lat/10)*10, levels=rev(c(30,40,50,60,70))), lon=round(lon/10)*10) %>% 
  filter(year>1850) %>% group_by(lat,lon, month)%>%summarize(sat=mean(sat)) %>%  
  ggplot(aes(x=month,y=sat))+geom_point()+facet_grid(lat~lon)


# Plot station count over time
stations.df2 %>% filter(year>1950& year<2000) %>% group_by(year) %>% summarize(m=n()) %>%
  ggplot()+geom_line(aes(x=year, y=m))


# predict each observation so that we can calculate anomaly
# Stolen from here: http://stackoverflow.com/questions/24356683/apply-grouped-model-back-onto-data
f <- function(df){
  mod <- gam(formula=sat~s(lon, lat,k=1000)+elev, data=df, subset=year>=1950 & year<=1980)
  normal <- predict(mod, newdata=df)
  data.frame(df, normal)
}
temp <- stations.df2 %>% filter(elev>-999) %>% group_by(month) %>%
  do(f(.))

temp %>% ungroup %>% filter(year==1960& elev>-900) %>% mutate(anom=sat-normal) %>%
  ggplot(aes(x=lon, y=lat, color=normal))+geom_point()+facet_wrap(~month)+
  scale_color_gradient2(low=muted('blue'), high=muted('red'))

temp2 <- stations.df2 %>% filter(start<=1950 & last >=1980 & year>=1950 & year<=1980) %>% group_by(month, station) %>%
  summarize(normal=mean(sat)) %>% inner_join(stations.df2)

temp2 %>% ungroup %>% filter(year==1960& elev>-900) %>% mutate(anom=sat-normal) %>%
  ggplot(aes(x=lon, y=lat, color=normal))+geom_point()+facet_wrap(~month)+
  scale_color_gradient2(low=muted('blue'), high=muted('red'))

#######################################################################
# Calculate annual average temperature anomaly
ann.temp <- temp2 %>% ungroup %>% group_by(station) %>% arrange(year, month) %>% 
  mutate(year2=lead(year,4)) %>% 
  group_by(station, year2) %>%
  summarize(n=n(), sat=mean(sat-normal)) %>% filter(n==12) %>%
  inner_join(inst.meta)

# Calculate MAM temp
spr.temp <- temp2 %>% ungroup %>% group_by(station) %>% arrange(year, month) %>% 
  mutate(year2=lead(year,4)) %>% 
  group_by(station, year2) %>% 
  filter(month %in% c('mar','apr','may')) %>%
  summarize(n=n(), sat=mean(sat-normal)) %>% filter(n==3) %>%
  inner_join(inst.meta)
# Calculate JJA temp
sum.temp <- temp2 %>% ungroup %>% group_by(station) %>% arrange(year, month) %>% 
  mutate(year2=lead(year,4)) %>% 
  group_by(station, year2) %>% 
  filter(month %in% c('jun','jul','aug')) %>%
summarize(n=n(), sat=mean(sat-normal)) %>% filter(n==3) %>%
  inner_join(inst.meta)

save(ann.temp, file='~/Dropbox/git_root/climate-bayes/data/indiv.station.Rdata')
save(spr.temp, file='~/Dropbox/git_root/climate-bayes/data/indiv.station.spring.Rdata')
save(sum.temp, file='~/Dropbox/git_root/climate-bayes/data/indiv.station.sum.Rdata')
rm(list=ls())
