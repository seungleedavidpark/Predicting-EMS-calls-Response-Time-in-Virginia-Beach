##### 0. Set up ######
library(tidyverse)
library(tidycensus)
library(dplyr)
library(sf)
library(mapview)
library(viridis)
library(riem)
library(lubridate)
library(gridExtra)
library(leaflet)
library(shiny)
library(mapboxapi)
library(riem)
library(measurements)
library(mapboxapi)

plotTheme <- theme(text = element_text( family = "Avenir", color = "black"),
                   plot.title =element_text(size=12),
                   plot.subtitle = element_text(size=8),
                   plot.caption = element_text(size = 6),
                   axis.text.x = element_text(size = 8, angle = 45, hjust = 1),
                   axis.title.x = element_text(size = 12),
                   axis.text.y = element_text(size = 8),
                   axis.title.y = element_text(size = 12),
                   # Set the entire chart region to blank
                   panel.background=element_blank(),
                   plot.background=element_blank(),
                   #panel.border=element_rect(colour="#F0F0F0"),
                   # Format the grid
                   panel.grid.major=element_line(colour="#565050",size=.2),
                   axis.ticks=element_blank())

mapTheme <- theme(text = element_text( family = "Avenir", color = "black"),
                  plot.title =element_text(size=20),
                  plot.subtitle = element_text(size=7),
                  plot.caption = element_text(size = 5),
                  axis.line=element_blank(),
                  axis.text.x=element_blank(),
                  axis.text.y=element_blank(),
                  axis.ticks=element_blank(),
                  axis.title.x=element_blank(),
                  axis.title.y=element_blank(),
                  panel.background=element_blank(),
                  panel.border=element_blank(),
                  panel.grid.major=element_line(colour = 'transparent'),
                  panel.grid.minor=element_blank(),
                  legend.direction = "vertical", 
                  legend.position = "right",
                  plot.margin = margin(1, 1, 1, 1, 'cm'),
                  legend.key.height = unit(1, "cm"), legend.key.width = unit(0.2, "cm"))

qBr <- function(df, variable, rnd) {
  if (missing(rnd)) {
    as.character(quantile(round(df[[variable]],0),
                          c(.01,.2,.4,.6,.8), na.rm=T))
  } else if (rnd == FALSE | rnd == F) {
    as.character(formatC(quantile(df[[variable]]), digits = 3),
                 c(.01,.2,.4,.6,.8), na.rm=T)
  }
}


q5 <- function(variable) {as.factor(ntile(variable, 5))}

nn_function <- function(measureFrom,measureTo,k) {
  measureFrom_Matrix <-
    as.matrix(measureFrom)
  measureTo_Matrix <-
    as.matrix(measureTo)
  nn <-   
    get.knnx(measureTo, measureFrom, k)$nn.dist
  output <-
    as.data.frame(nn) %>%
    rownames_to_column(var = "thisPoint") %>%
    gather(points, point_distance, V1:ncol(.)) %>%
    arrange(as.numeric(thisPoint)) %>%
    group_by(thisPoint) %>%
    summarize(pointDistance = mean(point_distance)) %>%
    arrange(as.numeric(thisPoint)) %>% 
    dplyr::select(-thisPoint) %>%
    pull()
  
  return(output)  
}


palette5 <- c("#03254c","#1167b1","#187bcd","#2a9df4","#d0efff")
palette4 <- c("#3a7d7c","#ffa137","#ff4400","#065125")
palette2 <- c("#03254c","#187bcd")
palette1 <- c("#03254c")

my_token <- "pk.eyJ1IjoiZ2F1bHQzNCIsImEiOiJja2ZsbWd5cm8xNDBsMnlwajMzbW15c2Y0In0.nZ9siGKFAjMx_JQVEzeOtg"

#load main dataset and make it an SF object
main_ems <- read.csv("Main_VaBeach_EMS_2017_18.csv")
main_ems.sf <- main_ems %>%
  na.omit() %>%
  st_as_sf(coords = c("X", "Y"), crs = 4326, agr = "constant") %>%
  st_transform('EPSG:6595')

### create time bins (30 min and 60 min)/create time duration from "CallDateandTime" to "OnSceneDateandTime" and "time_of_day" column
main_ems.sf <- main_ems.sf %>%
  mutate(interval60 = floor_date(mdy_hm(CallDateandTime), unit = "60 mins"),
         interval30 = floor_date(mdy_hm(CallDateandTime), unit = "30 mins"),
         week = week(interval60),
         dotw = wday(interval60, label=TRUE)) %>%
  mutate(ResponseTime =  as.numeric(difftime(mdy_hm(OnSceneDateandTime), mdy_hm(CallDateandTime), units = "min"))) %>%
  mutate(time_of_day = case_when(hour(interval60) >= 0 & hour(interval60) < 6 ~ "Overnight",
                                 hour(interval60) >= 6 & hour(interval60) < 12 ~ "Morning",
                                 hour(interval60) >= 12 & hour(interval60) < 18 ~ "Afternoon",
                                 hour(interval60) >= 18 & hour(interval60) <= 24 ~ "Evening")) %>%
  mutate(times = case_when(hour(interval60) >= 0 & hour(interval60) < 3 ~ "one-three",
                           hour(interval60) >= 3 & hour(interval60) < 6 ~ "three-six",
                           hour(interval60) >= 6 & hour(interval60) < 9 ~ "six-nine",
                           hour(interval60) >= 9 & hour(interval60) < 12 ~ "nine-twelve",
                           hour(interval60) >= 12 & hour(interval60) < 15 ~ "twelve-fifteen",
                           hour(interval60) >= 15 & hour(interval60) < 18 ~ "fifteen-eighteen",
                           hour(interval60) >= 18 & hour(interval60) < 21 ~ "eighteen-twentyone",
                           hour(interval60) >= 21 & hour(interval60) <= 24 ~ "twentyone-twentyfour")) %>%
  mutate(weekend = ifelse(dotw %in% c("Sun", "Sat"), "Weekend", "Weekday")) %>%
  mutate(Date = substring(CallDateandTime,1,6)) %>%
  unite(Date_timeofday, c(Date, time_of_day), sep = " ", remove = FALSE) %>%
  unite(Date_time, c(Date, times), sep = " ", remove = FALSE)
  
### create call volume column
vol_count_dat <- main_ems.sf %>%
  st_drop_geometry() %>%
  group_by(Date, times) %>%
  summarise(CallVolume = n()) %>%
  unite(Date_time, c(Date, times), sep = " ", remove = FALSE)

main_ems.sf <-
  left_join(main_ems.sf, vol_count_dat, by="Date_time")

### 

ggplot(main_ems.sf, aes(x=CallVolume, y=ResponseTime)) +
  geom_point(size = .75, colour = "darkblue") +
  plotTheme


### create fishnet
vb_boundary <-
  st_read("https://gismaps.vbgov.com/arcgis/rest/services/Basemaps/Administrative_Boundaries/MapServer/0/query?where=1%3D1&outFields=*&outSR=4326&f=json") %>%
  st_transform('EPSG:6595')

mapview::mapview(vb_boundary)

vb_tracts <- 
  st_read("https://opendata.arcgis.com/datasets/82ada480c5344220b2788154955ce5f0_2.geojson") %>%
  subset(OBJECTID!= 22) %>%
  st_transform('EPSG:6595')

vb_fishnet <- 
  st_make_grid(vb_boundary,
               cellsize = 1000, 
               square = FALSE) %>%
  .[vb_boundary] %>% 
  st_sf() %>%
  mutate(uniqueID = rownames(.))


### create EMS calls on fishnet
vb_ems_fishnet <- 
  dplyr::select(main_ems.sf) %>%    
  mutate(countEMS = 1) %>%    
  aggregate(., vb_fishnet, sum) %>%    
  mutate(countEMS = replace_na(countEMS, 0),
         uniqueID = rownames(.),
         cvID = sample(round(nrow(vb_fishnet) / 24),          
                       size=nrow(vb_fishnet), replace = TRUE))

ggplot() +
  geom_sf(data = vb_ems_fishnet, aes(fill = countEMS), color = NA) +
  scale_fill_viridis() +
  labs(title = "Fishnet of EMS calls in Virginia Beach")


####mapping all calls response time#######
ResponseTime_net <- 
  main_ems.sf %>%
  dplyr::select(ResponseTime, dotw, time_of_day) %>%
  mutate(ResponseTime = as.numeric(ResponseTime)) %>% 
  dplyr::select(ResponseTime) %>%
  aggregate(., vb_fishnet, mean) %>%
  mutate(ResponseTime = replace_na(ResponseTime, 0))

ggplot() + 
  geom_sf(data = vb_boundary, fill = "black") +
  geom_sf(data = ResponseTime_net %>%
            dplyr::filter(ResponseTime > 0), color = NA, aes(fill = ResponseTime)) +
  scale_fill_viridis_c(option="plasma") +
  geom_sf(data = ems_stations, color="white") +
  labs(title= "Response Time of EMS calls by fishnet") +
  mapTheme


#base plots for 4 time bins - create 4 maps per days by changing `dotw` ="Mon"
ggplot() + 
  geom_sf(data = vb_boundary, fill = "black") +
  geom_sf(data = net <- main_ems.sf %>% 
                          filter(`dotw` == "Sat" & `time_of_day` == "Overnight") %>%
                          mutate(ResponseTime = as.numeric(ResponseTime)) %>% 
                          dplyr::select(ResponseTime) %>%
                          aggregate(., vb_fishnet, mean) %>%
                          mutate(ResponseTime = replace_na(ResponseTime, 0)) %>%
                          dplyr::filter(ResponseTime > 0), color = NA, aes(fill = ResponseTime)) +
  scale_fill_viridis_c(option="plasma", limits=c(0,45), breaks=c(15,30,45)) +
  geom_sf(data = ems_stations, color="white", size =1, shape = 23, fill = "white") +
  labs(title= "Saturday Overnight Response Time") +
  mapTheme
  
ggplot() + 
  geom_sf(data = vb_boundary, fill = "black") +
  geom_sf(data = net <- main_ems.sf %>% 
            filter(`dotw` == "Sat" & `time_of_day` == "Morning") %>%
            mutate(ResponseTime = as.numeric(ResponseTime)) %>% 
            dplyr::select(ResponseTime) %>%
            aggregate(., vb_fishnet, mean) %>%
            mutate(ResponseTime = replace_na(ResponseTime, 0)) %>%
            dplyr::filter(ResponseTime > 0), color = NA, aes(fill = ResponseTime)) +
  scale_fill_viridis_c(option="plasma", limits=c(0,45), breaks=c(15,30,45)) +
  geom_sf(data = ems_stations, color="white", size =1, shape = 23, fill = "white") +
  labs(title= "Saturday Morning Response Time") +
  mapTheme

ggplot() + 
  geom_sf(data = vb_boundary, fill = "black") +
  geom_sf(data = net <- main_ems.sf %>% 
            filter(`dotw` == "Sat" & `time_of_day` == "Afternoon") %>%
            mutate(ResponseTime = as.numeric(ResponseTime)) %>% 
            dplyr::select(ResponseTime) %>%
            aggregate(., vb_fishnet, mean) %>%
            mutate(ResponseTime = replace_na(ResponseTime, 0)) %>%
            dplyr::filter(ResponseTime > 0), color = NA, aes(fill = ResponseTime)) +
  scale_fill_viridis_c(option="plasma", limits=c(0,45), breaks=c(15,30,45)) +
  geom_sf(data = ems_stations, color="white", size =1, shape = 23, fill = "white") +
  labs(title= "Saturday Afternoon Response Time") +
  mapTheme

ggplot() + 
  geom_sf(data = vb_boundary, fill = "black") +
  geom_sf(data = net <- main_ems.sf %>% 
            filter(`dotw` == "Sat" & `time_of_day` == "Evening") %>%
            mutate(ResponseTime = as.numeric(ResponseTime)) %>% 
            dplyr::select(ResponseTime) %>%
            aggregate(., vb_fishnet, mean) %>%
            mutate(ResponseTime = replace_na(ResponseTime, 0)) %>%
            dplyr::filter(ResponseTime > 0), color = NA, aes(fill = ResponseTime)) +
  scale_fill_viridis_c(option="plasma", limits=c(0,45), breaks=c(15,30,45)) +
  geom_sf(data = ems_stations, color="white", size =1, shape = 23, fill = "white") +
  labs(title= "Saturday Evening Response Time") +
  mapTheme






# weekend vs weekday response time
ggplot(data = main_ems.sf %>%
         group_by(dotw, time_of_day) %>%
         summarise(meanResponeTime = mean(ResponseTime, na.rm=TRUE))) +
  geom_bar(aes(x=meanResponeTime, y=dotw, fill = time_of_day), stat="identity", position=position_dodge())+
  scale_fill_manual(values = palette5) +
  labs(title="Response Time of EMS calls by days of the week and time of the day",
       x="ResponseTime", 
       y="Day of the week")+
  plotTheme


ggplot(data = main_ems.sf %>%
         group_by(CallPriority) %>%
         summarise(meanResponeTime = mean(ResponseTime, na.rm=TRUE))) +
  geom_bar(aes(x=CallPriority, y=meanResponeTime, fill=CallPriority), stat="identity", position=position_dodge(), show.legend = FALSE) +
  plotTheme



### 2.2 Load weather data
vb_weather <- 
  riem_measures(station = "NTU", date_start = "2017-01-01", date_end = "2018-03-01") %>%
  dplyr::select(valid, tmpf, p01i, sknt, relh)%>%
  replace(is.na(.), 0) %>%
  mutate(interval60 = ymd_h(substr(valid,1,13))) %>%
  mutate(week = week(interval60),
         dotw = wday(interval60, label=TRUE)) %>%
  group_by(interval60) %>%
  summarize(Temperature = max(tmpf),
            Precipitation = sum(p01i),
            Wind_Speed = max(sknt),
            Humidity = max(relh)) %>%
  mutate(Temperature = ifelse(Temperature == 0, 42, Temperature)) %>%
  mutate(SnowPresent = ifelse(Precipitation > 0.0 & Temperature < 32.0, "Snow", "NoSnow"),
         HeavyRain = ifelse(Precipitation > 0.5, "HeavyRain", "NoHeavyRain"))


##### plot weather data
grid.arrange(
  ggplot(vb_weather, aes(interval60,Precipitation)) + geom_line(aes(),) + 
    labs(title="Percipitation", x="Hour", y="Perecipitation") + theme(legend.position = "none"),
  ggplot(vb_weather, aes(interval60,Temperature)) + geom_line(aes(),) + 
    labs(title="Temperature", x="Hour", y="Temperature") + theme(legend.position = "none"),
  ggplot(vb_weather, aes(interval60,Humidity)) + geom_line(aes(),) + 
    labs(title="Humidity", x="Hour", y="Humidity")  + theme(legend.position = "none"),
  top="Weather Data - Virginia Beach - January to August, 2017")

main_ems.sf <-
  left_join(main_ems.sf, vb_weather, by="interval60")

#precipitation and response time
ggplot(main_ems.sf, aes(x=Precipitation, y=ResponseTime)) +
  geom_point(size = .75, colour = "darkblue") +
  plotTheme

#temperature and response time
ggplot(main_ems.sf, aes(x=Temperature, y=ResponseTime)) +
  geom_point(size = .75, colour = "darkblue") +
  plotTheme

#wind speed and response time
ggplot(main_ems.sf, aes(x=Wind_Speed, y=ResponseTime)) +
  geom_point(size = .75, colour = "darkblue") +
  plotTheme

grid.arrange(
  ggplot(main_ems.sf, aes(x=Precipitation, y=ResponseTime)) +
    geom_point(size = .75, colour = "darkblue") +
    plotTheme,
  ggplot(main_ems.sf, aes(x=Temperature, y=ResponseTime)) +
    geom_point(size = .75, colour = "darkblue") +
    plotTheme,
  ggplot(main_ems.sf, aes(x=Wind_Speed, y=ResponseTime)) +
    geom_point(size = .75, colour = "darkblue") +
    plotTheme
)

grid.arrange(
  ggplot(data = main_ems.sf %>%
         group_by(SnowPresent) %>%
         drop_na(SnowPresent) %>%
         summarise(meanResponeTime = mean(ResponseTime, na.rm=TRUE))) +
    geom_bar(aes(x=SnowPresent, y=meanResponeTime, fill=SnowPresent), stat="identity", position=position_dodge(), show.legend = FALSE) +
    scale_fill_manual(values = palette2) + 
    plotTheme,
    ggplot(data = main_ems.sf %>%
             group_by(HeavyRain) %>%
             drop_na(HeavyRain) %>%
             summarise(meanResponeTime = mean(ResponseTime, na.rm=TRUE))) +
      geom_bar(aes(x=HeavyRain, y=meanResponeTime, fill=HeavyRain), stat="identity", position=position_dodge(), show.legend = FALSE) +
      scale_fill_manual(values = palette2) + 
      plotTheme
)

#distance to ems stations and response time

ems_stations <- st_read('201127_ems_geocoded.shp') %>%
  st_transform(st_crs(vb_fishnet)) %>%
  mutate(OBJECTID = osm_id) %>%
  mutate(Legend = "ems_stations")

hospitals <- st_read('Hospitals__Virginia_.shp') %>% 
  dplyr::filter(City == 'Virginia Beach') %>%
  st_transform(st_crs(vb_fishnet)) %>%
  mutate(Legend = "hospitals")
#mapview(hospitals)

fire_stations <- st_read('Fire_Stations.shp') %>% 
  dplyr::filter(CITY == 'Virginia Beach') %>%
  st_transform(st_crs(vb_fishnet)) %>%
  mutate(Legend = "fire_stations")

ems_station_nn = 
  nn_function(st_coordinates(st_centroid(vb_fishnet)), ems_stations, 1)

