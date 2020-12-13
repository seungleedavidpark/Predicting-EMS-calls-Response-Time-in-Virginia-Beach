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
library(FNN)

# Set up plot and map themes
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
    get.knnx(measureTo_Matrix, measureFrom_Matrix, k)$nn.dist
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

# kloat color palettes
palette5 <- c("#03254c","#1167b1","#187bcd","#2a9df4","#d0efff")
palette4 <- c("#3a7d7c","#ffa137","#ff4400","#065125")
palette2 <- c("#03254c","#187bcd")
palette1 <- c("#03254c")

my_token <- "pk.eyJ1IjoiZ2F1bHQzNCIsImEiOiJja2ZsbWd5cm8xNDBsMnlwajMzbW15c2Y0In0.nZ9siGKFAjMx_JQVEzeOtg"

# load main dataset and make it an SF object
main_ems <- read.csv("Main_VaBeach_EMS_2017_18.csv")
main_ems.sf <- main_ems %>%
  na.omit() %>%
  st_as_sf(coords = c("X", "Y"), crs = 4326, agr = "constant") %>%
  st_transform('EPSG:6595')

# create time bins (30 min and 60 min)
# create days and weeks
# create ResponseTime from "CallDateandTime" to "OnSceneDateandTime" 
# create "time_of_day" (6 hour periods)
# create "times" (3 hour periods)
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

# create call volume column
vol_count_dat <- main_ems.sf %>%
  st_drop_geometry() %>%
  group_by(Date, times) %>%
  summarise(CallVolume = n()) %>%
  unite(Date_time, c(Date, times), sep = " ", remove = FALSE)

# join the volume data file to main dataframe
main_ems.sf <-
  left_join(main_ems.sf, vol_count_dat, by="Date_time")

# plot call volume
ggplot(main_ems.sf, aes(x=CallVolume, y=ResponseTime)) +
  geom_point(size = .75, colour = "darkblue") +
  plotTheme


# load VB boundary census tracts
vb_boundary <-
  st_read("https://gismaps.vbgov.com/arcgis/rest/services/Basemaps/Administrative_Boundaries/MapServer/0/query?where=1%3D1&outFields=*&outSR=4326&f=json") %>%
  st_transform('EPSG:6595')

mapview::mapview(vb_boundary)

vb_tracts <- 
  st_read("https://opendata.arcgis.com/datasets/82ada480c5344220b2788154955ce5f0_2.geojson") %>%
  subset(OBJECTID!= 22) %>%
  st_transform('EPSG:6595')

# create fishnet
vb_fishnet <- 
  st_make_grid(vb_boundary,
               cellsize = 1000, 
               square = FALSE) %>%
  .[vb_boundary] %>% 
  st_sf() %>%
  mutate(uniqueID = rownames(.))

#load ems stations hospitals and fire stations
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

# create nn features
main_ems.sf <- main_ems.sf %>%
  mutate(ems_station_nn = nn_function(st_coordinates(main_ems.sf), st_coordinates(ems_stations), 1),
         hospitals_nn = nn_function(st_coordinates(main_ems.sf), st_coordinates(hospitals), 1),
         fire_stations_nn = nn_function(st_coordinates(main_ems.sf), st_coordinates(fire_stations), 1))
  
# create EMS calls on fishnet
vb_ems_fishnet <- 
  dplyr::select(main_ems.sf) %>%    
  mutate(countEMS = 1,
  ) %>%    
  aggregate(., vb_fishnet, sum) %>%    
  mutate(countEMS = replace_na(countEMS, 0),
         uniqueID = rownames(.),
         cvID = sample(round(nrow(vb_fishnet) / 24),          
                       size=nrow(vb_fishnet), replace = TRUE))

# map fishnet of EMS calls in VB
ggplot() +
  geom_sf(data = vb_ems_fishnet, aes(fill = countEMS), color = NA) +
  scale_fill_viridis() +
  labs(title = "Fishnet of EMS calls in Virginia Beach")


# mapping all calls response time 
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


# base plots for 4 time bins - create 4 maps per days by changing `dotw` ="Mon"
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

# map Response Time of EMS calls by days of the week and time of the day
ggplot(data = main_ems.sf %>%
         group_by(dotw, time_of_day) %>%
         summarise(meanResponeTime = mean(ResponseTime, na.rm=TRUE))) +
  geom_bar(aes(x=meanResponeTime, y=dotw, fill = time_of_day), stat="identity", position=position_dodge())+
  scale_fill_manual(values = palette5) +
  labs(title="Response Time of EMS calls by days of the week and time of the day",
       x="ResponseTime", 
       y="Day of the week")+
  plotTheme

# map meanResponseTime by CallPriority
ggplot(data = main_ems.sf %>%
         group_by(CallPriority) %>%
         summarise(meanResponeTime = mean(ResponseTime, na.rm=TRUE))) +
  geom_bar(aes(x=CallPriority, y=meanResponeTime, fill=CallPriority), stat="identity", position=position_dodge(), show.legend = FALSE) +
  plotTheme



# Load weather data and create features
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

# join weather data to the main dataframe
main_ems.sf <-
  left_join(main_ems.sf, vb_weather, by="interval60")

# plot weather Precipitation, Temperature, Humidity data
grid.arrange(
  ggplot(vb_weather, aes(interval60,Precipitation)) + geom_line(aes(),) + 
    labs(title="Percipitation", x="Hour", y="Precipitation") + theme(legend.position = "none"),
  ggplot(vb_weather, aes(interval60,Temperature)) + geom_line(aes(),) + 
    labs(title="Temperature", x="Hour", y="Temperature") + theme(legend.position = "none"),
  ggplot(vb_weather, aes(interval60,Humidity)) + geom_line(aes(),) + 
    labs(title="Humidity", x="Hour", y="Humidity")  + theme(legend.position = "none"),
  top="Weather Data - Virginia Beach - January to August, 2017")

# plot precipitation and response time
ggplot(main_ems.sf, aes(x=Precipitation, y=ResponseTime)) +
  geom_point(size = .75, colour = "darkblue") +
  plotTheme

# plot temperature and response time
ggplot(main_ems.sf, aes(x=Temperature, y=ResponseTime)) +
  geom_point(size = .75, colour = "darkblue") +
  plotTheme

# plot wind speed and response time
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

#plot snow and heavy rain days
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


# histogram of the ResponseTime
ggplot(main_ems.sf, aes(x=ResponseTime)) +
  geom_histogram(binwidth=1, fill = "darkblue") +
  xlim(0, 50) +
  labs(title="Histogram of all Response Time of EMS calls",
       x="ResponseTime", 
       y="Count")+
  plotTheme  


# time features -> dotw,6hr/3hr bins (time of the day), week, weekday/weekend, season, holiday
# environmental features -> precipitation, humidity, temperature, snow, heavyrain, hurricane, thunderstorm
# spatial features -> distance to ems stations, fire stations, hospitals, tracts/voronoi/neighborhood, elevation
# EMS system features -> call volume, call priority, rescue squad/crew, calls for each rescue squad


### RescueSquad.Number
# MR: marine response team -> incident was in the water
# ECOMM: means dispatcher needed to notify supervisors
# starts with "E" supervisor of the rescue squad needed
# numbers which end with R or P are specific ambulances
# - "R" means thay are EMT, basic life suppport
# - "P" staff with a paramedic
# MCIT: mass casualty incident team

### Ending with S, starting with MRTK, Starting with E and endin with P, HOLD, EMSOPS, 
### start with MED, starts with U, starts with RB(RB04), ECH, AIRMED, stars with L, Starts with Z, 
### ends with DR, starts with INS, starts with JTSKI, starts with CART, starts with TAC, BAT, SPEC, 
### NE, BKTEM, SQ, FB, ends with S  

#

# create a voronoi diagram for the EMS stations

main_ems.sf$advanced_life_support <- ifelse (
  (
  endsWith(main_ems.sf$RescueSquad.Number, 'S') |
    startsWith(main_ems.sf$RescueSquad.Number, 'Z') |
    endsWith(main_ems.sf$RescueSquad.Number, 'P') |
    startsWith(main_ems.sf$RescueSquad.Number, 'MED')
  ), 
  "Advanced Life Support", "Not Advanced Life Support"
  )

main_ems.sf$b_advanced_life_support <- ifelse (
  (
    endsWith(main_ems.sf$RescueSquad.Number, 'S') |
      startsWith(main_ems.sf$RescueSquad.Number, 'Z') |
      endsWith(main_ems.sf$RescueSquad.Number, 'P') |
      startsWith(main_ems.sf$RescueSquad.Number, 'MED')
  ), 
  1, 0
)

ggplot(data = main_ems.sf %>%
         group_by(advanced_life_support) %>%
         drop_na(advanced_life_support) %>%
         summarise(meanResponeTime = mean(ResponseTime, na.rm=TRUE))) +
  geom_bar(aes(x=advanced_life_support, y=meanResponeTime, fill=advanced_life_support), stat="identity", position=position_dodge(), show.legend = FALSE) +
  scale_fill_manual(values = palette2) + 
  labs(title= "Average Response Time Difference of EMS Calls requiring Advanced Life Support") +
  plotTheme

main_ems.sf$chief_dispatch <- ifelse (
  (
    startsWith(main_ems.sf$RescueSquad.Number, 'ECH') |
      (((startsWith(main_ems.sf$RescueSquad.Number, 'CAR')) == TRUE) & ((startsWith(main_ems.sf$RescueSquad.Number, 'CART')) == FALSE)) |
      startsWith(main_ems.sf$RescueSquad.Number, 'BAT')
  ), 
  "Chief Dispatched", "Cheif not Dispatched"
)

main_ems.sf$b_chief_dispatch <- ifelse (
  (
    startsWith(main_ems.sf$RescueSquad.Number, 'ECH') |
      (((startsWith(main_ems.sf$RescueSquad.Number, 'CAR')) == TRUE) & ((startsWith(main_ems.sf$RescueSquad.Number, 'CART')) == FALSE)) |
      startsWith(main_ems.sf$RescueSquad.Number, 'BAT')
  ), 
  1, 0
)

ggplot(data = main_ems.sf %>%
         group_by(chief_dispatch) %>%
         drop_na(chief_dispatch) %>%
         summarise(meanResponeTime = mean(ResponseTime, na.rm=TRUE))) +
  geom_bar(aes(x=chief_dispatch, y=meanResponeTime, fill=chief_dispatch), stat="identity", position=position_dodge(), show.legend = FALSE) +
  scale_fill_manual(values = palette2) + 
  labs(title= "Average Response Time Difference of EMS Calls needing a Chief") +
  plotTheme

main_ems.sf$special_event <- ifelse (
  (
    main_ems.sf$RescueSquad.Number == 'BKTEAM' |
      main_ems.sf$RescueSquad.Number == 'EMSOPS' |
      startsWith(main_ems.sf$RescueSquad.Number, 'CART')
  ), 
  "Special Event", "Not Special Event"
)

main_ems.sf$b_special_event <- ifelse (
  (
    main_ems.sf$RescueSquad.Number == 'BKTEAM' |
      main_ems.sf$RescueSquad.Number == 'EMSOPS' |
      startsWith(main_ems.sf$RescueSquad.Number, 'CART')
  ), 
  1, 0
)

ggplot(data = main_ems.sf %>%
         group_by(special_event) %>%
         drop_na(special_event) %>%
         summarise(meanResponeTime = mean(ResponseTime, na.rm=TRUE))) +
  geom_bar(aes(x=special_event, y=meanResponeTime, fill=special_event), stat="identity", position=position_dodge(), show.legend = FALSE) +
  scale_fill_manual(values = palette2) + 
  labs(title= "Average Response Time Difference of EMS Calls at Special Events") +
  plotTheme

main_ems.sf$air_dispatch <- ifelse (
  (
    main_ems.sf$RescueSquad.Number == 'AIRMED'
  ), 
  "Air unit", "Not an Air Unit"
)

main_ems.sf$b_air_dispatch <- ifelse (
  (
    main_ems.sf$RescueSquad.Number == 'AIRMED'
  ), 
  1, 0
)

ggplot(data = main_ems.sf %>%
         group_by(air_dispatch) %>%
         drop_na(air_dispatch) %>%
         summarise(meanResponeTime = mean(ResponseTime, na.rm=TRUE))) +
  geom_bar(aes(x=air_dispatch, y=meanResponeTime, fill=air_dispatch), stat="identity", position=position_dodge(), show.legend = FALSE) +
  scale_fill_manual(values = palette2) + 
  labs(title= "Average Response Time Difference of EMS Calls Using Helicopter Ambulances") +
  plotTheme

main_ems.sf$hold_dispatch <- ifelse (
  (
    startsWith(main_ems.sf$RescueSquad.Number, 'HOLD')
  ), 
  "Held By Dispatcher", "Not Held by Dispatcher"
)

main_ems.sf$b_hold_dispatch <- ifelse (
  (
    startsWith(main_ems.sf$RescueSquad.Number, 'HOLD')
  ), 
  1, 0
)

ggplot(data = main_ems.sf %>%
         group_by(hold_dispatch) %>%
         drop_na(hold_dispatch) %>%
         summarise(meanResponeTime = mean(ResponseTime, na.rm=TRUE))) +
  geom_bar(aes(x=hold_dispatch, y=meanResponeTime, fill=hold_dispatch), stat="identity", position=position_dodge(), show.legend = FALSE) +
  scale_fill_manual(values = palette2) +
  labs(title= "Average Response Time Difference of EMS Calls on HOLD by Dispatcher") +
  plotTheme

main_ems.sf$water_dispatch <- ifelse (
  (
    startsWith(main_ems.sf$RescueSquad.Number, 'FBOA') |
      startsWith(main_ems.sf$RescueSquad.Number, 'RB') |
      startsWith(main_ems.sf$RescueSquad.Number, 'MRTK') |
      startsWith(main_ems.sf$RescueSquad.Number, 'JTSKI')
      
  ), 
  "Water Unit", "Not a Water Unit"
)

main_ems.sf$b_water_dispatch <- ifelse (
  (
    startsWith(main_ems.sf$RescueSquad.Number, 'FBOA') |
      startsWith(main_ems.sf$RescueSquad.Number, 'RB') |
      startsWith(main_ems.sf$RescueSquad.Number, 'MRTK') |
      startsWith(main_ems.sf$RescueSquad.Number, 'JTSKI')
    
  ), 
  1, 0
)

ggplot(data = main_ems.sf %>%
         group_by(water_dispatch) %>%
         drop_na(water_dispatch) %>%
         summarise(meanResponeTime = mean(ResponseTime, na.rm=TRUE))) +
  geom_bar(aes(x=water_dispatch, y=meanResponeTime, fill=water_dispatch), stat="identity", position=position_dodge(), show.legend = FALSE) +
  scale_fill_manual(values = palette2) + 
  labs(title= "Average Response Time Difference of Marine EMS Calls") +
  plotTheme

main_ems.sf$mass_casualty_dispatch <- ifelse (
  (
    startsWith(main_ems.sf$RescueSquad.Number, 'MCI') 
  ), 
  "Mass Casualty Incident", "Not a Mass Casualty Incident"
)

main_ems.sf$b_mass_casualty_dispatch <- ifelse (
  (
    startsWith(main_ems.sf$RescueSquad.Number, 'MCI') 
  ), 
  1, 0
)

ggplot(data = main_ems.sf %>%
         group_by(mass_casualty_dispatch) %>%
         drop_na(mass_casualty_dispatch) %>%
         summarise(meanResponeTime = mean(ResponseTime, na.rm=TRUE))) +
  geom_bar(aes(x=mass_casualty_dispatch, y=meanResponeTime, fill=mass_casualty_dispatch), stat="identity", position=position_dodge(), show.legend = FALSE) +
  scale_fill_manual(values = palette2) + 
  labs(title= "Average Response Time Difference of Mass Casualty EMS Calls") +
  plotTheme

main_ems.sf$fire_dispatch <- ifelse (
  (
    startsWith(main_ems.sf$RescueSquad.Number, 'L') |
      (startsWith(main_ems.sf$RescueSquad.Number, 'E') & ((startsWith(main_ems.sf$RescueSquad.Number, 'ECH') == FALSE) & (main_ems.sf$RescueSquad.Number != 'EMSOPS'))) |
      (startsWith(main_ems.sf$RescueSquad.Number, 'T') & (startsWith(main_ems.sf$RescueSquad.Number, 'TAC') == FALSE)) |
      endsWith(main_ems.sf$RescueSquad.Number, 'F') |
      startsWith(main_ems.sf$RescueSquad.Number, 'NE')
  ), 
  "Fire Team Dispactched", "Fire Team not Dispatched"
)

main_ems.sf$b_fire_dispatch <- ifelse (
  (
    startsWith(main_ems.sf$RescueSquad.Number, 'L') |
      (startsWith(main_ems.sf$RescueSquad.Number, 'E') & ((startsWith(main_ems.sf$RescueSquad.Number, 'ECH') == FALSE) & (main_ems.sf$RescueSquad.Number != 'EMSOPS'))) |
      (startsWith(main_ems.sf$RescueSquad.Number, 'T') & (startsWith(main_ems.sf$RescueSquad.Number, 'TAC') == FALSE)) |
      endsWith(main_ems.sf$RescueSquad.Number, 'F') |
      startsWith(main_ems.sf$RescueSquad.Number, 'NE')
  ), 
  1, 0
)

ggplot(data = main_ems.sf %>%
         group_by(fire_dispatch) %>%
         drop_na(fire_dispatch) %>%
         summarise(meanResponeTime = mean(ResponseTime, na.rm=TRUE))) +
  geom_bar(aes(x=fire_dispatch, y=meanResponeTime, fill=fire_dispatch), stat="identity", position=position_dodge(), show.legend = FALSE) +
  scale_fill_manual(values = palette2) + 
  labs(title= "Average Response Time Difference of Fire-related EMS Calls") +
  plotTheme


