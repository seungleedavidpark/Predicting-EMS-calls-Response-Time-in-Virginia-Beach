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
                   axis.title.x = element_text(size = 8),
                   axis.text.y = element_text(size = 8),
                   axis.title.y = element_text(size = 8),
                   # Set the entire chart region to blank
                   panel.background=element_blank(),
                   plot.background=element_blank(),
                   #panel.border=element_rect(colour="#F0F0F0"),
                   # Format the grid
                   panel.grid.major=element_line(colour="#565050",size=.2),
                   axis.ticks=element_blank())

mapTheme <- theme(text = element_text( family = "Avenir", color = "black"),
                  plot.title =element_text(size=12),
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


palette5 <- c("#3a7d7c","#ffa137","#ff4400","#065125","#e6d1ab")
palette4 <- c("#3a7d7c","#ffa137","#ff4400","#065125")
palette2 <- c("#3a7d7c","#ffa137")
palette1 <- c("#065125")

my_token <- "pk.eyJ1IjoiZ2F1bHQzNCIsImEiOiJja2ZsbWd5cm8xNDBsMnlwajMzbW15c2Y0In0.nZ9siGKFAjMx_JQVEzeOtg"

##### 1.load and format the VB dataset #####
#load main dataset and make it an SF object
main_ems <- read.csv("Main_VaBeach_EMS_2017_18.csv")
main_ems.sf <- main_ems %>%
  na.omit() %>%
  st_as_sf(coords = c("X", "Y"), crs = 4326, agr = "constant") %>%
  st_transform('EPSG:6595')
#mapview::mapview(main_ems.sf)

### create time bins (30 min and 60 min)
main_ems.sf <- main_ems.sf %>%
  mutate(interval60 = floor_date(mdy_hm(CallDateandTime), unit = "60 mins"),
         interval30 = floor_date(mdy_hm(CallDateandTime), unit = "30 mins"),
         week = week(interval60),
         dotw = wday(interval60, label=TRUE))

### create time duration from "CallDateandTime" to "OnSceneDateandTime" and "time_of_day" column
main_ems.sf <- main_ems.sf %>%
  mutate(Call_to_OnScene =  difftime(mdy_hm(OnSceneDateandTime), mdy_hm(CallDateandTime), units = "min")) %>%
  mutate(time_of_day = case_when(hour(interval60) >= 0 & hour(interval60) < 6 ~ "Overnight",
                                 hour(interval60) >= 6 & hour(interval60) < 12 ~ "Morning",
                                 hour(interval60) >= 12 & hour(interval60) < 18 ~ "Afternoon",
                                 hour(interval60) >= 18 & hour(interval60) <= 24 ~ "Evening"))

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

test <- main_ems.sf %>%
  dplyr::filter(dotw == "Mon")
  

test_net <- 
  dplyr::select(test) %>% 
  mutate(Call_to_OnScene = as.numeric(test$Call_to_OnScene)) %>% 
  aggregate(., vb_fishnet, mean) %>%
  mutate(Call_to_OnScene = replace_na(Call_to_OnScene, 0),
         uniqueID = rownames(.),
         cvID = sample(round(nrow(vb_fishnet) / 24), size=nrow(vb_fishnet), replace = TRUE))


Call_to_OnScene_net <- 
  dplyr::select(test) %>% 
  mutate(Call_to_OnScene = (test$Call_to_OnScene)) %>% 
  aggregate(., vb_fishnet, mean) %>%
  mutate(Call_to_OnScene = replace_na(Call_to_OnScene, 0),
         uniqueID = rownames(.),
         cvID = sample(round(nrow(vb_fishnet) / 24), size=nrow(vb_fishnet), replace = TRUE))

ggplot() + 
  geom_sf(data = vb_boundary, fill = "black") +
  geom_sf(data = test_net %>%
            dplyr::filter(Call_to_OnScene > 0), color = NA, aes(fill = Call_to_OnScene)) +
  scale_fill_viridis_c() +
  geom_sf(data = ems_stations, color="white") +
  labs(title= "call to Scene on mondays") +
  mapTheme

##### 2.Load additional datasets #####
### 2.1 Load census (pop_density, med_age, race, poverty, income, education, commute)
selected_vars <- c("B01003_001E", # Total Population
                   "B02001_001E", # Estimate!!Total population by race -- ##let's double check that it's okay to use this as long as we justify it
                   "B02001_002E", # People describing themselves as "white alone"
                   "B02001_003E", # People describing themselves as "black" or "african-american" alone
                   "B15001_050E", # Females with bachelors degrees
                   "B15001_009E", # Males with bachelors degrees
                   "B19013_001E", # Median HH income
                   "B25058_001E", # Median rent
                   "B06012_002E", # Total poverty
                   "B08301_001E", # People who have means of transportation to work
                   "B08301_002E", # Total people who commute by car, truck, or van
                   "B08301_010E", # Total people who commute by public transportation"
                   "B03002_012E", # Estimate Total Hispanic or Latino by race
                   "B19326_001E", # Median income in past 12 months (inflation-adjusted)
                   "B07013_001E", # Total households
                   "B08013_001E", # Travel Time to Work
                   "B01002_001E") # Median Age

vb_census <- 
  get_acs(geography = "tract", 
          variables = selected_vars, 
          year=2018, 
          state="VA",
          county = c("Virginia Beach"),
          geometry=T, 
          output="wide") %>%
  st_transform('EPSG:6595') %>%
  rename(TotalPop = B01003_001E,
         Med_Age = B01002_001E,
         Race_TotalPop = B02001_001E, 
         Whites = B02001_002E,
         Blacks = B02001_003E,
         FemaleBachelors = B15001_050E, 
         MaleBachelors = B15001_009E,
         MedHHInc = B19013_001E,
         TotalPoverty = B06012_002E,
         TotalCommute = B08301_001E,
         CarCommute = B08301_002E,
         PubCommute = B08301_010E,
         TotalHispanic = B03002_012E,
         MedInc = B19326_001E,
         TotalHH = B07013_001E)
vb_census <- vb_census %>%
  dplyr::select(-NAME, -starts_with("B0"), -starts_with("B1"), -starts_with("B2")) %>%
  mutate(Area = st_area(vb_census),
         pctWhite = (ifelse(Race_TotalPop > 0, Whites / Race_TotalPop,0))*100,
         pctBlack = (ifelse(Race_TotalPop > 0, Blacks / Race_TotalPop,0))*100,
         pctHis = (ifelse(Race_TotalPop >0, TotalHispanic/Race_TotalPop,0))*100,
         pctBachelors = (ifelse(Race_TotalPop > 0, ((FemaleBachelors + MaleBachelors) / Race_TotalPop),0)) *100,
         pctPoverty = (ifelse(Race_TotalPop > 0, TotalPoverty / Race_TotalPop, 0))*100,
         pctCarCommute = (ifelse(TotalCommute > 0, CarCommute / TotalCommute,0))*100,
         pctPubCommute = (ifelse(TotalCommute > 0, PubCommute / TotalCommute,0))*100,
         year = "2018") %>%
  mutate(MedHHInc = replace_na(MedHHInc, 0),
         pctBachelors= replace_na(pctBachelors,0),
         pctHis= replace_na(pctHis,0),
         pctCarCommute= replace_na(pctCarCommute,0),
         PopDens = (TotalPop/(Area/27878400))) %>%
  dplyr::select(-Whites, -Blacks, -FemaleBachelors, -MaleBachelors, -TotalPoverty, -CarCommute, -PubCommute, -TotalCommute, -TotalHispanic)

### 2.2 Load weather data
vb_weather <- 
  riem_measures(station = "NTU", date_start = "2017-01-01", date_end = "2017-08-15") %>%
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
  mutate(Temperature = ifelse(Temperature == 0, 42, Temperature))

##### plot weather data
grid.arrange(
  ggplot(vb_weather, aes(interval60,Precipitation)) + geom_line(aes(),) + 
    labs(title="Percipitation", x="Hour", y="Perecipitation") + theme(legend.position = "none"),
  ggplot(vb_weather, aes(interval60,Temperature)) + geom_line(aes(),) + 
    labs(title="Temperature", x="Hour", y="Temperature") + theme(legend.position = "none"),
  ggplot(vb_weather, aes(interval60,Humidity)) + geom_line(aes(),) + 
    labs(title="Humidity", x="Hour", y="Humidity")  + theme(legend.position = "none"),
  top="Weather Data - Virginia Beach - January to August, 2017")

### 2.3 Load EMS stations, hospitals, and fire stations
ems_stations <- st_read('201127_ems_geocoded.shp') %>%
  st_transform(st_crs(vb_fishnet)) %>%
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
#mapview(fire_stations)

intersections <- st_read("https://opendata.arcgis.com/datasets/e61e0e16ed01403bb58fcaa86b859363_3.geojson") %>%
  dplyr::filter(grepl("PKWY|BLVD| AVE", STREETS)) %>%
  st_transform(st_crs(vb_fishnet)) %>%
  mutate(Legend = "intersections")
mapview(intersections)

### 2.4 load health data
health_dat <- read.csv('health_data_500_cities_vabch.csv') %>% 
  dplyr::select(TractFIPS, ends_with('CrudePrev')) %>% 
  rename(GEOID = TractFIPS)

vb_health <- merge(health_dat, vb_census,
                   by.x = "GEOID", by.y = "GEOID",
                   all.x = FALSE, all.y = TRUE,
                   sort = FALSE) %>% 
  dplyr::select(GEOID, ends_with('CrudePrev'), geometry)


##### 3. exploratory analysis ####
### EMS calls distributions
ggplot(main_ems.sf %>%
         group_by(interval60) %>%
         tally()) +
  geom_line(aes(x = interval60, y = n, color="colors_3")) +
  labs(title="EMS calls in Virginia Beach, January thru August 2017",
       x="Date", 
       y="Number of calls") +
  plotTheme() +
  theme(legend.position = "none")

### ems calls by time of day (need work)
main_ems.sf %>%
  group_by(EMSCallNumber, time_of_day) %>%
  tally()%>%
  summarize(total_calls = sum(n))%>%
  ggplot()+
  geom_histogram(aes(total_calls, fill= "color_3"), binwidth = 1)+
  labs(title="",
       x="Number of trips", 
       y="Frequency")+
  facet_wrap(~time_of_day)+
  plotTheme() +
  theme(legend.position = "none")

ggplot(main_ems.sf %>% mutate(hour = hour(CallDateandTime))) +
  geom_freqpoly(aes(hour, color = dotw), binwidth = 1)+
  labs(title="EMS calls by day of the week, August 2017 to Feb 2018",
       x="Hour", 
       y="Trip Counts")+
  plotTheme()

### map pop density fishnet
PopDens_net <- 
  dplyr::select(vb_census) %>% #what does it mean to dplyr::select(traffic_crashes)? I thought this was for selecting columns?
  mutate(PopDens = as.numeric(vb_census$PopDens)) %>% #why do we say countCrashes = 1? shouldnt it be equal to an aggregate sum?
  aggregate(., vb_fishnet, mean) %>%
  mutate(PopDens = replace_na(PopDens, 0),
         uniqueID = rownames(.),
         cvID = sample(round(nrow(vb_fishnet) / 24), size=nrow(vb_fishnet), replace = TRUE))

ggplot() + 
  geom_sf(data = vb_boundary, fill = NA) +
  geom_sf(data = PopDens_net, color = NA, aes(fill = PopDens)) +
  labs(title= "Population Density across Virginia Beach") +
  mapTheme

ggplot() +
  geom_sf(data = vb_tracts, fill = NA) +
  geom_sf(data = vb_census, aes(fill = Med_Age)) +
  mapTheme

### map med age fishnet
MedAge_net <- 
  dplyr::select(vb_census) %>% #what does it mean to dplyr::select(traffic_crashes)? I thought this was for selecting columns?
  mutate(Med_Age = as.numeric(vb_census$Med_Age)) %>% #why do we say countCrashes = 1? shouldnt it be equal to an aggregate sum?
  aggregate(., vb_fishnet, mean) %>%
  mutate(Med_Age = replace_na(Med_Age, 0),
         uniqueID = rownames(.),
         cvID = sample(round(nrow(vb_fishnet) / 24), size=nrow(vb_fishnet), replace = TRUE))

ggplot() + 
  geom_sf(data = vb_boundary, fill = NA) +
  geom_sf(data = MedAge_net, color = NA, aes(fill = Med_Age)) +
  labs(title= "Median Age across Virginia Beach") +
  mapTheme

### map health disease fishnet <- <- 
heart_disease_net <-
  vb_health %>%
  st_sf() %>%
  mutate(CHD_CrudePrev = ifelse(is.na(vb_health$CHD_CrudePrev),0,vb_health$CHD_CrudePrev)) %>%
  dplyr::select(CHD_CrudePrev) %>% 
  aggregate(., vb_fishnet, mean)

ggplot() + 
  geom_sf(data = vb_boundary, fill = NA) +
  geom_sf(data = heart_disease_net, color = NA, aes(fill = CHD_CrudePrev)) +
  labs(title= "Prevalance of Heart Disease across Virginia Beach") +
  mapTheme

tracts_mapping <- 
  vb_health %>%
  st_sf() %>%
  mutate(CHD_CrudePrev = ifelse(is.na(vb_health$CHD_CrudePrev),0,vb_health$CHD_CrudePrev))

# interpolate from tracts to fishnet. Look up the `extensive` parameter.
tracts_fishnet <-
  dplyr::select(tracts_mapping, CHD_CrudePrev) %>%
  st_interpolate_aw(., vb_fishnet, extensive = TRUE)

#map
grid.arrange(
  ggplot() + geom_sf(data=tracts_mapping, aes(fill = CHD_CrudePrev)) + ggtitle("Tracts"),
  ggplot() + geom_sf(data=tracts_fishnet, aes(fill = CHD_CrudePrev), colour=NA) + ggtitle("Grid cells"), 
  ncol=2)


##### 4. test for spatial process and correlations #####
### local moran's I
### colinearity tests

##### 5. model building #####
### what type modeling
### what variables
### confusion matrices 

##### 6. Cross Validation #####
### to think about whether to cross validate over neighborhoods or census tracts
### temporal and spatial CV 


  