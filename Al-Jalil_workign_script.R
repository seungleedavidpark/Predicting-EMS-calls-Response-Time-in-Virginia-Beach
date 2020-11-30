##### 1.load and format the VB dataset ####
### make additional columns (delays, etc.)
### make it sf object
### fishnet

##### 2.load load additional datasets #####
### health outcome, crash, census etc.
### nearest neighbor?

##### 3. exploratory analysis ####

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

library(dplyr)
library(sf)
library(leaflet)
library(shiny)
library(mapboxapi)
library(mapview)
library(tidycensus)
library(tidyverse)
library(viridis)
library(riem)
library(measurements)
library(mapboxapi)

my_token <- "pk.eyJ1IjoiZ2F1bHQzNCIsImEiOiJja2ZsbWd5cm8xNDBsMnlwajMzbW15c2Y0In0.nZ9siGKFAjMx_JQVEzeOtg"
mb_access_token(my_token, install = TRUE, overwrite = TRUE) #install = True installs your token so you dont have to keep loading your token

colors_3 <- viridisLite::viridis(3)

colors_12 <- viridisLite::viridis(12)

ems_locations <- st_read('201127_ems_geocoded.shp') %>%
  st_transform('EPSG:6595')
mapview(ems_locations)

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

#add health
health_dat <- read.csv('health_data_500_cities_vabch.csv') %>% 
  dplyr::select(TractFIPS, ends_with('CrudePrev')) %>% 
  rename(GEOID = TractFIPS)

vb_health <- merge(health_dat, vb_census,
                   by.x = "GEOID", by.y = "GEOID",
                   all.x = FALSE, all.y = TRUE,
                   sort = FALSE) %>% 
  dplyr::select(GEOID, ends_with('CrudePrev'), geometry)

hospitals <- st_read('Hospitals__Virginia_.shp') %>% 
  dplyr::filter(City == 'Virginia Beach')
mapview(hospitals)

fire_stations <- st_read('Fire_Stations.shp') %>% 
  dplyr::filter(CITY == 'Virginia Beach')
mapview(fire_stations)


#--- david_script

main_ems <- read.csv("Main_VaBeach_EMS_2017_18.csv")
main_ems.sf <- main_ems %>%
  na.omit() %>%
  st_as_sf(coords = c("X", "Y"), crs = 4326, agr = "constant") %>%
  st_transform('EPSG:6595')
mapview::mapview(main_ems.sf)

### make additional columns (delays, etc.)
### fishnet
vb_boundary <-
  st_read("https://gismaps.vbgov.com/arcgis/rest/services/Basemaps/Administrative_Boundaries/MapServer/0/query?where=1%3D1&outFields=*&outSR=4326&f=json") %>%
  st_transform('EPSG:6595')

mapview::mapview(vb_boundary)

vb_fishnet <- 
  st_make_grid(vb_boundary,
               cellsize = 1000, 
               square = TRUE) %>%
  .[vb_boundary] %>% 
  st_sf() %>%
  mutate(uniqueID = rownames(.))

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


PopDens_net <- 
  dplyr::select(tracts18) %>% #what does it mean to dplyr::select(traffic_crashes)? I thought this was for selecting columns?
  mutate(PopDens = as.numeric(tracts18$PopDens)) %>% #why do we say countCrashes = 1? shouldnt it be equal to an aggregate sum?
  aggregate(., fishnet, mean) %>%
  mutate(PopDens = replace_na(PopDens, 0),
         uniqueID = rownames(.),
         cvID = sample(round(nrow(fishnet) / 24), size=nrow(fishnet), replace = TRUE))

##### 2.load additional datasets #####
### census (pop_density, med_age, race, poverty, income, education)
selected_vars <- c("B02001_001E", # Estimate!!Total population by race -- ##let's double check that it's okay to use this as long as we justify it
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
                   "B08013_001E",
                   "B01002_001E")

vb_census <- 
  get_acs(geography = "tract", 
          variables = selected_vars, 
          year=2018, 
          state="VA",
          county = c("Virginia Beach"),
          geometry=T, 
          output="wide") %>%
  #st_transform('ESRI:102658') %>%
  rename(Med_Age = B01002_001E,
         TotalPop = B02001_001E, 
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
         TotalHH = B07013_001E) %>%
  dplyr::select(-NAME, -starts_with("B0"), -starts_with("B1"), -starts_with("B2")) %>%
  mutate(pctWhite = (ifelse(TotalPop > 0, Whites / TotalPop,0))*100,
         pctBlack = (ifelse(TotalPop > 0, Blacks / TotalPop,0))*100,
         pctHis = (ifelse(TotalPop >0, TotalHispanic/TotalPop,0))*100,
         pctBachelors = (ifelse(TotalPop > 0, ((FemaleBachelors + MaleBachelors) / TotalPop),0)) *100,
         pctPoverty = (ifelse(TotalPop > 0, TotalPoverty / TotalPop, 0))*100,
         pctCarCommute = (ifelse(TotalCommute > 0, CarCommute / TotalCommute,0))*100,
         pctPubCommute = (ifelse(TotalCommute > 0, PubCommute / TotalCommute,0))*100,
         year = "2018") %>%
  mutate(MedHHInc = replace_na(MedHHInc, 0),
         pctBachelors= replace_na(pctBachelors,0),
         pctHis= replace_na(pctHis,0),
         pctCarCommute= replace_na(pctCarCommute,0)) %>%
  dplyr::select(-Whites, -Blacks, -FemaleBachelors, -MaleBachelors, -TotalPoverty, -CarCommute, -PubCommute, -TotalCommute, -TotalHispanic)

### weather data
vb_weather <- 
  riem_measures(station = "NTU", date_start = "2017-01-01", date_end = "2017-08-15") %>%
  dplyr::select(valid, tmpf, p01i, sknt, relh)%>%
  replace(is.na(.), 0) %>%
  #mutate(interval60 = ymd_h(substr(valid,1,13))) %>%
  #mutate(week = week(interval60),
  #dotw = wday(interval60, label=TRUE)) %>%
  #group_by(interval60) %>%
  summarize(Temperature = max(tmpf),
            Precipitation = sum(p01i),
            Wind_Speed = max(sknt),
            Humidity = max(relh)) %>%
  mutate(Temperature = ifelse(Temperature == 0, 42, Temperature))