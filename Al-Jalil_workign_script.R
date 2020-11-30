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
         TotalHH = B07013_001E) %>%
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

hospital_iso <- mb_isochrone(hospitals, #this can be an sf object or a number as coordinates
                             profile = "driving", #isocromes are assuming free of traffic
                             time = c(5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60),
                             access_token = my_token) #this can either be a single number, or a series of numbers in minutes

mapview(hospital_iso)

mapbox_map <- leaflet() %>%
  addMapboxTiles(style_id = "streets-v11", #AddMapboxTiles converts vector tiles into raster tiles
                 username = "mapbox",
                 access_token = my_token) #username is not your username, but "mapbox"

mapbox_map
mapbox_map %>%
  addPolygons(data = hospital_iso,
              color = rev(colors_12),
              fillColor = rev(colors_12),
              fillOpacity = 0.01, 
              opacity = .01, 
              weight = 0.2) %>%
  addLegend(labels = c(5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60),
            colors = colors_12,
            title = "Drive-time<br/>around Hospitals") 


isos <- mb_isochrone(
  location = "1 NRG Pkwy, Houston, TX 77054",
  profile = "driving",
  time = 1:45
)

pal <- colorNumeric("viridis", isos$time, na.color = "transparent") #generates a series of colors

#this shows accessibility to the polling droop-off location
mapbox_map %>%
  addPolygons(data = isos,
              fillColor = ~pal(time),
              stroke = FALSE,
              fillOpacity = 0.1) %>%
  addLegend(values = isos$time,
            pal = pal,
            title = "Drive-time to NRG Arena")

##making an "accessibility surface"

#fasterize takes a dataset,and a fucntion, and will generate a raster dataset from a vector dataset
library(fasterize)
library(sf)

isos_proj <- st_transform(isos, 32615) #changes the projection system

template <- raster(isos_proj, resolution = 100) #creates a 100mx100m black cell template

iso_surface <- fasterize(isos_proj, template, field = "time", fun = "min")

mapbox_map %>%
  addRasterImage(iso_surface, colors = pal, opacity = 0.5) %>%
  addLegend(values = isos$time, pal = pal,
            title = "Drive-time to NRG Arena")

## Identifying populations that may have a difficult time voting

# sf: sf, which stands for simple features, has cemented itself in the last couple years as the 
#   core package for vector-based spatial data representation and analysis in R. Spatial data are 
#   represented with sf much like regular R data frames, but with a list-column representing the 
#   geometry of each row.
# tidyverse: A collection of popular R packages maintained by RStudio that work together to facilitate 
#   data representation, wrangling, and visualization.
# tidycensus: An R package for downloading and working with data from the US Census Bureau's decennial 
#   Census, American Community Survey (aggregate and microdata), and Population Estimates program. I first 
#   wrote this package three years ago because I grew tired of the tedious process of downloading 
#   Census data, cleaning it, and joining to shapefiles to do spatial analysis. tidycensus does all 
#   this for you internally with the ability to return Census and ACS data as simple features objects 
#   ready for mapping and analysis.

#download early voding sites, in C:\Users\gault\OneDrive\UPenn\Third Semester\Public Policy Analytics\Masterclass\data

library(tidyverse)
library(tidycensus)

ev_sites <- read_rds("tarrant_EV_sites.rds")

walking_isos <- mb_isochrone(
  ev_sites,
  profile = "walking",
  time = 20,
  id = "name" #name is a column that identifies the name of the polling place
)

mapbox_map %>%
  addPolygons(data = walking_isos,
              popup = ~id)


