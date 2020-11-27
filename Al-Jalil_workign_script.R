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
library(tidygeocoder)
library(sf)
library(leaflet)
library(leaflet)

ems_locations <- st_read('201127_ems_geocoded.shp')
mapview(ems_locations)
