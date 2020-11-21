library(tidyverse)
library(dplyr)
library(sf)
library(mapview)

##### 1.load and format the VB dataset ####
###load main dataset
main_ems <- read.csv("Main_VaBeach_EMS_2017_18.csv")
main_ems.sf <- main_ems %>%
  na.omit() %>%
  st_as_sf(coords = c("X", "Y"), crs = 4326, agr = "constant") %>%
  st_transform('EPSG:6595')
#mapview::mapview(main_ems.sf)



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