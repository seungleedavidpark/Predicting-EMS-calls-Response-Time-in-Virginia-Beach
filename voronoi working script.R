library(dplyr)

california <- map_data("state") %>% filter(region == "california")
ncdc.cali <- ncdc_locations %>% filter(state=="CA")
cali_map <-
  ggplot(data=ncdc.cali,aes(x=long,y=lat)) +
  scale_fill_gradientn("Elevation", 
                       colors=c("seagreen","darkgreen","green1","yellow","gold4", "sienna"),
                       values=scales::rescale(c(-60,0,1000,2000,3000,4000))) + 
  scale_color_gradientn("Elevation", 
                        colors=c("seagreen","darkgreen","green1","yellow","gold4", "sienna"),
                        values=scales::rescale(c(-60,0,1000,2000,3000,4000))) + 
  coord_quickmap() + 
  theme_minimal() +
  theme(axis.text=element_blank(),
        axis.title=element_blank())

cali_map +
  geom_point(aes(color=elev),size=.01) +
  geom_path(data=california,aes(long,lat,group=group),color="black")

cali_map +
  geom_voronoi(aes(fill=elev),outline=california)


# create voronoi
library(data.table)
setDT(ems_stations)[, paste0("latlong", 1:2) := tstrsplit(latlong, ",")]

ems_stations <- ems_stations %>%
  rename(
    lat = latlong1,
    lon = latlong2
  )

vb_map <- ggplot() + 
  geom_sf(data = vb_boundary, fill = "black")

vb_map +
  geom_voronoi(data = ems_stations, aes(x=lat, y=lon), na.rm=TRUE, outline = vb_boundary)
