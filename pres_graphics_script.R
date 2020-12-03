plotTheme <- theme(text = element_text( family = "Avenir", color = "black"),
                   plot.title =element_text(size=12),
                   plot.subtitle = element_text(size=8),
                   plot.caption = element_text(size = 6),
                   axis.text.x = element_text(size = 8, angle = 45, hjust = 1),
                   axis.title.x = element_text(size = 10),
                   axis.text.y = element_text(size = 8),
                   axis.title.y = element_text(size = 10),
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
  mutate(ResponseTime =  difftime(mdy_hm(OnSceneDateandTime), mdy_hm(CallDateandTime), units = "min")) %>%
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
                          filter(`dotw` == "Sun" & `time_of_day` == "Overnight") %>%
                          mutate(ResponseTime = as.numeric(ResponseTime)) %>% 
                          dplyr::select(ResponseTime) %>%
                          aggregate(., vb_fishnet, mean) %>%
                          mutate(ResponseTime = replace_na(ResponseTime, 0)) %>%
                          dplyr::filter(ResponseTime > 0), color = NA, aes(fill = ResponseTime)) +
  scale_fill_viridis_c(option="plasma") +
  geom_sf(data = ems_stations, color="white", size =1.5, shape = 23, fill = "white") +
  mapTheme
  
ggplot() + 
  geom_sf(data = vb_boundary, fill = "black") +
  geom_sf(data = net <- main_ems.sf %>% 
            filter(`dotw` == "Sun" & `time_of_day` == "Morning") %>%
            mutate(ResponseTime = as.numeric(ResponseTime)) %>% 
            dplyr::select(ResponseTime) %>%
            aggregate(., vb_fishnet, mean) %>%
            mutate(ResponseTime = replace_na(ResponseTime, 0)) %>%
            dplyr::filter(ResponseTime > 0), color = NA, aes(fill = ResponseTime)) +
  scale_fill_viridis_c(option="plasma") +
  geom_sf(data = ems_stations, color="white", size =1.5, shape = 23, fill = "white")+
  mapTheme

ggplot() + 
  geom_sf(data = vb_boundary, fill = "black") +
  geom_sf(data = net <- main_ems.sf %>% 
            filter(`dotw` == "Sun" & `time_of_day` == "Afternoon") %>%
            mutate(ResponseTime = as.numeric(ResponseTime)) %>% 
            dplyr::select(ResponseTime) %>%
            aggregate(., vb_fishnet, mean) %>%
            mutate(ResponseTime = replace_na(ResponseTime, 0)) %>%
            dplyr::filter(ResponseTime > 0), color = NA, aes(fill = ResponseTime)) +
  scale_fill_viridis_c(option="plasma") +
  geom_sf(data = ems_stations, color="white", size =1.5, shape = 23, fill = "white") +
  mapTheme

ggplot() + 
  geom_sf(data = vb_boundary, fill = "black") +
  geom_sf(data = net <- main_ems.sf %>% 
            filter(`dotw` == "Sun" & `time_of_day` == "Evening") %>%
            mutate(ResponseTime = as.numeric(ResponseTime)) %>% 
            dplyr::select(ResponseTime) %>%
            aggregate(., vb_fishnet, mean) %>%
            mutate(ResponseTime = replace_na(ResponseTime, 0)) %>%
            dplyr::filter(ResponseTime > 0), color = NA, aes(fill = ResponseTime)) +
  scale_fill_viridis_c(option="plasma") +
  geom_sf(data = ems_stations, color="white", size =1.5, shape = 23, fill = "white") +
  mapTheme






# weekend vs weekday response time

ggplot(data = main_ems.sf, aes(x=ResponseTime, y=dotw, fill = time_of_day))+
  geom_bar(stat="identity", position=position_dodge())+
  labs(title="Response Time of EMS calls by days of the week and time of the week",
       x="ResponseTime", 
       y="Day of the week")+
  plotTheme




