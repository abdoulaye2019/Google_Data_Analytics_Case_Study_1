bike_df_clean %>% 
  group_by(rows = member_casual) %>% 
  summarise(Values = mean(ride_length)) %>% 
  ggplot(aes(x = rows, y = Values, fill = rows)) +
  geom_col() +
  labs(title = "Number of trips",
       y = "Number of rides",
       x = "Members types",
       subtitle = "Nombre de déplacements effectués par les occasionnels et les membres")+
  theme_minimal()

library(mapview)
library(leafsync)
casual_electric_SDGA <- bike_df_clean %>%
  filter(member_casual == "casual" & month == "mars" & rideable_type == "electric_bike" & 
           start_station_name == "DuSable Lake Shore Dr & Monroe St") %>% 
   select(member_casual, Longitude = start_lng, Latitude = start_lat, rideable_type, ride_length) #%>% 
  #write.csv("casaul.csv")
  # #mutate(longitude = start_lng, latitude = start_lat) %>%
  # mapview(xcol = "Longitude", ycol = "Latitude", crs = 4326, grid = FALSE)


member_electric_SDGA <- bike_df_clean %>%
  filter(member_casual == "member" & month == "mars" & rideable_type == "electric_bike" & 
           start_station_name == "DuSable Lake Shore Dr & Monroe St") %>% 
  select(member_casual, Longitude = start_lng, Latitude = start_lat, rideable_type, ride_length) #%>% 
 # write.csv("members.csv")
# #mutate(longitude = start_lng, latitude = start_lat) %>%
# mapview(xcol = "Longitude", ycol = "Latitude", crs = 4326, grid = FALSE)

  
casual <- mapview(casual_electric_SDGA, xcol = "Longitude",
        ycol = "Latitude",
        crs = 4326, grid = FALSE,cex = "ride_length",
        zcol = "ride_length",
        #col.regions = "tomato",
        layer.name = "Members",
        zoom = 19,
        use.layer.names = mapviewOptions(platform = "leaflet","CartoDB.DarkMatter"))


members <- mapview(member_electric_SDGA, xcol = "Longitude",
        ycol = "Latitude",
        crs = 4326, grid = FALSE,cex = "ride_length",
        zcol = "ride_length",
        #col.regions = "tomato",
        layer.name = "Members",
        zoom = 19,
        use.layer.names = mapviewOptions(platform = "leaflet","CartoDB.DarkMatter"))

sync(casual, members)


bike_df_clean %>% 
  drop_na() %>% 
  group_by(rows = start_station_name) %>% 
  summarise(start_station_name= n()) %>% 
  arrange(desc(start_station_name))
        
        
# Write data in csv file mean_ride_length

bike_df_clean %>% 
  group_by(rows = member_casual) %>% 
  summarise(Values = mean(ride_length)) %>% 
  write.csv("mean_ride_length_by_casual_members.csv")


# Write data in csv file mean_ride_length

bike_df_clean %>% 
  group_by(columns = day_of_week, Rows = member_casual) %>% 
  summarise(Values = mean(ride_length), .groups='drop') %>% 
  write.csv("mean_ride_length_by_casual_members_by_day_of_week.csv")


# Write data in csv file number of trips per day
bike_df_clean %>% 
  group_by(columns = day_of_week) %>% 
  summarise(Values = length(ride_id)) %>% 
  write.csv("number_trip_by_casual_members.csv")

# Write data in csv file number of usage of bike types by users
bike_df_clean %>% 
  group_by(Days = day_of_week, members = member_casual) %>% 
  summarise(Values = length(rideable_type),.groups='drop') %>% 
  write.csv("usage_of_bike.csv")

# Monthly mean of ride length by users
bike_df_clean %>% 
  group_by(month = month, members = member_casual) %>% 
  summarise(Values = mean(ride_length),.groups='drop') %>% 
  write.csv("monthly_mean_users.csv")


bike_df_clean %>% 
  group_by(Days = day_of_week, members = member_casual, type = rideable_type) %>% 
  summarise(Values = length(rideable_type),.groups='drop') %>% 
  ggplot(mapping = aes(x = type , y = Values,
                       fill = members)) +
  scale_fill_viridis_d() +
  geom_col(width=0.5, position = position_dodge(width=0.5)) +
  #theme(legend.position="none") +
  labs(title = "Rideable Type",
       subtitle = "number and type of bicycles used per user per day",
       x = "Days",
       y = "Number of used bicylcle",
       caption = "Cyclistic trip data") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))
