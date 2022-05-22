
# Library -----------------------------------------------------------------

library(tidytuesdayR)
library(dplyr)
library(ggplot2)
library(maps)
library(stringr)


# Data Form Tidy Tuesday --------------------------------------------------

tt_available() #to see available data in tidy tuesday 
tt <- tt_load("2022-01-18") #Download dataset based on date 
Chocolate <- tt$chocolate



# Data Wrangling  ---------------------------------------------------------

df <- Chocolate %>% 
  mutate(Percent_of_Coc = as.numeric(sub("%", "",cocoa_percent))) %>% 
  group_by(country_of_bean_origin) %>% 
  summarise(average_rating = mean(rating),
            cocoa_per = mean(Percent_of_Coc)) %>% 
  arrange(desc(average_rating)) 

df <- df %>% rename("Country" = "country_of_bean_origin")


Map_data_world_c <- Map_data_world %>% left_join(df, by = c("region" = "Country"))

Map_data_world <- map_data("world") %>% as_tibble()

Map_data_world_c %>% ggplot(aes(long, lat, group = subregion))+
  geom_map(
    aes( map_id = region),
    map = Map_data_world,
    color = "white", 
    fill = "brown",
  )+
 coord_quickmap(ylim = c(-60,90))+
  geom_polygon(aes(group = group, fill =average_rating ), color = "black",na.rm = T)









