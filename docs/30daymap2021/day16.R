##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                            #30daymapchallenge2021                        ----
##                              day 16 - urban                                 ~~~~
##                            author: Matt Malishev                         
##                               @darwinanddavis                            
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
citation <- ""


# pcks -----------------------------------------------------
pacman::p_load(here,dplyr,readr,ggplot2,sf,sp,ggmap,rnaturalearth,rnaturalearthdata,maptools,scales,rgdal,colorspace,mapdata,ggsn,mapview,mapproj,ggthemes,grid,ggtext,rworldxtra,stringr,ggtext,showtext)
fh <- "day16"
df_sf <- df %>% st_as_sf(coords = c("lon","lat")) 
ggplot() +
  geom_sf(data = df_sf, aes(fill = bird_mass %>% as.factor()), show.legend = F)


# spatial data -----------------------------------------------------------
dd <- rnaturalearth::ne_states(c("Australia","New Zealand"), returnclass = "sf") 


## ggmap
# ggmap::get_map()  
# The different types of background graphics you can use as a base to your map  
maptypes <- c("terrain", "terrain-background", "satellite", "roadmap", "hybrid", "toner", "watercolor", "terrain-labels", "terrain-lines", "toner-2010", "toner-2011", "toner-background", "toner-hybrid", "toner-labels", "toner-lines", "toner-lite");maptypes

# ggmap::get_googlemap()
maptypes_google = c("terrain", "satellite", "roadmap","hybrid");maptypes_google

# return as raster then convert to sf for proj
# all return class = "raster"??

