##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                            #30daymapchallenge2021                        ----
##                              day 12 - population                                 ~~~~
##                            author: Matt Malishev                         
##                               @darwinanddavis                            
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
citation <- ""

# pcks -----------------------------------------------------
pacman::p_load(here,dplyr,readr,ggplot2,sf,sp,ggmap,rnaturalearth,rnaturalearthdata,maptools,scales,rgdal,colorspace,mapdata,ggsn,mapview,mapproj,ggthemes,grid,ggtext,rworldxtra,stringr,ggtext,showtext)
fh <- "day12"
df_sf <- df %>% st_as_sf(coords = c("lon","lat")) 
ggplot() +
  geom_sf(data = df_sf, aes(fill = bird_mass %>% as.factor()), show.legend = F)


# spatial data -----------------------------------------------------------
dd <- rnaturalearth::ne_states(c("Australia","New Zealand"), returnclass = "sf") 
bbox <- dd %>% filter(name == "New South Wales") %>% 
  st_centroid() %>% 
  st_buffer(10) %>% 
  st_bbox()

bbox <- dd %>% st_bbox()

ggplot() +
  geom_sf(data = dd, col = NA) +
  geom_hex(data = df,aes(lon,lat,col = bird_size), bins=500, show.legend = T) +
  coord_sf(xlim = c(bbox[1],bbox[3]), ylim = c(bbox[2],bbox[4]))


## ggmap
# ggmap::get_map()  
# The different types of background graphics you can use as a base to your map  
maptypes <- c("terrain", "terrain-background", "satellite", "roadmap", "hybrid", "toner", "watercolor", "terrain-labels", "terrain-lines", "toner-2010", "toner-2011", "toner-background", "toner-hybrid", "toner-labels", "toner-lines", "toner-lite");maptypes

# ggmap::get_googlemap()
maptypes_google = c("terrain", "satellite", "roadmap","hybrid");maptypes_google

# return as raster then convert to sf for proj
# all return class = "raster"??

# for sf (with x = st_as_sf(meuse.sr))
ddhex <- st_make_grid(st_as_sf(dd), 
                      square=FALSE,
                      cellsize = 1,
                      n = 200,
                      # crs = prj,
                      flat_topped = T)[st_as_sf(dd)]


ggplot() + 
  geom_sf(data = ddhex, size = 0.2) +
  geom_hex(data = df, aes(Longitude, Latitude), bins = 500) +
  coord_sf(xlim = c(bbox[1],bbox[3]), ylim = c(bbox[2],bbox[4]))

