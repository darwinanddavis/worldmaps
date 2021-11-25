##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                            #30daymapchallenge2021                        ----
##                              day 22 - boundaries                         ~~~~
##                            author: Matt Malishev                         
##                               @darwinanddavis                            
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
citation <- "Natural Earth"

# pcks -----------------------------------------------------
pacman::p_load(here,dplyr,readr,ggplot2,sf,sp,ggmap,rnaturalearth,rnaturalearthdata,maptools,scales,rgdal,colorspace,mapdata,ggsn,mapview,mapproj,ggthemes,grid,ggtext,rworldxtra,stringr,ggtext,showtext)
fh <- "day22"

# data ----------------------------------------------------------
bg <- "#f2f0ee"
fg <- "#10163c"
border_col <- "#4d515c"
road_col <- "#745D42" 
park_col <- "#2D4A67"
graticule_col <-"#D1C9C0"
path_col <- "#f4d29f"
show_col(c(bg,fg,border_col,road_col,park_col,graticule_col,path_col))

# read spatial data ------------------------------------------------------------
library(rnaturalearth)
library(rnaturalearthdata)
countries <- c("Australia")
dd <- rnaturalearth::ne_states(countries, returnclass = "sf") 
parks_aus <- readRDS(here::here("shapefiles","parks_aus","parks_aus.Rda")) %>% st_transform(crs = 4326)
rivers_ne <- readRDS(here::here("shapefiles","rivers","rivers_ne.Rda"))
lakes_ne <- readRDS(here::here("shapefiles","lakes","lakes_ne.Rda"))
urban_areas <- readRDS(here::here("shapefiles","urban","urban_areas.Rda")) # smooth polygons 
roads_global <- readRDS(here::here("shapefiles","roads","roads_global.Rda"))

# get projection ------------------------------------------------
prj <- 3032
# prj <- proj=stere +lat_0=-90 +lat_ts=-71 +lon_0=70
add_graticule <- T
add_graticule <- ifelse(get_projection == T, add_graticule <- T, add_graticule <- F)
dd <- dd %>% st_transform(prj) # set proj for polygon data
parks_aus <- parks_aus %>% st_transform(prj) 
urban_areas <- urban_areas %>% st_transform(prj) 
rivers_ne <- rivers_ne %>% st_transform(prj) 
lakes_ne <- lakes_ne %>% st_transform(prj) 

# plot --------------------------------------------------------------------

park_alpha <- 0.4
linetype <- 3
add_graticule <- T

bbox <- city_df %>% st_buffer(8 * 10^7) %>% st_bbox
  
  p <- ggplot() +
    geom_sf(data = urban_areas, fill = adjustcolor(road_col,park_alpha), col = NA, size = 0) + # parks
    geom_sf(data = roads_global, col = road_col, size = 0.02) + # add ne roads
    geom_sf(data = lakes_ne, fill = bg, col = border_col, size = 0) + # add global lakes
    geom_sf(data = rivers_ne, fill = NA, col = bg, size = 0.1) + # add rivers
    coord_sf(xlim = c(bbox[1],bbox[3]), ylim = c(bbox[2],bbox[4])) + # proj
    theme_nothing() +
    theme(panel.grid.major = element_line(colour = ifelse(add_graticule == T, graticule_col, bg), linetype = linetype),
          plot.background = element_rect(fill = bg),
          axis.text = element_blank(), 
          axis.ticks.length=unit(0, "null"),
          panel.ontop = F
    ) +
    labs(x = NULL, y = NULL) 
  p

# save --------------------------------------------------------------------
ggsave(here::here("img","30daymap2021/") %>% paste0(fh,".png"),p, device = "png", dpi = "retina", width = 29.7, height = 42, units = "cm",bg = "transparent")  
  