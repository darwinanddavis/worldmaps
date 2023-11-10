##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                            #30daymapchallenge2023                        ----
##                                day 9 - hexagons                          ~~~~
##                            author: Matt Malishev                         
##                               @darwinanddavis                            
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# pcks --------------------------------------------------------------------
pacman::p_load(here,dplyr,readr,ggplot2,sf,sp,ggmap,rnaturalearth,rnaturalearthdata,maptools,scales,rgdal,colorspace,mapdata,ggsn,mapview,mapproj,ggthemes,grid,rworldxtra,stringr,ggtext,showtext,lubridate,foreign,rmapshaper)
sf_use_s2(FALSE)

# world data ----------------------------------------------------
dd <- ne_states("Australia", returnclass = "sf") #

# get state level shp files from global dataset
fh <- here::here("data","Historical Bushfire Boundaries.gdb","a00000009.gdbtable")
fhl <- fh %>% st_layers() %>% .[[1]] # get layer names
a7 <- fh %>% st_read(layer = fhl) 
ii <- a7 %>% pull(state) %>% unique 

# sep into states
for(i in ii){
  df <- a7 %>% filter(state %in% i) %>% 
    rename("date" = "ignition_date") %>% 
    mutate(year = date %>% year,
           month = date %>% month,
           day = date %>% day,
           hour = date %>% hour,
           minute = date %>% minute,
           second = date %>% second
    ) %>% 
    mutate_at(c("year","month","day"), as.numeric)
  fhh <- i %>% str_split_fixed(" ", 2) %>% .[1]
  dir.create(here::here("data",fhh))
  df %>% st_write(here::here("data",fhh,paste0(fhh,".shp")))
}

# calc burnt areas -------------------------------------------------------------

# area by year 
fhh <- c("NSW","VIC","TAS","SA","WA","QLD") 
df_area_sum <- c()
for(f in fhh){
  df  <-  st_read(here::here("bushfire","data",f,paste0(f,".shp"))) %>% 
    group_by(year) %>% 
    summarise("area_sum" = area_ha %>% sum) %>% 
    mutate("state_id" = f)  
  df_area_sum <- rbind(df,df_area_sum)
}
dir.create(here::here("bushfire","data","_area",f))
df_area_sum %>% st_write(here::here("bushfire","data","_area",f,paste0(f,"_area.shp")))
df_area_sum %>% st_drop_geometry %>% write_csv(here::here("bushfire","data","_area",f,paste0(f,"_area.csv")))


# hexagon density of burnt areas ------------------------------------------

# get intersecting shp polygons by year to hex 
hex_polygons <- readRDS(here::here("data","30daymap2023","day9.Rda")) %>% 
  filter(!st_is_empty(.)) # rm empty geos

# or redo hex cellsize calcs 
d_ <- here::here("data",fhh,paste0(fhh,".shp")) %>% 
  lapply(st_read) %>% rlist::list.rbind()
prj <- 4326
cellsize <- 0.15
hex_points <- d_ %>% as_Spatial() %>% spsample(type = "hexagonal", cellsize = cellsize)
hex_polygons <- HexPoints2SpatialPolygons(hex_points) %>%
st_as_sf(crs = st_crs(prj)) # %>% st_intersection(., df_hex)

# add colpal
hex_polygons$fill <- lengths(st_intersects(hex_polygons, d_))


# get sat map  ------------------------------------------------------------

# devtools::install_github('oswaldosantos/ggsn')
require(ggmap)
require(ggsn)
api <- here::here("data","api.txt") %>% read_lines() # read personal google api 
register_google(key = api)
addy <- "Alice Springs, Northern Territory"
extent <- geocode(addy, output = "more") %>% pull(address)
maptype <- "satellite"
zoom <- 4
color <- "bw"
darken <- 0.3

# get google map
prj <- 3857 # set proj to match google map prj
base_map <-  get_map(extent, maptype = maptype,color = color, zoom = zoom)

# function to fix the bbox to be in EPSG:3857
ggmap_bbox <- function(map) {
  if (!inherits(map, "ggmap")) stop("map must be a ggmap object")
  # Extract the bounding box (in lat/lon) from the ggmap to a numeric vector,
  # and set the names to what sf::st_bbox expects:
  map_bbox <- setNames(unlist(attr(map, "bb")),
                       c("ymin", "xmin", "ymax", "xmax"))
  # Coonvert the bbox to an sf polygon, transform it to 3857,
  # and convert back to a bbox (convoluted, but it works)
  bbox_3857 <- st_bbox(st_transform(st_as_sfc(st_bbox(map_bbox, crs = 4326)), prj))
  # Overwrite the bbox of the ggmap object with the transformed coordinates
  attr(map, "bb")$ll.lat <- bbox_3857["ymin"]
  attr(map, "bb")$ll.lon <- bbox_3857["xmin"]
  attr(map, "bb")$ur.lat <- bbox_3857["ymax"]
  attr(map, "bb")$ur.lon <- bbox_3857["xmax"]
  map
}
map <- ggmap_bbox(base_map) # transform proj of sat map


# plot --------------------------------------------------------------------
fg <- "#EFEFEF" %>% darken(0.2)
opac <- 0.2
width <- 940

# set colpal
col_low <- "#335C73"
col_mid <- "#BA9BB6" 
col_high <- "#FFFFFF"
  
# match sequential colpal as sequential values in df
colv <- colorRampPalette(colors = c(col_low, col_mid, col_high))
colpal <- colv(hex_polygons$fill %>% n_distinct() %>% log)

map %>% ggmap(extent = "device", legend = "bottom", darken = c(opac,fg)) +
  geom_sf(data = dd %>% st_transform(prj), fill = fg, col = fg, size = 0.05, alpha = 0.01, inherit.aes = F) +
  geom_sf(data = dd2 %>% st_transform(prj), fill = fg, col = fg, size = 0.05, alpha = 0.01, inherit.aes = F) +
  geom_sf(data = hex_polygons, aes(fill = fill %>% log), col = fg %>% adjustcolor(opac), size = 0.01, inherit.aes = F) +
  scale_fill_gradientn(name = "", colours = colpal, aesthetics = c("fill"), na.value = "transparent") +
  theme_map() +
  theme( # regular map bg
    plot.background = element_rect(fill = "transparent", color = "transparent"),
    panel.background = element_rect(fill = "transparent", color = "transparent")
    ,panel.grid.major = element_line(colour = fg %>% darken(0.3), size = 0.1, linetype = 3)
    ,panel.ontop = F
    , legend.background = element_rect(fill = "transparent")
    ,legend.key.size = unit(0.4, "cm")
    , legend.text = element_blank()
    , legend.title = element_text(color = colpal[colpal %>% length])
    , legend.position=c(0.1,0.28)
  )
# save map
ggsave(here::here("bushfire","plot","satellite","aus.png"), device = "png", dpi = "retina", width = 30, height = 30, units = "cm", bg = "transparent")


# area sum per year by state  ---------------------------------------------

# line plot over time
yr_min <- 1935
d <- here::here("data","_area","area_sum_all.csv") %>% read_csv %>% 
  filter(year > yr_min)

# stacked barplot
limits <- d$state_id %>% n_distinct()
colpal <- sequential_hcl(limits,"Burg") %>% rev
opac <- 0.7
d %>% 
  ggplot() +
  geom_bar(aes(year,area_sum, fill = state_id %>% factor), col = NA, size = 0
           , position="fill"
           ,stat="identity"
  ) +
  # scale_colour_manual("State",values = colpal, aesthetics = "colour") +
  scale_fill_manual("State",values = adjustcolor(colpal,1), aesthetics = "fill") +
  # scale_x_continuous(limits = c(1935,2022)) +
  labs(y = "Area burnt", x = "Year") +
  coord_flip() + 
  theme_classic()
width <- 25
height <- 15
ggsave(here::here("bushfire","plot","area", paste0("area_sum_",yr_min,".png")), device = "png", dpi = "retina", width = width, height = height, units = "cm")

