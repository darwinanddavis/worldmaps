##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                            #30daymapchallenge2022                        ----
##                             day 25 - two colours                                 ~~~~
##                            author: Matt Malishev                         
##                               @darwinanddavis                            
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

fh <- "two_colours"

# pcks --------------------------------------------------------------------
pacman::p_load(sf,here,dplyr,osmdata,ggplot2,colorspace,osrm,osmextract,magrittr,sfheaders,maps,stringr,mapdata)

# get data ----------------------------------------------------------------

# find centroids 
data(world.cities) # /maps
prj <- 4326
buff <- 30
site <- c("Melbourne","Amsterdam","Leipzig","Atlanta","Medellin")
countries <- c("Australia","Netherlands","Germany","USA","Colombia")
sfcent <- world.cities %>% # get sites 
  filter(country.etc %in% countries, name %in% site) %>% 
  dplyr::select(name,long,lat) %>% dplyr::rename(.cols = 2, lon = long) %>% 
  slice(-6) %>% 
  st_as_sf(coords = c("lon", "lat"), crs = 4326)
sfpoly <- sfcent %>% st_buffer(buff) %>% st_cast("POLYGON")

# osrm  -------------------------------------------------------------------
dir.create(here::here(fh)); dir.create(here::here(fh,"data",site)); dir.create(here::here(fh,"plot",site))
require(osmdata)
saveosm <- function(id,f) saveRDS(here::here(fh,"data",site,paste(id,f)))

# roads
o1 <- c("motorway","primary","trunk")
o2 <- "secondary"
o3 <- c("tertiary","footpath","sidewalk","pedestrian","living_street")

# get osm data for cities
for(id in sfpoly %>% nrow){
  bb <- sfpoly[id,] %>% st_bbox()
  get_osm <- function(bb,key,value,type) opq(bbox = bb) %>%
    add_osm_feature(key = key, value = value) %>%
    osmdata_sf() %>% .[[type]]
  
  osm_primary <- get_osm(bb,"highway",o1,"osm_lines")
  osm_secondary <- get_osm(bb,"highway",o2,"osm_lines")
  osm_tertiary <- get_osm(bb,"highway",o3,"osm_lines")
  
  # save to dir
  osm_primary %>% saveosm(id,"osm_primary.Rda")
  osm_secondary %>% saveosm(id,"osm_secondary.Rda")
  osm_tertiary %>% saveosm(id,"osm_tertiary.Rda")
}

# plot --------------------------------------------------------------------
name <- "Medellin"
id <- sfpoly[sfpoly$name == name,]
width = 20
height = 40
opac <- 0.8
cex <- 8
size <- 1.2  
bg <- "#FFFFFF"; col_na <- "#000000"
colpal <- c("#C36518","#573C78","#6F8E2F","#275791","#000000")
my_theme <-  theme(panel.grid.major = element_line(colour = NA),
                   panel.grid.minor = element_line(colour = NA),
                   panel.background = element_rect(fill = bg, colour = NA),
                   plot.background = element_rect(fill = bg, colour = NA),
                   axis.text = element_text(colour = bg, size = cex),
                   text = element_text(color = fg, size = cex),
                   legend.background = element_rect(fill = bg, colour = "transparent"),
                   legend.box.background = element_rect(fill = bg, colour = "transparent"))

# read in data and add colpal 
df <- here::here("data","site",name) %>% list.files(full.names = T) %>% readRDS() %>% 
  rbind(
    osm_primary,
    osm_secondary,
    osm_tertiary
  ) %>% mutate(colpal = colpal[id])

# optional title
ttldf <- tibble(label = site %>% str_to_upper() %>% str_sub(0,3),
                x = bb %>% unlist %>% .[1] + 0.01,
                y = bb %>% unlist %>% .[4] - 0.02,
                angle = 90, color = "#000000", label.color = NA, fill = NA
)

# plot 
ggplot() + 
  geom_sf(data = df %>% filter(highway %in% o1), aes(fill = colpal, col = colpal), size = size) +
  geom_sf(data = df %>% filter(highway %in% o2), aes(fill = colpal, col = colpal), size = size/2) +
  geom_sf(data = df %>% filter(highway %in% o3), aes(fill = colpal, col = colpal), size = size/3) +
  geom_sf(data = sfcent, col = col_na, fill = adjustcolor(col_na,opac), size = 2) +
  # ggspatial::annotation_scale(location = 'bl',bar_cols =c(bg,fg), line_width = 0.5, pad_x = unit(1.5, "cm"), pad_y = unit(0.1, "cm"), text_col = fg, line_col = fg, style = "ticks", width_hint = 0.1) + # add scale bar
  ggthemes::theme_map() +
  theme(legend.background = element_rect(fill="transparent", color = "transparent")) +
  coord_sf(xlim = bb[c(1,3)], bb[c(2,4)],crs = prj) +
  my_theme 

ggsave(here::here(fh,"plot",paste(df[1],".png", sep ="_")), device = "png", width = width, height = height, units = "cm", dpi = "retina", bg = "transparent")
