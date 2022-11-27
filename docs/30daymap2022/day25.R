##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                            #30daymapchallenge2022                        ----
##                             day 25 - two colours                                 ~~~~
##                            author: Matt Malishev                         
##                               @darwinanddavis                            
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

fh <- "two_colours"

# pcks --------------------------------------------------------------------
pacman::p_load(sf,here,dplyr,osmdata,ggplot2,colorspace,osrm,osmextract,magrittr,sfheaders,maps,stringr)

# get data ----------------------------------------------------------------

# find centroids 
prj <- 4326
site <- c("melbourne","amsterdam","leipzig","atlanta","medellin")
countries <- c("australia","netherlands","germany","usa","colombia")
sfcent <- world.cities %>% # get sites 
  filter(country.etc %in% countries, name %in%  str_to_title(site)) %>% 
  dplyr::select(name,long,lat) %>% dplyr::rename(.cols = 2, lon = long) %>% 
  sfheaders::sf_point(keep = T, x = "lon", y = "lat") 
st_crs(sfcent) <- prj # set proj
sfpath <- data.frame(id='center', lon=c(sfcent %>% st_coordinates() %>% .[1],sfcent %>% st_coordinates() %>% .[1] +0.5), lat=c(sfcent %>% st_coordinates() %>% .[2],sfcent %>% st_coordinates() %>% .[2]+0.5)) %>% sfheaders::sfc_linestring(x='lon', y='lat') %>% st_as_sf(crs = prj)
sfpoly <- sfcent %>% st_buffer(1000) %>% st_cast("POLYGON")

# osrm  -------------------------------------------------------------------
dir.create(here::here(fh)); dir.create(here::here(fh,"data",site)); dir.create(here::here(fh,"plot",site))

library(osmdata)
bb <- sfpoly %>% st_bbox()
get_osm <- function(bb,key,value,type) opq(bbox = bb) %>%
  add_osm_feature(key = key, value = value) %>%
  osmdata_sf() %>% .[[type]]

# roads
o1 <- c("motorway","primary","trunk")
o2 <- "secondary"
o3 <- c("tertiary","footpath","sidewalk","pedestrian","living_street")

osm_primary <- get_osm(bb,"highway",o1,"osm_lines")
osm_secondary <- get_osm(bb,"highway",o2,"osm_lines")
osm_tertiary <- get_osm(bb,"highway",o3,"osm_lines")

# save to dir
osm_primary %>% saveRDS(here::here(fh,"data",site,paste("osm_primary.Rda")))
osm_secondary %>% saveRDS(here::here(fh,"data",site,paste("osm_secondary.Rda")))
osm_tertiary %>% saveRDS(here::here(fh,"data",site,paste("osm_tertiary.Rda")))

# plot --------------------------------------------------------------------
width = 20
height = 40
opac <- 0.8
cex <- 8
size <- 0.4  
bg <- "#262626"; fg <- "#FFFFFF"; col_na <- "#82093E"
colv <- c("#C36518","#573C78","#6F8E2F","#275791","#000000")
my_theme <-  theme(panel.grid.major = element_line(colour = NA),
                   panel.grid.minor = element_line(colour = NA),
                   panel.background = element_rect(fill = bg, colour = NA),
                   plot.background = element_rect(fill = bg, colour = NA),
                   axis.text = element_text(colour = bg, size = cex),
                   text = element_text(color = fg, size = cex),
                   legend.background = element_rect(fill = bg, colour = "transparent"),
                   legend.box.background = element_rect(fill = bg, colour = "transparent"))


df <- rbind(
  osm_primary,
  osm_secondary,
  osm_tertiary
)

ttldf <- tibble(label = site %>% str_to_upper() %>% str_sub(0,3),
                x = bb %>% unlist %>% .[1] + 0.01,
                y = bb %>% unlist %>% .[4] - 0.02,
                angle = 90, color = colpal[1], label.color = NA, fill = NA
)

colpal <- sequential_hcl(df[var1] %>% n_distinct(), colv)

var1 <- "max"
var2 <- ""
size <- 0.4
colpal <- sequential_hcl(df[var1] %>% n_distinct(), "PinkYl") 
ggplot() + 
  geom_sf(data = df %>% filter(highway %in% o1), aes(col = .data[[var1]], fill = .data[[var1]]),size = size) +
  geom_sf(data = df %>% filter(highway %in% o2), aes(col = .data[[var1]], fill = .data[[var1]]),size = size/2) +
  geom_sf(data = df %>% filter(highway %in% o3), aes(col = .data[[var1]], fill = .data[[var1]]),size = size/3) +
  scale_fill_gradientn(name = paste0(ttl,"\n"), colours = adjustcolor(colpal,opac),aesthetics = c("col","fill"), na.value = col_na) +
  geom_sf(data = sfcent, col = col_na, fill = adjustcolor(col_na,opac), size = 2) +
  ggspatial::annotation_scale(location = 'bl',bar_cols =c(bg,fg), line_width = 0.5, pad_x = unit(1.5, "cm"), pad_y = unit(0.1, "cm"), text_col = fg, line_col = fg, style = "ticks", width_hint = 0.1) +
  ggthemes::theme_map() +
  theme(legend.background = element_rect(fill="transparent", color = "transparent")) +
  coord_sf(xlim = bb[c(1,3)], bb[c(2,4)],crs = prj) +
  my_theme 

ggsave(here::here(fh,"plot",paste(sites[1],".png", sep ="_")), device = "png", width = width, height = height, units = "cm", dpi = "retina", bg = "transparent")
