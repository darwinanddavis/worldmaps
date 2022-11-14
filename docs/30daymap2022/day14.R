##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                            #30daymapchallenge2022                        ----
##                                day 14 - hexagons                             ~~~~
##                            author: Matt Malishev                         
##                               @darwinanddavis                            
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

fh <- "day14"
citation <- "https://www.globalfishingwatch.org"

# pcks ----------------------------------------------------------
pacman::p_load(here,dplyr,ggplot2,readr,ggthemes,colorspace,sf,rnaturalearth,lubridate,svglite,rgdal,stringr)

# data ----------------------------------------------------------
bb <- maps::world.cities %>% filter(country.etc == "Australia", name == "Sydney") #%>% st_as_sf(coords = c("long","lat"), crs = prj) %>% st_buffer(1.2*10^6) %>% st_bbox 
crsn <- 2163
plat <- bb[,"lat"]
plon <- bb[,"long"]
prj <- paste0("+lat_0=",plat," +lon_0=",plon," +init=epsg:",crsn) # opt1

# latlon, geartype, boat length, engine size, tonnage, cumulative fishing hours per year  
df <- here::here("data","vessels") %>% list.files(full.names = T) %>% read_csv

aus <- df %>% filter(flag_ais == "AUS") 
china <- df %>% filter(flag_ais == "CHN") 

# fishing activity last 10 years (latlon)
fishing_hours <- df %>% filter(flag_ais %in% c("AUS","CHN")) %>% 
  select(mmsi,flag_ais,contains("hours"))

# fishing vessel type (latlon) 
read_geartype_all <- function(dh)  here::here("data","daily_activity","fleet-daily-csvs-100-v2-2020") %>% list.files(full.names = T) %>% read_csv %>% filter(flag %in% dh) %>% select(-mmsi_present) %>% st_as_sf(coords = c("cell_ll_lon","cell_ll_lat"), crs = 4326)
china_geartype_2020_all <- read_geartype_all("CHN")
aus_geartype_2020_all <- read_geartype_all("AUS")
aus_geartype_2020_all %>% st_drop_geometry %>% pull(date) %>% unique
china_geartype_2020_all  <-  readRDS(here::here("data","r","china_geartype_2020_all.Rda"))
aus_geartype_2020_all  <-  readRDS(here::here("data","r","aus_geartype_2020_all.Rda"))

# get daily activity and latlon
fh <- "2020-01-01"
activity <- here::here("data","daily_activity","mmsi-daily-csvs-10-v2-2020") %>% list.files(fh,full.names = T) %>% read_csv 
aus_activity <- activity %>% filter(mmsi %in% aus$mmsi) %>% st_as_sf(coords = c("cell_ll_lon","cell_ll_lat"), crs = 4326) %>% st_transform(prj)
china_activity <- activity %>% filter(mmsi %in% china$mmsi)  %>% st_as_sf(coords = c("cell_ll_lon","cell_ll_lat"), crs = 4326) %>% st_transform(prj)

# all of 2020
read_activity_all <- function(dh)  here::here("data","daily_activity","mmsi-daily-csvs-10-v2-2020") %>% list.files(full.names = T) %>% read_csv %>% filter(mmsi %in% dh)  %>% st_as_sf(coords = c("cell_ll_lon","cell_ll_lat"), crs = 4326) #%>% st_transform(prj)
china_activity_2020_all <- read_activity_all(china$mmsi)
aus_activity_2020_all <- read_activity_all(aus$mmsi)
china_activity_2020_all  <-  readRDS(here::here("data","r","china_activity_2020_all.Rda"))
aus_activity_2020_all  <-  readRDS(here::here("data","r","aus_activity_2020_all.Rda"))


# world map 
dd <- ne_countries("medium",returnclass = "sf") %>% 
  st_transform(prj)

# plot ---------------------------------------------------------------------
height <- 30
width <- 30
opac <- 0.5
fg <- "#333333" %>% lighten(0.3) 
bg <- "#A8BCCD" %>% lighten(0.1)
my_theme <- theme(
  plot.background = element_rect(fill = bg, color = "transparent"),
  panel.background = element_rect(fill = bg, color = "transparent"),
  panel.grid.major = element_line(colour = fg %>% darken(0.1)),
  legend.background = element_rect(fill = "transparent")
)

# fishing hours 
d <- fishing_hours %>% reshape2::melt(id = "flag_ais") %>% filter(variable != "mmsi")
colpal <- sequential_hcl(d$variable %>% n_distinct(), "BluGrn") 
ggplot() +
  geom_bar(data = d, aes(variable, value,col = variable), stat = "identity") +
  scale_fill_manual(name = "Fishing hours", values = colpal, aesthetics = c("fill","col")) +
  coord_flip() +
  facet_grid(~flag_ais) 

### activity
col_china <- "#7D0112"
col_aus <- "#C6699C"
ggplot() +
  geom_sf(data = china_activity, aes(size = fishing_hours/10), col = adjustcolor(col_china,opac), show.legend = F) +
  geom_sf(data = aus_activity, aes(size = fishing_hours/10), col = adjustcolor(col_aus,opac), show.legend = F) +
  # geom_sf(data = aus_activity_2020_all, size = 0.1, col = adjustcolor(col_aus,opac), show.legend = F) +
  geom_sf(data = dd, aes(geometry = geometry), color = adjustcolor(fg,opac), fill = fg, size = 0.1, inherit.aes = F) +
  theme_map() +
  my_theme


# hex
# set new memory allocation for this session https://stackoverflow.com/questions/51248293/error-vector-memory-exhausted-limit-reached-r-3-5-0-macos
usethis::edit_r_environ()
usethis::edit_r_environ("project")
Sys.setenv('R_MAX_VSIZE' = 6.4*10^9)
fh <- "2020" # all files
activity <- here::here("data","daily_activity","mmsi-daily-csvs-10-v2-2020") %>% list.files(fh,full.names = T) %>% read_csv 
df_hex <- activity %>% filter(
  fishing_hours != 0 & # rm 0 fishing hours 
    mmsi %in% china$mmsi) %>% # china or aus
  st_as_sf(coords = c("cell_ll_lon","cell_ll_lat"), crs = 4163) 
hex_points <- df_hex %>% as_Spatial() %>% spsample(type = "hexagonal", cellsize = 0.3)
hex_polygons <- HexPoints2SpatialPolygons(hex_points) %>%
  st_as_sf(crs = st_crs(df_hex)) # %>% st_intersection(., df_hex)
hex_polygons$fill <- lengths(st_intersects(hex_polygons, df_hex))
# save to dir
locc <- "china"
hex_polygons %>% saveRDS(here::here("data","r",paste0(locc,"_activity_2020_all_hex.Rda")))


# plot hex2
# bb <- maps::world.cities %>% filter(country.etc == "Japan", name == "Fukuoka") # %>% st_as_sf(coords = c("long","lat"), crs = prj) %>% st_buffer(1.2*10^6) %>% st_bbox 
bb <- maps::world.cities %>% filter(country.etc == "Australia", name == "Brisbane") # %>% st_as_sf(coords = c("long","lat"), crs = prj) %>% st_buffer(1.2*10^6) %>% st_bbox
crsn <- 2163
plat <- bb[,"lat"]
plon <- bb[,"long"]
prj <- paste0("+lat_0=",plat," +lon_0=",plon," +init=epsg:",crsn) # opt1
dd <- ne_countries("large",returnclass = "sf") %>% st_transform(prj)
hex_polygons_aus <- hex_polygons_aus %>% st_transform(prj)
hex_polygons_china <- hex_polygons_china %>% st_transform(prj)
bbox <- bb %>%  st_as_sf(coords = c("long","lat")) %>% st_buffer(5*10^6) %>%  st_bbox 

bbox <- data.frame("lon" = c(92.972933, -132.706889),
                   "lat" = c(38.087688,-16.038734)) %>%  st_as_sf(coords = c("lon","lat"), crs = 4326) %>% 
  st_transform(prj) %>% st_bbox
dd <- ne_countries("large",returnclass = "sf") %>% st_transform(prj)
hex_polygons_aus <- hex_polygons_aus %>% st_transform(prj)
hex_polygons_china <- hex_polygons_china %>% st_transform(prj)

# hex with buffer decay --------
# full hex --------
colpal <- sequential_hcl(hex_polygons$fill %>% n_distinct()  %>% log + 1, "Dark Mint") %>% rev   # colpal for full hex
p <- ggplot() +
  geom_sf(data = hex_polygons, aes(fill = fill %>% log + 1, col = fill %>% log + 1),size = 0.05) +
  geom_sf(data = hex_polygons_aus, aes(fill = fill %>% log + 1, col = fill %>% log + 1),size = 0.05) +
  scale_fill_gradientn(name = "Activity", colours = colpal, aesthetics = c("col"), na.value = "transparent"
                       # , guide = "none"
  ) +
  scale_fill_gradientn(name = "Activity", colours = adjustcolor(colpal,opac), aesthetics = c("fill"), na.value = "transparent", guide = "none") +
  geom_sf(data = dd, aes(geometry = geometry), color = fg %>% lighten(0.2), fill = fg, size = 0.3) +
  coord_sf(xlim = c(bbox[1],bbox[3]), ylim = c(bbox[2],bbox[4])) +
  theme_map() +
  theme( # regular map bg 
  plot.background = element_rect(fill = fg %>% darken(0.3), color = "transparent"),
  panel.background = element_rect(fill = fg %>% darken(0.3), color = "transparent")
  ,panel.grid.major = element_line(colour = fg %>% lighten(0.5), size = 0.1, linetype = 3)
  ,panel.ontop = F
  , legend.background = element_rect(fill = "transparent")
  , legend.text = element_blank()
  , legend.title = element_text(color = fg %>% lighten(0.2))
  , legend.position=c(0.15,0)
)
p
ttl <- "aus_all_hex2_transparent"
prj <- ""
ggsave(here::here("plot",paste0(ttl,"_",prj,".png")),p,device = "png", width = width, height = height, units = "cm", dpi = "print")

# all 2020 data -------------------------------------------------
d <- activity %>% filter(fishing_hours != 0 & mmsi %in% aus$mmsi)
var1 <- "fishing_hours"
colpal <- sequential_hcl(d[,var1] %>%  n_distinct(), "OrRd") %>% rev 
ggplot() +
  geom_tile(data=d, aes(cell_ll_lon,cell_ll_lat,fill=.data[[var1]]),stat = "identity") +
  # geom_sf(data = dd, aes(geometry = geometry), color = adjustcolor(fg,opac), fill = fg, size = 0.1, inherit.aes = F) +
  scale_fill_gradientn(name = "Fishing hours", colors = colpal, aesthetics = c("fill","col")) +
  theme_map() +
  my_theme





