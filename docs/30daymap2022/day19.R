##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                            #30daymapchallenge2022                        ----
##                                day 19 - globe                             ~~~~
##                            author: Matt Malishev                         
##                               @darwinanddavis                            
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

fh <- "day19"
citation <- "https://www.globalfishingwatch.org"

# pcks ----------------------------------------------------------
pacman::p_load(here,dplyr,ggplot2,readr,ggthemes,colorspace,sf,rnaturalearth,lubridate,svglite,rgdal,stringr)

# data ----------------------------------------------------------
bb <- maps::world.cities %>% filter(country.etc == "Australia", name == "Sydney") 
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

# plot globe
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
hex_polygons_china <- readRDS(here::here("data","r",paste0(locc,"_activity_2020_all_hex.Rda"))) %>% filter(fill != 0)  # w/o projection
hex_polygons_china <- hex_polygons %>% filter(fill != 0)


# set proj ------------------------------------------------------
bb <- maps::world.cities %>% filter(country.etc == "Australia", name == "Brisbane") # %>% st_as_sf(coords = c("long","lat"), crs = prj) %>% st_buffer(1.2*10^6) %>% st_bbox
crsn <- 2163
plat <- bb[,"lat"]
plon <- bb[,"long"]
prj <- paste0("+lat_0=",plat," +lon_0=",plon," +init=epsg:",crsn) # opt1
dd <- ne_countries("large",returnclass = "sf") %>% st_transform(prj)
hex_polygons_aus <- hex_polygons_aus %>% st_transform(prj)
hex_polygons_china <- hex_polygons_china %>% st_transform(prj)
bbox <- bb %>%  st_as_sf(coords = c("long","lat")) %>% st_buffer(5*10^6) %>%  st_bbox 
colpal <- sequential_hcl(hex_polygons_china$fill %>% n_distinct()  %>% log + 1, "Burg") %>% rev  # colpal for full hex

p <- ggplot() +
  geom_sf(data = hex_polygons_china, aes(fill = fill %>% log + 1, col = fill %>% log + 1),size = 0.05) +
  scale_fill_gradientn(name = "Activity", colours = colpal, aesthetics = c("col"), na.value = "transparent"
                       # , guide = "none"
  ) +
  scale_fill_gradientn(name = "Activity", colours = adjustcolor(colpal,opac), aesthetics = c("fill"), na.value = "transparent", guide = "none") +
  geom_sf(data = dd, aes(geometry = geometry), color = fg %>% lighten(0.8), fill = fg, size = 0.3) +
  theme_map() +
  theme( # regular map bg
  plot.background = element_rect(fill = "transparent", color = "transparent"),
  panel.background = element_rect(fill = "transparent", color = "transparent")
  ,panel.grid.major = element_line(colour = fg %>% darken(0.3), size = 0.1, linetype = 3)
  ,panel.ontop = F
  , legend.background = element_rect(fill = "transparent")
  , legend.text = element_blank()
  , legend.title = element_text(color = colpal[colpal %>% length])
  , legend.position=c(0.05,0.15)
)
p

ttl <- "china_all"
prj <- ""
ggsave(here::here("plot","reds",paste0(ttl,"_",prj,".png")),p,device = "png", width = width, height = height, units = "cm", dpi = "print")


# heatmap of aus vs china fishing hours  ------------------------
df <- china_geartype_2020_all %>% st_drop_geometry() %>% mutate(fishing_prop = fishing_hours/hours)
df$geartype %>% unique
gear <- "trawler" # pole_and_line # pot_and_cage
colv <- "SunsetDark"
df <- df %>% mutate(fishing_prop = fishing_hours/hours) %>%
  filter(geartype == gear) %>% 
  group_by(date) %>% tally(fishing_hours/hours, name = "fishing_prop") %>% 
  mutate("year" = date %>% year,"month" = date %>% month,"day" = date %>% day) %>% 
  filter(fishing_prop !=0)
df$fishing_prop <- df$fishing_prop/df$fishing_prop %>% max

xlab <- "Month"
ylab <- "Day"
legttl <- "Proportion of\nfishing time\n"
nn <- df %>% pull(fishing_prop) %>% n_distinct()
colpal <- c(sequential_hcl(nn,colv) %>% .[nn],sequential_hcl(nn,colv) %>% .[1])
textcol <- colpal[colpal %>% length]
ggplot() +
  geom_tile(data = df, aes(month,day,fill = fishing_prop), col = "transparent",size = 0.2) +
  scale_fill_gradient(name = legttl, low = colpal[1], high = colpal[2], guide = "colourbar",na.value = "transparent") +
  scale_x_continuous(breaks = df$month %>% unique, labels = month(df$month %>% unique,label = T, abbr = T)) +
  scale_y_continuous(breaks = df$day %>% unique) +
  labs(x = xlab, y = ylab) +
  theme(
    plot.background = element_blank(),
    panel.background = element_blank()
    ,panel.grid = element_blank()
    , legend.background = element_rect(fill = "transparent")
    , legend.text = element_text(color = textcol)
    , legend.title = element_text(color = textcol)
    , text = element_text(color = textcol)
    , axis.text = element_text(color = textcol)
    , axis.ticks = element_line(color = textcol)
  )
ggsave(here::here("plot","activity",paste0(ttl,"_",gear,"_activity.png")),device = "png", width = width/2, height = height/2, units = "cm", dpi = "print")
