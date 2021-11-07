##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                            #30daymapchallenge2021                        ----
##                                day 4 - hexagons                           ~~~~
##                            author: Matt Malishev                         
##                               @darwinanddavis                            
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# source --------------------------------------------------------
# http://worh.org/library/bars-vs-grocery-stores-mapping-data
# https://www.r-graph-gallery.com/hexbin-map.html

citation <- ""

# pcks -----------------------------------------------------
pacman::p_load(here,dplyr,readr,ggplot2,sf,sp,ggmap,rnaturalearth,rnaturalearthdata,maptools,scales,rgdal,colorspace,mapdata,ggsn,mapview,mapproj,ggthemes,grid,ggtext,rworldxtra,stringr,ggtext,showtext)
fh <- "day4"
day4 <- here::here("data","30daymap2021",paste0(fh,".xlsx")) %>% 
  readxl::read_xlsx(trim_ws = T) %>% 
  rename("birds_struck" = 29,
         "bird_size" = 37,
         "bird_mass" = 38) %>% 
  mutate_at("Altitude", as.numeric) 

# wrangle data ----------------------------------------------------

# facet by PhaseOfFlight, SpeciesFamily, 

ggplot() +
  geom_hex(data = day4,aes(Longitude,Latitude,col = bird_size), bins=500)
  


# spatial data -----------------------------------------------------------

dd <- rnaturalearth::ne_states(c("Australia","New Zealand"), returnclass = "sf") 

# plot --------------------------------------------------------------------
# params
colpal <- sequential_hcl(df$depth %>% unique %>% length,"Reds 3") %>% rev()
fg <- "#455B68"
bg <- "#8095A1"
border <- "#30414A"
col_na <- "#EFEFEF"
col_mag <- "#000000"
col_font <- colpal %>% tail(1)
family <- fontlib
font_size <- 9
size <- 3
opac <- 0.5
angle <- 30
ttl <- tibble("label" = "Rumble in SE Asia",
              "sub" = "500 years of",
              "base" = "earthquakes" %>% str_to_upper(),
              "lon" = 19934001,
              "lat" = 14044799)
credits <- tibble("label" = "Data: NOAA | Matt Malishev | @darwinanddavis",
                  "lon" = 20534001,
                  "lat" = 12344799)

bbox <- dd %>% filter(name == "New South Wales") %>% 
  st_centroid() %>% 
  st_buffer(20) %>% 
  st_bbox()

ggplot() +
  # geom_text(data = ttl, aes(lon,lat,label=label),check_overlap = T,family = family,size=font_size, col = col_font, angle = angle) +
  # geom_text(data = ttl, aes(lon,lat - 500000,label=sub),check_overlap = T,family = family,size=font_size, col = col_font, angle = angle) +
  # geom_text(data = ttl, aes(lon,lat - 1100000,label=base),check_overlap = T,family = family,size=font_size+1, col = col_font, angle = angle) + geom_text(data = ttl, aes(lon,lat - 1300000,label=base),check_overlap = T,family = family,size=font_size, col = alpha(col_font,opac-0.2), angle = angle-3) + geom_text(data = ttl, aes(lon,lat - 1200000,label=base),check_overlap = T,family = family,size=font_size-1, col = alpha(col_font,opac-0.3), angle = angle+3) +
  geom_sf(data=dd , fill = fg, col = border, size = 0.2) +
  geom_hex(data = day4[1:20000,], aes(Longitude, Latitude), bins = 200) +
  # facet_wrap(~bird_size)
  coord_sf(xlim = c(bbox[1],bbox[3]), ylim = c(bbox[2],bbox[4]))


# for sf (with x = st_as_sf(meuse.sr))
ddhex <- st_make_grid(st_as_sf(dd), 
                      square=FALSE,
                      cellsize = 1,
                      n = 200,
                      # crs = prj,
                      flat_topped = T)[st_as_sf(dd)]


ggplot() + 
  geom_sf(data = ddhex, size = 0.2) +
  geom_hex(data = day4, aes(Longitude, Latitude), bins = 500) +
  coord_sf(xlim = c(bbox[1],bbox[3]), ylim = c(bbox[2],bbox[4]))

