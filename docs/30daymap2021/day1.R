##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                            #30daymapchallenge2021                        ----
##                                day 1 - points                                 ~~~~
##                            author: Matt Malishev                         
##                               @darwinanddavis                            
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# source --------------------------------------------------------
# https://data.noaa.gov/metaview/page?xml=NOAA/NESDIS/NGDC/MGG/Hazards/iso/xml/G012153.xml&view=getDataView
citation <- "National Geophysical Data Center / World Data Service (NGDC/WDS): NCEI/WDS Global Significant Earthquake Database. NOAA National Centers for Environmental Information. doi:10.7289/V5TD9V7K"

# pcks -----------------------------------------------------
pacman::p_load(here,dplyr,readr,ggplot2,sf,sp,ggmap,rnaturalearth,rnaturalearthdata,maptools,scales,rgdal,colorspace,mapdata,ggsn,mapview,mapproj,ggthemes,grid,ggtext,rworldxtra,stringr,ggtext,showtext)
fh <- "day1"
day1 <- here::here("data","30daymap2021",paste0(fh,".tsv")) %>% 
  read_tsv(trim_ws = T,skip_empty_rows = T)

# add font
newfonts <- "/Fonts/CaslonAntique-WJV.ttf"
fontlib <- "caslon"
font_add(fontlib,regular = newfonts,bold = newfonts)
showtext_auto(enable = T) # auto showtext

# clean data ----------------------------------------------------
df <- day1 %>% select(Year,Country,Area,Region,Latitude,Longitude,`Focal Depth (km)`,Mag,`Damage ($Mil)`) %>% 
  rename("lat" = 5,
         "lon" = 6,
         "depth" = 7) %>% 
  mutate_at(c("Year","lon","lat","Mag","depth"),as.numeric) %>% 
  select(lat,lon,Year,Mag,depth) %>% 
  filter(Year > 2021-500) %>% # select last 500 years
  na.omit %>% 
  st_as_sf(coords = c("lon","lat"), crs = 4326)

# world data ----------------------------------------------------
dd <- rnaturalearth::ne_countries("medium", 
                                  type = "sovereignty",
                                  returnclass = "sf")

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

# prj -----------------------------------------------------------
add_graticule <- T
prj <- 3032 #24370 
dd <- dd %>% st_transform(prj) # set proj for polygon data
df$geometry <- df$geometry %>% 
  sf::st_transform(crs = prj) # now transform proj

custom_latlon <- data.frame("lon" = c(90, 140),
                            "lat" = c(7, 17))
bbox <- custom_latlon %>% # turn custom bbox into sf 
  st_as_sf(coords = c("lon","lat"), crs = 4326) %>% 
  st_transform(prj) %>% st_bbox()


m1 <- ggplot() +
  geom_text(data = ttl, aes(lon,lat,label=label),check_overlap = T,family = family,size=font_size, col = col_font, angle = angle) +
  geom_text(data = ttl, aes(lon,lat - 500000,label=sub),check_overlap = T,family = family,size=font_size, col = col_font, angle = angle) +
  geom_text(data = ttl, aes(lon,lat - 1100000,label=base),check_overlap = T,family = family,size=font_size+1, col = col_font, angle = angle) + geom_text(data = ttl, aes(lon,lat - 1300000,label=base),check_overlap = T,family = family,size=font_size, col = alpha(col_font,opac-0.2), angle = angle-3) + geom_text(data = ttl, aes(lon,lat - 1200000,label=base),check_overlap = T,family = family,size=font_size-1, col = alpha(col_font,opac-0.3), angle = angle+3) +
  geom_sf(data = df,aes(geometry = geometry,col=depth,fill=depth), size = size) + # add cities
  scale_fill_gradientn(name = leg_col, colours = adjustcolor(colpal,opac),aesthetics = c("col","fill"), na.value = col_na) +
  geom_sf(data=dd , fill = fg, col = border, size = 0.2) + 
  geom_sf_label(data=dd %>% filter(subregion == "South-Eastern Asia"), aes(label=name),family=family, color=border, size = font_size/6, label.size = 0, fill = NA) +
  coord_sf(crs = prj, xlim = c(bbox[1],bbox[3]), ylim = c(bbox[2],bbox[4])) +
  labs(x=NULL,y=NULL) +
  theme(panel.grid.major = element_line(colour = border, linetype = 3, size = 0.2),
        plot.background = element_rect(fill = bg),
        panel.background = element_rect(fill = "transparent"),
        axis.text = element_blank(), 
        axis.ticks.length=unit(0, "null"),
        plot.margin=unit(c(0.1,0.1,0.1,0.1),"mm"),
        panel.ontop = F,
        legend.position=c(0.1,0.22),
        legend.key.size = unit(0.5, "cm"),
        legend.background = element_blank(),
        legend.title = element_text(family = family,colour = col_font),
        legend.text = element_text(family = family,colour = col_font)
  ) +
  geom_text(data = credits, aes(lon,lat,label=label),check_overlap = T,family = family,size=font_size/3, col = col_font, angle = angle-2)

# save --------------------------------------------------------------------
ggsave(here::here("img","30daymap2021/") %>% paste0(fh,".png"),m1, device = "jpg", dpi = "retina", width = 20, height = 20, units = "cm")
