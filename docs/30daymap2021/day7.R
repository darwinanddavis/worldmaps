##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                            #30daymapchallenge2021                        ----
##                                day 7 - green                                 ~~~~
##                            author: Matt Malishev                         
##                               @darwinanddavis                            
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# source --------------------------------------------------------
# https://data.noaa.gov/metaview/page?xml=NOAA/NESDIS/NGDC/MGG/Hazards/iso/xml/G012153.xml&view=getDataView
citation <- "World Bank | Protected Planet"  

# pcks -----------------------------------------------------
pacman::p_load(here,dplyr,readr,ggplot2,sf,sp,ggmap,rnaturalearth,rnaturalearthdata,maptools,scales,rgdal,colorspace,mapdata,ggsn,mapview,mapproj,ggthemes,grid,ggtext,rworldxtra,stringr,ggtext,showtext,ggimage,grid)
fh <- "day7"
df_forest <- here::here("data","30daymap2021",paste0(fh,"_forest_percent.csv")) %>% read_csv(trim_ws = T,skip_empty_rows = T, skip = 3)
df_agri <- here::here("data","30daymap2021",paste0(fh,"_agri_percent.csv")) %>% read_csv(trim_ws = T,skip_empty_rows = T, skip = 3)
img <- here::here("img","30daymap2021","day7.svg")
plot_inset <- here::here("img","30daymap2021","day7_donut.png") %>% readPNG() 

# add font
newfonts <- "/Fonts/Dusha.ttf"
fontlib <- "dusha"
font_add(fontlib,regular = newfonts,bold = newfonts)
showtext_auto(enable = T) # auto showtext

# clean data ----------------------------------------------------
shp <- here::here("data","30daymap2021",paste0(fh,"_parks_brazil.Rda")) %>% readRDS()
forest <- df_forest %>% filter(`Country Name` == "Brazil") %>% 
  select(!c(`Country Code`,`Indicator Name`,`Indicator Code`)) %>% 
  select(!`1960`:`1989`)
forest <- as_tibble(t(forest[,-1]), rownames = "row_names") %>%  # convert cols to rows 
  rename("year" = 1,"percent" = 2) %>% na.omit()
agri <- df_agri %>% filter(`Country Name` == "Brazil") %>% 
  select(!c(`Country Code`,`Indicator Name`,`Indicator Code`))
agri <- as_tibble(t(agri[,-1]), rownames = "row_names") %>%  # convert cols to rows 
  rename("year" = 1,"percent" = 2) %>% na.omit()

# clean data for donut 
shp_desig <- shp %>% 
  select(DESIG_ENG) %>% 
  count(DESIG_ENG, sort = T) %>% 
  head(5)
shp_desig$fraction = shp_desig$n / sum(shp_desig$n)
# get min and max for each segment 
shp_desig$ymax = cumsum(shp_desig$fraction)
shp_desig$ymin = c(0, head(shp_desig$ymax, n=-1))



# world data ----------------------------------------------------
dd <- rnaturalearth::ne_countries("large", 
                                  type = "sovereignty",
                                  returnclass = "sf") %>% 
  filter(continent %in% c("South America",
                          "North America",
                          "Antarctica")) %>% 
  filter(!name %in% c("Mexico","Canada","United States of America"))
brazil <- dd %>% filter(name == "Brazil")

# physical
rivers_ne <- here::here("data","30daymap2021",paste0(fh,"_rivers_ne.Rda")) %>% readRDS()
urban_areas <- here::here("data","30daymap2021",paste0(fh,"_urban_areas.Rda")) %>% readRDS()

# prj -----------------------------------------------------------
# add_graticule <- T
# prj <- 5362 # 5387 5360 5361 5362 5358
prj <- "+proj=stere +lat_0=-90"

dd <- dd %>% st_transform(prj) # set proj for polygon data
brazil <- brazil %>% st_transform(prj)
shp <- shp %>% st_transform(prj) 
rivers_ne <- rivers_ne %>% st_transform(prj) 
urban_areas <- urban_areas %>% st_transform(prj) 
bath1000 <- bath1000 %>% st_transform(prj) 
bath5000 <- bath5000 %>% st_transform(prj) 
bbox <- brazil %>% st_buffer(600000) %>% st_bbox

# plot --------------------------------------------------------------------
# params
fg <- "#01130C"
bg <- "#263431"
border <- "#525E54"
col_na <- "#EFEFEF"
colv_hi <- "#CAE4CB"
colv_lo <- "#0C640F"
colfunc1 <- colorRampPalette(c(colv_hi,colv_lo)) 
colvec <- colfunc1(shp_desig$DESIG_ENG %>% unique %>% length) 
col_font <- colv_hi
family <- fontlib
font_size <- 45
size <- 3
opac <- 0.8
angle <- 90
xmax <- 4 # donut plot x
xmin <- 3 # donut plot x
inset_xmin <- brazil %>% st_buffer(-1000) %>% st_bbox() %>% .[1] # inset xmin
inset_ymin <- brazil %>% st_buffer(-900000) %>% st_bbox() %>% .[4] # inset ymin
ic <- 4 # inset plot magnification 
ttl <- tibble("label" = "BRAZIL",
              "sub" = "Classifying major ecoregions",
              "lon" = brazil %>% st_buffer(2 * 10^5) %>% st_bbox() %>% .[3],
              "lat" = brazil %>% st_buffer(-1.5 * 10^6) %>% st_bbox() %>% .[2])
credits <- tibble("label" = "Data: Worldbank.org | Matt Malishev | @darwinanddavis",
                  "lon" = brazil  %>% st_bbox() %>% .[3],
                  "lat" = brazil %>% st_buffer(-8 * 10^5) %>% st_bbox() %>% .[2])
subttl_xy <- 100 # subtitle xy 

# plot ----------------------------------------------------------

# donut plot 
donut <- ggplot() +
  geom_rect(data = shp_desig, aes(ymax=ymax, ymin=ymin, xmax=xmax, xmin=xmin, fill=n), col = bg, show.legend = F, size = 1) +
  geom_image(data = NULL,aes(1,0,image = img), size = 0.5) +
  scale_fill_gradientn(name = "Ecotype", colours = colvec,aesthetics = c("fill")) +
  coord_polar(theta="y") + 
  xlim(c(xmin-2, xmax)) +
  theme_nothing()
ggsave(here::here("img","30daymap2021/") %>% paste0(fh,"_donut.png"),donut, device = "png", dpi = "retina", width = 10, height = 10, units = "cm", bg = "transparent")  
plot_inset <- here::here("img","30daymap2021","day7_donut.png") %>% readPNG() 

# map
m7 <- ggplot() +
  geom_text(data = ttl, aes(lon-1*10^6,lat,label=label),check_overlap = T,family = family,size=font_size, col = col_font, angle = angle) +
  geom_text(data = ttl, aes(lon,lat,label=sub),check_overlap = T,family = family,size=font_size/4, col = col_font, angle = angle) +
  geom_sf(data = dd,fill = fg, col = border, size = 0.1) +
  geom_sf(data = brazil,fill = lighten(fg,0.1), col = border, size = 0.2) +
  geom_sf_label(data = dd %>% filter(continent == "South America"), aes(label=name),family=family, color=border, size = font_size/15, label.size = 0, fill = NA, angle = 45) +
  geom_sf(data = urban_areas, fill = adjustcolor(border,0.4), col = NA, size = 0) +
  geom_sf(data = rivers_ne, fill = NA, col = darken(bg,0.1), size = 0.1) + # add rivers
  geom_sf(data = shp_desig,aes(geometry = geometry,col= DESIG_ENG,fill= DESIG_ENG), size = 0.01, show.legend = F) +
  scale_fill_manual(name = "Ecotype", values = colvec, aesthetics = "col") +
  scale_fill_manual(name = "Ecotype", values = adjustcolor(colvec,0.8), aesthetics = "fill") +
  coord_sf(crs = prj, xlim = c(bbox[1],bbox[3]), ylim = c(bbox[2],bbox[4])) +
  labs(x=NULL,y=NULL) +
  # add donut inset
  annotation_raster(plot_inset,xmin = inset_xmin, xmax = inset_xmin + ic * 10^6, ymin = inset_ymin ,ymax = inset_ymin + ic * 10^6) +
  # themes
  theme_nothing()+
  theme(panel.grid.major = element_line(colour = border, linetype = 3, size = 0.2),
        plot.background = element_rect(fill = "transparent", colour = NA),
        panel.background = element_rect(fill = "transparent"),
        axis.text = element_blank(), 
        axis.ticks.length=unit(0, "null"),
        plot.margin=unit(c(0.1,0.1,0.1,0.1),"mm"),
        panel.ontop = F) +
  geom_text(data = credits, aes(lon,lat,label=label),check_overlap = T,family = family,size=font_size/5, col = col_font)

# save --------------------------------------------------------------------
ggsave(here::here("img","30daymap2021/") %>% paste0(fh,".png"),m7, device = "png", dpi = "retina", width = 20, height = 20, units = "cm",bg = "transparent")  

