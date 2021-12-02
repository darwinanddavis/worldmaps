

# pcks -----------------------------------------------------
pacman::p_load(here,dplyr,readr,ggplot2,sf,sp,ggmap,rnaturalearth,rnaturalearthdata,maptools,scales,rgdal,colorspace,mapdata,ggsn,mapview,mapproj,ggthemes,grid,ggtext,rworldxtra,stringr,ggtext,showtext)
fh <- "day18"

dd <- rnaturalearth::ne_countries("large", # add surrounding countries
                                  type = "sovereignty",
                                  returnclass = "sf") # get country level data
dd2 <- dd %>% filter(name == "Australia")
bbox <- dd2 %>% st_bbox()

# bathymetry
add_bath_layer <- function(depth) "/Volumes/Matt_timemachine/maptracks/shapefiles/bathymetry/bathymetry_" %>% paste0(i,".Rda") %>% readRDS()
bath_full <- c()
for(i in c(200,seq(1000,10000,1000))){
  bath_full <- rbind(bath_full,add_bath_layer(i))
}

# params
colv <- "#CD3DF8"
bath_full <- bath_full %>% 
  mutate(col = 
    case_when(depth == 200 ~ colv,
              depth == 1000 ~ colv %>% darken(0.1),
              depth == 2000 ~ colv %>% darken(0.2),
              depth == 3000 ~ colv %>% darken(0.3),
              depth == 4000 ~ colv %>% darken(0.4),
              depth == 5000 ~ colv %>% darken(0.5),
              depth == 6000 ~ colv %>% darken(0.6),
              depth == 7000 ~ colv %>% darken(0.7),
              depth == 8000 ~ colv %>% darken(0.8),
              depth == 9000 ~ colv %>% darken(0.9),
              depth == 10000 ~ colv %>% darken(0.95)
              )
  )
bath_full$col %>% unique %>% scales::show_col()



# proj ----------------------------------------------------------

crs_codes = rgdal::make_EPSG() # get all espg code and info
prj_opt <- crs_codes[crs_codes$note %>% str_which("stil"),"prj4"]
crs_codes %>% filter(code %in% prj_opt) %>% pull(prj4) # pull espg name and code 

crsn <- 3857  
plat <- 54.53592823593436
plon <- -66.65424634230524
prj <- paste0("+lat_0=",plat," +lon_0=",plon," +init=epsg:",crsn) # opt1
prj <- "+proj=stere +lat_0=180 +lon_0=0 +k=1 +x_0=0 +y_0=0 +ellps=clrk66 +units=m +no_defs"
prj <- 3032

bath_full <- bath_full %>% st_transform(crs = prj)
dd <- dd %>% st_transform(crs = prj)
dd2 <- dd2 %>% st_transform(crs = prj)

ggplot() +
  geom_sf(data = bath_full %>% filter(depth == 200), aes(fill = colv, col = colv) ,size = 0.05, show.legend = F) +
  geom_sf(data = bath_full %>% filter(depth == 1000), aes(fill = colv %>% darken(0.5), col = colv %>% darken(0.5)) ,size = 0.05, show.legend = F) +
  geom_sf(data = dd, fill = "#000000", col = NA, size = 0) + # country base
  geom_sf(data = dd2, fill = "#000000", col = NA, size = 0) + # country base
  # geom_sf(data = bath1000, fill = darken(bg, 0.3),size = 0, col = NA) +
  # geom_sf(data = bath2000, fill = darken(bg, 0.4),size = 0, col = NA) +
  # geom_sf(data = bath3000, fill = darken(bg, 0.5),size = 0, col = NA) +
  # geom_sf(data = bath5000, fill = darken(bg, 0.6),size = 0, col = NA) +
  # geom_sf(data = bath8000, fill = darken(bg, 0.7),size = 0, col = NA) +
  # geom_sf(data = bath10000, fill = darken(bg, 0.8),size = 0, col = NA) +
  coord_sf(xlim = c(bbox[1],bbox[3]), ylim = c(bbox[2],bbox[4])) + # use bbox from proj data ie pathm
  theme_nothing() +
  theme(panel.grid.major = element_line(colour = "transparent"),
        plot.background = element_rect(fill = "transparent"),
        axis.text = element_blank(), 
        axis.ticks.length=unit(0, "null"),
        # plot.margin=unit(c(-10,-10,-10,-10),NULL),
        panel.ontop = F
  ) +
  labs(x = NULL, y = NULL)

