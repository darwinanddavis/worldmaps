##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                            #30daymapchallenge2023                        ----
##                                day 30 - summary                          ~~~~
##                            author: Matt Malishev                         
##                               @darwinanddavis                            
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# pcks --------------------------------------------------------------------
pacman::p_load(here,dplyr,sf,sfheaders,ggplot2,maps,ggmap,nngeo,rnaturalearth,colorspace,scales)
sf_use_s2(FALSE)

# data --------------------------------------------------------------------
dd <- ne_states("Australia",returnclass = "sf")
bbox <- dd %>% st_bbox() # set bounds 

# plot --------------------------------------------------------------------

# load data from 1980 and everything above 1000 ha
fhh <- c("NSW","VIC","TAS","SA","QLD","WA")
area_min <- 1000
date_min <- "1980-01-01"
k <- 10
cex <- 0.01
opac <- 0.8
colb <- "#2C2C2C"
colt <- "transparent"
width <- 50
my_theme <- theme(plot.background = element_rect(fill = colt, colour = colt), panel.background = element_rect(fill = colt, colour = colt))
for(f in fhh){
  d  <-  here::here("data",f,paste0(f,".shp")) %>% 
    st_read() %>% 
    filter(date > date_min & area_ha > area_min) %>% # get dates and areas
    st_centroid() %>%  # only want centroid of polygons
    filter(is.na(st_dimension(.)) == F) %>% # ditch empty geos
    mutate("state_id" = f) %>% 
    dplyr::select(c(date,area_ha,year,month,day,hour,minute,second,state_id,geometry)) 
  dn <- d %>% st_nn(d, k = k, progress = F) # get nearest neighbour
  dn_dist <- d %>% st_nn(d, k = k, progress = F,returnDist = T) # get nn distance for colpal
  colv <- sequential_hcl(dn_dist$dist %>% unlist %>% length,"BrwnYl")
  d %>% st_connect(d, ids = dn) %>%  # plot
    ggplot() + 
    geom_sf(size = cex, col = colv, alpha = opac, show.legend = F) +
    labs(subtitle = f) +
    coord_sf(xlim = bbox[c(1,3)],ylim = bbox[c(2,4)]) + # set bounding box
    theme_nothing() + my_theme +
    ggspatial::annotation_scale(
      location = "bl",
      bar_cols = c(colpal[1]),
      line_width = 0.5,
      pad_x = unit(1.5,"cm"),
      pad_y = unit(1.5,"cm"),
      text_col = colpal[1], line_col = colpal[1],
      style = "ticks", # "bar"
      width_hint = 0.1 # set scale to 10% of map
    )
  ggsave(paste0(here::here("plot",f,".png")), device = "png", dpi = "retina", width = width, height = width, units = "cm", bg = colt)
}

