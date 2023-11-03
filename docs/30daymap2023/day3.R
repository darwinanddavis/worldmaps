##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                            #30daymapchallenge2023                        ----
##                                day 3 - polygons                                 ~~~~
##                            author: Matt Malishev                         
##                               @darwinanddavis                            
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# pcks --------------------------------------------------------------------
pacman::p_load(sf,rmapshaper,sf,ggplot2,colorspace,here,rnaturalearth)

# get data --------------------------------------------------------------------
dd <- ne_states("Australia", returnclass = "sf") # aus polygons

# get state level shp files from global fire dataset
fh <- here::here("bushfire","data","Historical Bushfire Boundaries.gdb","a00000009.gdbtable")
fhl <- fh %>% st_layers() %>% .[[1]] # get layer names
a7 <- fh %>% st_read(layer = fhl) 
ii <- a7 %>% pull(state) %>% unique 

# sep into aus states
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
  dir.create(here::here("bushfire","data",fhh))
  df %>% st_write(here::here("bushfire","data",fhh,paste0(fhh,".shp")))
}


# sort by largest area  -------------------------------------------------
fhh <- c("ACT","NSW","VIC","TAS","SA","WA","QLD") 
fhh <- "VIC"
d_area <- c()
# quant <- 0.999
topn <- 5
keep <-  0.05
for(f in fhh){
  # sort by top area by state
  d <- paste0(dirr,"/data/",f,"/",f,".shp") %>% 
    st_read %>% 
    mutate("state_id" = f) %>% 
    arrange(area_ha %>% desc) %>% 
    head(topn) %>% # or get topn
    ms_simplify(keep = keep)
  d_area <- rbind(d_area,d)
}
d_area <- d_area %>% filter(state_id %in% c("NSW","VIC","SA","WA","QLD")) 


# plot --------------------------------------------------------------------
# create custom colpal
width <- 20
fg <- "#3A2830"
colpal <- "#D1935B" 
colv <- c(); for(nn in 1:5){colv <- c(colpal %>% lighten(nn/10) %>% lighten(nn/10),colv)}
d_area$colpal <- colv %>% rev %>%  rep(5) 

# plot
ggplot() + # area
  geom_sf(data = dd, col = fg %>% lighten(0.5), fill = "transparent", size = 0.1) +
  geom_sf(data =  d_area, 
          aes(colour = colpal, fill = adjustcolor(colpal,opac))
          ,show.legend = F) +
  scale_colour_manual(values = d_area$colpal %>% unique, aesthetics = "colour") +
  scale_fill_manual(values = d_area$colpal %>% unique %>% adjustcolor(opac), aesthetics = "fill") +
  ggmap::theme_nothing()
ggsave(paste0(dirr,"/plot/aus.png"), device = "png", dpi = "retina", width = width, height = width, units = "cm")








