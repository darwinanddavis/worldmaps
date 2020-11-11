# --------------------------------------------------------------------
# 30 day map challenge 2020
# day 10
# author: Matt Malishev
# @darwinanddavis  

# pkgs --------------------------------------------------------------------
pacman::p_load(dplyr,readr,RCurl,png,jpeg,mapdata,rvest,xml2,magrittr,sp,ggplot2,stringr,ggthemes,ggnetwork,colorspace,ggtext,ggsn,ggspatial)



# data --------------------------------------------------------------------

d <- map_data("worldHires","japan") # \mapdata

# map ---------------------------------------------------------------------

img <- "https://m.media-amazon.com/images/M/MV5BM2ZiZTk1ODgtMTZkNS00NTYxLWIxZTUtNWExZGYwZTRjODViXkEyXkFqcGdeQXVyMTE2MzA3MDM@._V1_UX182_CR0,0,182,268_AL_.jpg"
imgr <- img %>% getURLContent %>% readJPEG

bg <- "black"

p <- ggplot() +
  annotation_raster(imgr, -Inf, Inf, -Inf, Inf) +
  geom_polygon(data=d,aes(long,lat,group=group),
               fill="transparent",col="black",size=0.2,
               rule = "evenodd") + 
  theme_blank() + 
  theme(panel.background = element_rect(fill = bg, colour = bg), 
        plot.background = element_rect(fill = bg)) + 
  theme(legend.key = element_rect(fill = bg)) 
p
  
