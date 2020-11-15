# --------------------------------------------------------------------
# 30 day map challenge 2020
# day 10
# author: Matt Malishev
# @darwinanddavis  

# pkgs --------------------------------------------------------------------
pacman::p_load(ggfortify,dplyr,here,foreign,rgdal,sp,sf,readr,purrr,rvest,xml2,magrittr,ggplot2,stringr,ggthemes,ggnetwork,elevatr,raster,colorspace,ggtext,ggsn,ggspatial,magick,cowplot)


# data --------------------------------------------------------------------
# shp, dbf, shx
# dbf <- here::here("worldmaps","data","day14.dbf") %>% read.dbf(as.is = F)

# read df and shp data 
shp <- here::here("worldmaps","data","day14.shp") %>% readOGR()
d <- shp@data # df vars
bb <- shp@bbox #bbox
sf <- shp@polygons # polygons 
sfp <- sf %>% purrr::map("Polygons")

sfp %>% purrr::map(disaggregate)

sf %>% purrr::map("Polygons") 
sf[[1]] %>% attr(which="Polygons") %>% purrr::map("coords") # works



# map
ggplot() +
  geom_polygon(data=sf[[1]],aes())
