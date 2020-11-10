# --------------------------------------------------------------------
# 30 day map challenge 2020
# day 10
# author: Matt Malishev
# @darwinanddavis  

# https://nceas.github.io/oss-lessons/spatial-data-gis-law/3-mon-intro-gis-in-r.html

# pkgs --------------------------------------------------------------------
pacman::p_load(dplyr,readr,rvest,xml2,magrittr,sp,ggplot2,stringr,ggthemes,ggnetwork,colorspace,ggtext,ggsn,ggspatial)


# data --------------------------------------------------------------------
melb <- url %>% read_html %>% 
# read in shp 
readOGR(".shp")

# map ---------------------------------------------------------------------

### ideas 
# group by construction year comparing 1901 to 2019  
# group by number of floors above ground  
# compare bike spaces over time  

ggplot() +
  geom_path() +
  coord_fixed() +
  labs(title=,
       subtitle=,
       x=,
       y=)

