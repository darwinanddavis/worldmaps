# --------------------------------------------------------------------
# 30 day map challenge 2020
# day 10
# author: Matt Malishev
# @darwinanddavis  

# pkgs --------------------------------------------------------------------
pacman::p_load(dplyr,readr,rvest,xml2,magrittr,sp,sf,rgdal,ggmap,ggplot2,stringr,ggthemes,ggnetwork,colorspace,ggtext,ggsn,ggspatial,showtext,here)

# data --------------------------------------------------------------------
url <- "/Users/malishev/Downloads/2018_Building_Footprints.kml"
melb <- url %>% sf::st_read()
bb <- melb$geometry %>% attr(which = "bbox") # get bbox for clipping plot

# get geo elements 
methods(class = "sf")
bb <- melb$geometry %>% attr(which = "bbox") # bbox
ll <- melb$geometry %>% st_coordinates() %>% .[1,c("X","Y")] # first latlon


newfonts <- "/Users/malishev/Library/Fonts/BREVE2.ttf"
fontlib <- "breve"
font_add(fontlib,regular = newfonts,bold = newfonts)
showtext_auto(enable = T) # auto showtext

# map ---------------------------------------------------------------------

colv <- "#FF637D"
bg <- "#FFB2BF"
ttl <- data.frame("main" = "**MELBOURNE  \  AUS**",
                  "x" = bb[1] - 0.005,
                  "y" = bb[2] + 0.01,
                  "size" = 300,
                  "family" = fontlib,
                  "angle" = 90)
ttl_coord <- data.frame("main" = "**37° 48' 49'' S <br> 144° 57' 47'' E**",
                        "x" = bb[3] + 0.005,
                        "y" = bb[4] - 0.01,
                        "size" = 120,
                        "family" = fontlib,
                        "angle" = 270)

# map
quartz() # load plot window first (polygon edge error)
p <- ggplot() + 
  geom_sf(data=melb,colour = colv, fill = NA,size=0.07) +
  theme_blank() +
  theme(
    panel.background = element_rect(fill = bg), 
    plot.background = element_rect(fill = bg, color = colv), 
    legend.background = element_rect(fill = bg),
    legend.box.background = element_rect(fill = bg)
  ) + 
  theme(plot.margin=unit(rep(1.1,4),"cm")) +
  geom_richtext(data=ttl,aes(x,y,label=main, # add title
                             family = family, size = size,
                             angle = angle), show.legend = F,
                color=colv, fill = NA, label.color = NA,alpha = 1) +
  geom_richtext(data=ttl_coord,aes(x,y,label=main, # add title
                                   family = family, size = size,
                                   angle = angle), show.legend = F,
                color=colv, fill = NA, label.color = NA, alpha = 1) +
  geom_richtext(data=credits,aes(x,y,label=main, # add title
                                   family = family, size = size,
                                   angle = angle), show.legend = F,
                color=colv, fill = NA, label.color = NA, alpha = 1) 
p  

# save --------------------------------------------------------------------
p %>% ggsave(here::here("worldmaps","img","day10.png"),width=15,height=15,units = "cm", dpi="screen")



