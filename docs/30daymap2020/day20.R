# --------------------------------------------------------------------
# 30 day map challenge 2020
# day 20
# author: Matt Malishev
# @darwinanddavis  

# pkgs --------------------------------------------------------------------
pacman::p_load(dplyr,here,mapdeck,rgdal,sp,sf,raster,colorspace,mapdata,ggmap,jpeg)

# data --------------------------------------------------------------------
# read df and shp data 
tt <- here::here("worldmaps","data","day20.tif") %>% raster()
dl <- here::here("worldmaps","data","day20_distribution_limit.shp") %>% st_read()
dl <- dl %>% st_boundary() # get boundary

aus <- ggplot() +
    geom_polygon(data=map_data("world",region="Australia"),aes(long,lat,group=group),fill="#E8A154", colour="#E8A154") +
  geom_raster(data=tt_,aes(x,y,fill=layer),show.legend = T) +
  scale_fill_gradientn(name = "Camels!",
                       colors = colv) +
  theme_minimal()
# ggsave(here::here("worldmaps","img","day20_aus.jpg"),aus,device = "jpeg",width=10,height=10,units="cm")
  
# set prj
prj <- "+init=epsg:4326"
tt@crs@projargs <- prj # raster
dl <- dl %>% st_transform(prj) # shp

# transform tiff data
bb <- tt@extent %>% st_bbox() %>% unlist
tt_ <- tt %>%  rasterToPoints() %>% # convert to df
  as.data.frame  
names(tt_) <- c("x","y","layer")

# style -------------------------------------------------------------------

my_style <- "mapbox://styles/darwinanddavis/ckhq8z67b0t0g19paaaeqmula" # style  
my_style_public <- "https://api.mapbox.com/styles/v1/darwinanddavis/ckhq8z67b0t0g19paaaeqmula.html?fresh=true&title=copy&access_token="
ttl <- "Camels"  

# colv
colv <- sequential_hcl(6,"Red-Blue")
colvl <- colv[1] # link col
family <- "Copperplate"

main <- data.frame("Y"=tt_$y %>% min - 5,"X"=tt_$x %>% median - 2,
                   "size" = 32,
                   "title"= paste0("The Great\nFeral Camel Crater\n of Australia"))

main2 <- data.frame("Y"=main$Y - 6,"X"=main$X,
                    "size" = main$size - 8,
                    "title"= paste0("___________________ \n",
                      "Camels: 1,000,000 + \n  ",
                      "Area: 3,300,000 sq km \n  ",
                      "Density: ~2 per sq km²"
                    ))

title_text <- list(title = 
                     paste0("<strong style=color:",colvl,";>The Great Feral Camel Crater\n of Australia</strong><br/>
                            Author: <a style=color:",colvl,"; href=https://darwinanddavis.github.io/DataPortfolio/> Matt Malishev </a> <br/>
                            Github: <a style=color:",colvl,"; href=https://github.com/darwinanddavis/worldmaps/tree/gh-pages> @darwinanddavis </a> <br/>
                            Data source: <a style=color:",colvl,"; href=https://data.gov.au/data/dataset/9e807c7f-bc64-47ea-a1f2-87a4609ea69c/> NT Dept. Env. & Natural Resources </a> <br/>
                            Map style: <a style=color:",colvl,"; href=", my_style_public,"MAPBOX_ACCESS_TOKEN> Mapbox </a> <br/>
                            Spot an error? <a style=color:",colvl,"; href=https://github.com/darwinanddavis/worldmaps/issues> Submit an issue </a> <br/>"),
                   css = "font-size: 11px; background-color: rgba(255,255,255,0.5);"
                     )

# map vars 
zoom <- 21
pitch <- 30
bearing <- 40
opac <- 1
cs <- 5 

mp20 <- mapdeck(
  location = c(tt_$x %>% max,tt_$y %>% mean),
  zoom = zoom,
  pitch = pitch, bearing = bearing,
  min_zoom = zoom, max_zoom = zoom,
  min_pitch = pitch, max_pitch = pitch,
  style = my_style
) %>% 
  add_sf(data = dl,
         stroke_colour = colv[1],
         stroke_width = 0.1,
         stroke_opacity = opac,
         layer_id = "limit",
         legend = F) %>% 
add_screengrid(data = tt_,
               lon = "x",
               lat = "y",
               weight = "layer",
               aggregation = "sum",
               cell_size = cs,
               opacity = opac,
               colour_range = colv,
               layer_id = "camel",
               focus_layer = T,
               update_view = T
) %>%
  add_text(data=main,lat = "Y", lon = "X",
           text = "title", layer_id = "m1",
           alignment_baseline = "top",anchor = "start",
           fill_colour = colv[1],
           size = "size",
           billboard = F,update_view = F,
           font_weight = "bold",
           font_family = family
  ) %>%
  add_text(data=main2,lat = "Y", lon = "X",
           text = "title", layer_id = "m2",
           alignment_baseline = "top",anchor = "start",
           fill_colour = colv[1],
           size = "size", 
           billboard = F,update_view = F,
           font_family = family
  ) %>%
  add_title(title = title_text,layer_id = "heading")
mp20
mp20 %>% htmlwidgets::saveWidget(here::here("worldmaps","30daymap2020","day20.html"))

