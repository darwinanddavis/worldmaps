# --------------------------------------------------------------------
# 30 day map challenge 2020
# day 4
# author: Matt Malishev
# @darwinanddavis  

# pkgs --------------------------------------------------------------------
pacman::p_load(here,mapdeck,dplyr,purrr,readr)

# read data ---------------------------------------------------------------
url <- "https://data.cityofnewyork.us/api/views/vfnx-vebw/rows.csv?accessType=DOWNLOAD"
red <- url %>% read_csv %>% filter(`Primary Fur Color` == "Cinnamon")

# labels ------------------------------------------------------------------
main <- data.frame("Y"=40.775546,"X"=-73.960502,
                   "title"= paste("Manhattan's cinnamon squirrels\n","Number of squirrels: ", red %>% nrow()))
data_source <- data.frame("Y"=40.773011,"X"=-73.956465,
                          "title"= paste("Data source: NYC Open Data - Squirrel Census"))
title_text <- list(title = 
                     "<strong>NYC squirrel census</strong> <br/>
                   Author: <a href=https://darwinanddavis.github.io/DataPortfolio/> Matt Malishev </a> <br/>
                   Github: <a href=https://github.com/darwinanddavis/worldmaps> @darwinanddavis </a> <br/>
                   Data source: <a href=https://data.cityofnewyork.us/Environment/2018-Central-Park-Squirrel-Census-Squirrel-Data/vfnx-vebw> 2018 Central Park Squirrel Census </a> <br/>
                   Spot an error? <a href=https://github.com/darwinanddavis/worldmaps/issues> Submit an issue </a> <br/>" ,
                   css = "font-size: 11px; background-color: rgba(255,255,255,0.5);"
)


# map ---------------------------------------------------------------------
zoom <- 13.5
pitch <- 0
bearing <- 280

my_style <- "mapbox://styles/darwinanddavis/ckh68ceou0j2119p3jys0yzud" # style  
mp5 <- mapdeck(
  location = c(red$X %>% mean,red$Y %>% mean), 
  zoom = zoom,
  pitch =  pitch, bearing = bearing,
  min_zoom = zoom, max_zoom = zoom,
  min_pitch = pitch, max_pitch = pitch,
  style = my_style
) %>%
  add_grid(data = red, lat = "Y", lon = "X", 
           cell_size = 25, extruded = F,
           elevation = 2,elevation_scale = 1,
           layer_id = "red", update_view = F,
           legend = T, legend_options = list(title="Cinnamon squirrels"), 
           colour_range = colorspace::sequential_hcl(6,"Reds")) %>% 
  add_text(data=main,lat = "Y", lon = "X", 
           layer_id = "label", text = "title",
           alignment_baseline = "bottom",anchor = "end",
           fill_colour = "#6D0026", angle = 61,
           billboard = F,update_view = F,
           font_family = "BREVE SC",
           size=35
  ) %>% 
  add_text(data=data_source,lat = "Y", lon = "X", 
           layer_id = "datasource", text = "title",
           alignment_baseline = "bottom",anchor = "end",
           fill_colour = "#6D0026", angle = 61,
           billboard = F,update_view = F,
           font_family = "BREVE SC",
           size=20
  ) %>% 
  add_title(title = title_text, layer_id = "title")
mp5
mp5 %>% saveWidget(here::here("worldmaps","30daymap2020","day5.html"))  




