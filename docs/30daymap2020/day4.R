# --------------------------------------------------------------------
# 30 day map challenge 2020
# day 4
# author: Matt Malishev
# @darwinanddavis  

# pkgs -----------------------------------------------------
require(pacman)
p_load(mapdeck,readr,ggmap,dplyr,sf,sfheaders,data.table,tigris,sp,maps,colorspace)

# api keys ---------------------------------------------------------------------
# set_token"(<mapbox_key>") # mapbox key

# read data --------------------------------------------------------
od <- paste0("https://github.com/darwinanddavis/worldmaps/blob/gh-pages/data/day4_od.Rda?raw=true") %>% url %>% readRDS
dd <- paste0("https://github.com/darwinanddavis/worldmaps/blob/gh-pages/data/day4_dd.Rda?raw=true") %>% url %>% readRDS
od[,c("lat","lon")] <- od[,c("lat","lon")] %>% round(3)
dd[,c("lat","lon")] <- dd[,c("lat","lon")] %>% round(3)

# labels ------------------------------------------------------------------
latlon_data <- with(world.cities, data.frame( # //maps
  "city" = name,"country" = country.etc,"lat" = lat,"lon" = long,"population" = pop)
)
city_labels <- c("San Francisco","Memphis","New Orleans","Saint Louis","Chicago","Atlanta","Asheville","Raleigh","Washington","New York")
city_text <- latlon_data %>% filter(country == "USA" & city %in% city_labels ) %>% select(city,lat,lon)
title_text <- list(title = 
              "<strong>Mapping Lyft ride data</strong> <br/>
              Author: <a href=https://darwinanddavis.github.io/DataPortfolio/> Matt Malishev </a> <br/>
               Github: <a href=https://github.com/darwinanddavis/worldmaps> @darwinanddavis </a> <br/>
               Spot an error? <a href=https://github.com/darwinanddavis/worldmaps/issues> Submit an issue </a> <br/>" ,
              css = "font-size: 8px; background-color: rgba(255,255,255,0.5);"
)


# map ---------------------------------------------------------------------
my_style <- "mapbox://styles/darwinanddavis/ckh4kmfdn0u6z19otyhapiui3" # style  
mp4 <- mapdeck(
  location = c(od$lon[1],od$lat[1]), 
  zoom = 10,
  pitch =  30,
  style = my_style
) %>%
  add_hexagon(data = dd, lat = "lat", lon = "lon", 
              radius = 100,
              elevation = "lat",
              elevation_scale = 15,
              elevation_function = "sum",
              layer_id = "dest",
              legend = T, update_view = F,
              legend_options = list(title="Destination"),
              auto_highlight = T, highlight_colour = "#FFFFFFFF",
              colour_range = colorspace::sequential_hcl(6,"OrYel")) %>%
  add_hexagon(data = od, lat = "lat", lon = "lon", 
              radius = 100,
              elevation = "lat",
              elevation_scale = 5,
              elevation_function = "sum",
              layer_id = "origin",
              legend = T, update_view = F,
              legend_options = list(title="Origin"),
              auto_highlight = T, highlight_colour = "#FFFFFFFF",
              colour_range = colorspace::sequential_hcl(6,"Purp")) %>% 
  add_text(data=city_text,lon = "lon", lat = "lat",
           layer_id = "label", text = "city",
           alignment_baseline = "top",anchor = "end",
           fill_colour = "black",
           billboard = T,update_view = F,
           font_family = "Lato Regular",
           size=15
  ) %>% 
  add_title(title = title_text, layer_id = "title")
mp4
mp4 %>% saveWidget(here::here("worldmaps","30daymap2020","day4.html"))  


