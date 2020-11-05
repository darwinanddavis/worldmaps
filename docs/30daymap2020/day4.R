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

# labels ------------------------------------------------------------------
latlon_data <- with(world.cities, data.frame( # //maps
  "city" = name,"country" = country.etc,"lat" = lat,"lon" = long,"population" = pop)
)
city_labels <- c("San Francisco","Memphis","New Orleans","Saint Louis","Chicago","Atlanta","Asheville","Raleigh","Washington","New York")
city_text <- latlon_data %>% filter(country == "USA" & city %in% city_labels ) %>% select(city,lat,lon)

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
              elevation_scale = 10,
              elevation_function = "sum",
              layer_id = "dest",
              legend = T, update_view = F,
              legend_options = list(title="Destination"),
              auto_highlight = T, highlight_colour = "#FFFFFFFF",
              colour_range = colorspace::sequential_hcl(6,"Peach")) %>%
  add_hexagon(data = od, lat = "lat", lon = "lon", 
              radius = 100,
              elevation = "lat",
              elevation_scale = 10,
              elevation_function = "sum",
              layer_id = "origin",
              legend = T, update_view = F,
              legend_options = list(title="Origin"),
              auto_highlight = T, highlight_colour = "#FFFFFFFF",
              colour_range = colorspace::sequential_hcl(6,"BluGrn")) %>% 
  add_text(data=city_text,lon = "lon", lat = "lat",
           layer_id = "label", text = "city",
           alignment_baseline = "top",anchor = "end",
           fill_colour = "black",
           billboard = T,update_view = F,
           font_family = "Lato Regular",
           size=15
  )
mp4
mp4 %>% saveWidget(here::here("worldmaps","30daymap2020","day4.html"))  


