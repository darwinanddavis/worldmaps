# --------------------------------------------------------------------
# 30 day map challenge 2020
# day 1
# author: Matt Malishev
# @darwinanddavis  

# pcks --------------------------------------------------------------------
require(pacman)
p_load(mapdeck,readr,purrr,stringr,dplyr,tibble,htmltools,sf,sfheaders,data.table,stringr,tigris,sp,here,htmlwidgets)

# key_m <- readr::read_lines("<mapdeck key>")
# set_token(key_m) # mapbox key
# read in current data
id_df <- "https://raw.githubusercontent.com/darwinanddavis/worldmaps/gh-pages/data/id_df.csv" %>% read_csv

# read in latlon data -----------------------------------------------------

fh <- "coffee"
m <- paste0("https://github.com/darwinanddavis/worldmaps/blob/gh-pages/data/",fh,".Rda?raw=true") %>% url %>% readRDS
m$name <- m$name %>% paste0("\n \n \n") # add linebreaks to names
m_ttl <- tibble(lon=-43,lat=31, # add title
                name=paste0(fh %>% stringr::str_to_upper(),"\nSNACKMAP")
)
m %>% names

# labels ------------------------------------------------------------------
family <- "Chalkboard"
colv <- "#E1B69B"

style <- list( # css label style 
  "font-weight" = "normal",
  "padding" = "8px",
  "color" = colv
)

point_label <- paste0( # add label tooltip  
  "<div style=\"color:",style$color,"; padding:",style$padding,"; font-family:",family,";\">
  <b>",m$name,"<b><br><br>
  <b>Address: <b><br>", m$address %>% str_to_upper(),"<br><br> 
  <b>Type: <b><br>", m$type %>% str_to_upper() %>% str_replace_all("_"," "), 
  "</div>") %>% map(htmltools::HTML)

m$label <- point_label # add css text tooltip to df

# map ---------------------------------------------------------------------
mp <- mapdeck(data=m,
        location = c(m$lon[1],m$lat[1]), 
        zoom = 2,
        pitch =  0,
        style = id_df %>% filter(Name == fh) %>% pull(Style)
) %>%
  add_pointcloud(lon = "lon",lat = "lat", 
                 layer_id = "latlon",id = "latlon",
                 fill_colour = id_df %>% filter(Name == fh) %>% pull(Col),
                 auto_highlight = T, 
                 highlight_colour = paste0(colv,"00"),
                 elevation = 0,
                 radius = 10, 
                 update_view = F,
                 tooltip = "label"
  ) %>% 
  mapdeck::add_text(lon = "lon", lat = "lat", # location names 
                    layer_id = "label", text = "name",
                    alignment_baseline = "top",anchor = "end",
                    fill_colour = id_df %>% filter(Name == fh) %>% pull(Col),
                    billboard = T,update_view = F,
                    font_family = family,
                    size=15
  ) %>% 
  mapdeck::add_text(m_ttl,lon = "lon", lat = "lat", # title
                    layer_id = "title", text = "name",
                    alignment_baseline = "top",anchor = "end",
                    fill_colour = id_df %>% filter(Name == fh) %>% pull(Col),
                    billboard = F, update_view = F,
                    font_family = family, font_weight = "bold",
                    size=35
  )
mp
mp %>% htmlwidgets::saveWidget(here::here("worldmaps","30daymap2020","day1.html"))  

