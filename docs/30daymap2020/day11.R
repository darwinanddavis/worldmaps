# --------------------------------------------------------------------
# 30 day map challenge 2020
# day 11
# author: Matt Malishev
# @darwinanddavis  

# pkgs --------------------------------------------------------------------
pacman::p_load(here,mapdeck,dplyr,purrr,readr,showtext,stringr,colorspace,htmltools)

# data --------------------------------------------------------------------
url <- "https://data.melbourne.vic.gov.au/api/views/fp38-wiyy/rows.csv?accessType=DOWNLOAD"
tree <- url %>% read_csv()
tree %>% rename(DBH = `Diameter Breast Height`,
                Expectancy = `Useful Life Expectency Value`,
                Lon = Longitude,
                Lat = Latitude,
                Species = `Common Name`) -> tree
# data 
tree$Expectancy[is.na(tree$Expectancy)] <- 0 # available data 

# get font 
newfonts <- "/Users/malishev/Library/Fonts/BREVE2.ttf"
fontlib <- "breve"
font_add(fontlib,regular = newfonts,bold = newfonts)
showtext_auto(enable = T) # auto showtext


# labels ------------------------------------------------------------------
main <- data.frame("Y"=tree$Lat %>% min + 0.005,"X"=tree$Lon %>% max + 0.0005,
                   "title"= paste0("Vulnerability of\nMelbourne's\nUrban Forest"))

main2 <- data.frame("Y"=tree$Lat %>% min + 0.02,"X"=tree$Lon %>% max + 0.01,
                   "title"= paste0("No. of trees: ", tree %>% nrow() %>% format(big.mark=",",scientific = F,trim = T),"\n",
                                   "Tree species: ", tree %>% pull(Species) %>% str_to_title() %>% unique %>% length,"\n",
                                   "Avg lifespan: ", tree %>% summarise(Expectancy %>% mean) %>% pull %>% plyr::round_any(1)," years"))

title_text <- list(title = 
                     "<strong style=color:#004616;>Vulnerability of Melbourne's<br> Urban Forest</strong> <br/>
                   Tree locations and heights <br> (DBH) looking at life <br> expectancy of species. <br>
                   <span style=color:#004616;>Green </span> = No available data. <br> <br>
                   Author: <a style=color:#004616; href=https://darwinanddavis.github.io/DataPortfolio/> Matt Malishev </a> <br/>
                   Github: <a style=color:#004616; href=https://github.com/darwinanddavis/worldmaps> @darwinanddavis </a> <br/>
                   Data source: <a style=color:#004616; href=https://data.melbourne.vic.gov.au/> City of Melbourne </a> <br/>
                   Spot an error? <a style=color:#004616; href=https://github.com/darwinanddavis/worldmaps/issues> Submit an issue </a> <br/>" ,
                   css = "font-size: 10px; background-color: rgba(255,255,255,0.5);"
)

ttl <- "Life expectancy (years)"  
colv <- paste0(c("#004616",sequential_hcl(5,"YlOrBr")),"B3")

# map ---------------------------------------------------------------------
zoom <- 20
pitch <- 60
bearing <- -30

my_style <- "mapbox://styles/darwinanddavis/ckhe7nocp0euc19oeehfy713s" # style  
mp11 <- mapdeck(
  location = c(tree$Lon %>% median,tree$Lat %>% median),
  zoom = zoom, 
  pitch = pitch, bearing = bearing,
  min_zoom = zoom, max_zoom = zoom,
  min_pitch = pitch, max_pitch = pitch,
  style = my_style
) %>%
  add_grid(data = tree, lat = "Lat", lon = "Lon", 
           elevation = "DBH", elevation_scale = 1,
           elevation_function = "max",
           colour_function = "max",
           colour = "Expectancy",
           layer_id = "tree",
           extruded = T, cell_size = 3,
           update_view = T, focus_layer = T,
           legend = T, #mll,
           legend_options = list(title = ttl),
           colour_range = colv) %>% 
  add_text(data=main,lat = "Y", lon = "X", 
           text = "title", layer_id = "m1",
           alignment_baseline = "top",anchor = "start",
           fill_colour = colv[1], angle = 82,
           billboard = F,update_view = T,
           font_weight = "bold",
           font_family = "Avenir"
           ) %>% 
  add_title(title = title_text, layer_id = "title")
mp11
mp11 %>% htmlwidgets::saveWidget(here::here("worldmaps","30daymap2020","day11.html")) # saved without heading 

