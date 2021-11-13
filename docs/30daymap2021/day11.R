##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                            #30daymapchallenge2021                        ----
##                              day 12 - population                                 ~~~~
##                            author: Matt Malishev                         
##                               @darwinanddavis                            
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
citation <- "ATSB"

# pcks -----------------------------------------------------
pacman::p_load(mapdeck,ggmap,dplyr,sf,sfheaders,rnaturalearth,rnaturalearthdata,maptools,scales,rgdal,colorspace,mapdata,ggsn,mapview,mapproj,ggthemes,grid,ggtext,rworldxtra,stringr,ggtext,showtext)

# data ----------------------------------------------------------
fh <- "day11"
df <- here::here("data","30daymap2021",paste0(fh,".xlsx")) %>% 
  readxl::read_xlsx(trim_ws = T) %>% 
  rename("lat" = 12,
         "lon" = 13,
         "birds_struck" = 29,
         "bird_size" = 37,
         "bird_mass" = 38) %>% 
  mutate_at(c("Altitude","bird_mass"), as.numeric) 

# style ------------------------------------------------------------------
my_style <- "mapbox://styles/darwinanddavis/ckvxevehj5sui15qp3l80qfll" # style  
my_style_public <- "https://api.mapbox.com/styles/v1/darwinanddavis/ckvxevehj5sui15qp3l80qfll.html?title=view&access_token="
colv <- paste0(sequential_hcl(6,"Red-Purple"))
colvl <- colv[1] # link col

ttl <- "Bird mass (kg)"  
main <- data.frame("Y" = df$lat %>% na.omit() %>% min + 18,
                   "X" = df$lon %>% na.omit() %>% max - 50,
                   "title"= paste0("Avian Airstrike"))
main2 <- data.frame("Y" = main$Y - 2.2,"X"=main$X,
                    "title"= paste0("Aircraft-bird strikes across Australia\n2008-2017"))
main3 <- data.frame("Y" = main2$Y - 3,"X"=main2$X,
                    "title"= paste0("Total collisions: ", df %>% nrow %>% format(big.mark=",",scientific = F,trim = T),
                                    "\nNo. unique species: ", df$SpeciesFamily %>% unique %>% length,
                                    "\n\n\n Year of most collisions: 2012"
                                    ))
  
title_text <- list(title = 
                     paste0("<strong style=color:",colvl,";>Avian airstrikes</strong> <br/>
                            Aircraft-bird collision occurrence <br>
                            Author: <a style=color:",colvl,"; href=https://darwinanddavis.github.io/DataPortfolio/> Matt Malishev </a> <br/>
                            Github: <a style=color:",colvl,"; href=https://github.com/darwinanddavis/worldmaps/tree/gh-pages> @darwinanddavis </a> <br/>
                            Data source: <a style=color:",colvl,"; href=",citation,"> atsb.gov.au </a> <br/>
                            Map style: <a style=color:",colvl,"; href=", my_style_public,"MAPBOX_ACCESS_TOKEN> Mapbox </a> <br/>
                            Spot an error? <a style=color:",colvl,"; href=https://github.com/darwinanddavis/worldmaps/issues> Submit an issue </a> <br/>"),
                   css = "font-size: 10px; background-color: rgba(255,255,255,0.5);"
                     )

# map ---------------------------------------------------------------------
zoom <- 12
pitch <- 0
bearing <- -30
family <- "Geneva"

mapdeck(
  location = c(df$lon[1],df$lat[1]), 
  zoom = zoom,
  pitch = pitch, bearing = bearing,
  # min_zoom = zoom, max_zoom = zoom,
  # min_pitch = pitch, max_pitch = pitch,
  style = my_style
  # colourvalues::colour_palettes()
) %>%
  add_heatmap(data = df, lat = "lat", lon = "lon",
    weight = "bird_mass",threshold = 0.2,intensity = 5,
    layer_id = "bird_mass", colour_range = colv) %>%
  add_text(data=main,lat = "Y", lon = "X", 
           text = "title", layer_id = "m1",
           size = 40,
           alignment_baseline = "top",anchor = "start",
           fill_colour = colv[1], angle = 0,
           billboard = F,update_view = F,
           font_weight = "bold",
           font_family = family
  ) %>% 
  add_text(data=main2,lat = "Y", lon = "X", 
           text = "title", layer_id = "m2",
           size = 25,
           alignment_baseline = "top",anchor = "start",
           fill_colour = colv[1], angle = 0,
           billboard = F,update_view = F,
           font_family = family
  ) %>% 
  add_text(data=main3,lat = "Y", lon = "X", 
           text = "title", layer_id = "m3",
           size = 15,
           alignment_baseline = "top",anchor = "start",
           fill_colour = colv[1], angle = 0,
           billboard = F,update_view = F,
           font_family = family
  ) %>% 
   add_title(title = title_text, layer_id = "heading") %>% 
htmlwidgets::saveWidget(here::here("30daymap2021",paste0(fh,".html")))
