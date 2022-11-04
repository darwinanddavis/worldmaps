##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                            #30daymapchallenge2022                        ----
##                                day 3 - polygons                          ~~~~
##                            author: Matt Malishev                         
##                               @darwinanddavis                            
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

fh <- "day3"


# pcks ----------------------------------------------------------
pacman::p_load(here,ggplot2,lubridate,stringr, dplyr,tibble,readr,palmerpenguins,plotly,htmlwidgets,cowplot,gridExtra,colorspace,readxl,sf,htmltools)

# data ----------------------------------------------------------
df <- here::here("jips_test","jips_data.xlsx") %>% readxl::read_xlsx()
df <- df %>% rename(.cols = 3,
                    "Score" = 3)

colpal <- sequential_hcl(6,"ag_GrnYl")
colv <- colpal[1]
scales::show_col(colpal)

# world data
require(mapdata) # high res data
data(world.cities) # /maps
world_cities <- world.cities 

# get country names  
library(rnaturalearth)
library(rnaturalearthdata)
require(leaflet)  
require(geosphere) # for flight arc paths 

ic <- df$Country %>% unique
mp <- ne_countries(scale = "medium", returnclass = "sf") %>% 
  filter(name %in% ic) %>% 
  rename("Country" = name) %>% 
  left_join(df,by = "Country") %>% 
  mutate("Col" = case_when(
    Score < -3 ~ colpal[1],
    Score < -2 ~ colpal[2],
    Score <= 0 ~ colpal[3],
    Score > 0 ~ colpal[4],
    Score > 2 ~ colpal[5],
    Score > 3 ~ colpal[6],
    T ~ colpal[6]
  ))

pal <- colorNumeric(
  palette = colpal,
  domain = mp$Score
)

# leaflet
setview <- c(0,0)
custom_tile <- "http://d.sm.mapstack.stamen.com/((darkmatter,$00ffff[hsl-color]),(mapbox-water,$00589c[hsl-color]),(parks,$ff9a30[source-in]))/{z}/{x}/{y}.png"
par(bg="black")
opac <- 1
mp_scores <- mp$Score
mp_names <- mp$Country

# style

# text labels 
style <- list(
  "color" = colv,
  "font-weight" = "normal",
  "padding" = "8px"
)
layer_options <- layersControlOptions(collapsed = F)
text_label_opt <- labelOptions(noHide = F, direction = "top", textsize = "15px",
                               textOnly = F, opacity = 0.7, offset = c(0,0),
                               style = style, permanent = T
)

# titles
ttl <- paste0("<div style=\"color:",colv,";\"> 
              Human rights protection scores 
              </div>","by country (1980-2017)")

map_title <- tags$style( 
  HTML(".leaflet-control.map-title { 
       transform: translate(-50%,-20%);
       position: fixed !important;
       left: 50%;
       text-align: center;
       padding-left: 10px; 
       padding-right: 10px; 
       background: white; opacity: 0.5;
       font-size: 25px;
       }"
       ))

title <- tags$div(
  map_title, HTML(ttl)
)  

# bl
heading_bl <- paste0(
  "Data source: <a style=color:",colv,"; href=www.jips.org>JIPS.org</a><br>
  Author: <a style=color:",colv,"; href=https://darwinanddavis.github.io/DataPortfolio/> Matt Malishev </a><br>
  Twitter/Github: <a style=color:",colv,"; href=https://github.com/darwinanddavis/worldmaps/tree/gh-pages> @darwinanddavis </a><br>
  Spot an error? <a style=color:",colv,"; href=https://github.com/darwinanddavis/worldmaps/issues> Submit an issue </a>"
)

# tr
heading_tr <- paste(
  "<strong> Total countries <div style=\"color:",colv,"; font-size:150%\">",mp$Country %>% unique %>% length,"</div> </strong>", "<br/>",
  "<strong> Years evaluated <div style=\"color:",colv,"; font-size:150%\"> 1980-2017 </div> </strong>","<br/>"
)

# easy buttons 
locate_me <- easyButton( # locate user
  icon="fa-crosshairs", title="Zoom to my position",
  onClick=JS("function(btn, map){ map.locate({setView: true}); }"));

reset_zoom <- easyButton( # reset zoom 
  icon="fa-globe", title="Reset zoom",
  onClick=JS("function(btn, map){ map.setZoom(3);}"));  

map_control_box <- htmltools::tags$style( 
  htmltools::HTML(".leaflet-control.layers-base { 
                  text-align: left;
                  padding-left: 10px; 
                  padding-right: 10px; 
                  background: white; opacity: 1;
                  font-size: 20px;
                  }"
       ))

control_box <- htmltools::tags$div(
  map_control_box, htmltools::HTML("")
)  


# layers
mp1 <- mp %>% filter(Year == 1980)
mp2 <- mp %>% filter(Year == 1990)
mp3 <- mp %>% filter(Year == 2000)
mp4 <- mp %>% filter(Year == 2010)
mp5 <- mp %>% filter(Year == 2017)

map <- leaflet() %>% 
  setView(setview[1],setview[2],zoom=2) %>% 
  addTiles(custom_tile) %>% 
  addPolygons(data =  mp1, opacity = opac,color = ~Col,fillColor = ~Col,stroke = TRUE,weight = 0.5, 
              # popup=paste0("<br>",mp1$Score,"<br>"),
              popup = paste0("<strong>",mp1$Country,"</strong><br/><br/>","<strong> Protection score </strong><br/><span style=color:",mp$Col,";>", mp1$Score,"</span><br/>") %>% purrr::map(htmltools::HTML),
              label=paste(mp1$Country), 
              labelOptions = text_label_opt, popupOptions = text_label_opt,
              group = "1980"
  ) %>%  
  addPolygons(data =  mp2, opacity = opac,color = ~Col,fillColor = ~Col,stroke = TRUE,weight = 0.5, 
              popup = paste0("<strong>",mp2$Country,"</strong><br/><br/>","<strong> Protection score </strong><br/><span style=color:",mp$Col,";>", mp2$Score,"</span><br/>") %>% purrr::map(htmltools::HTML),
              label=paste(mp2$Country), 
              labelOptions = text_label_opt, popupOptions = text_label_opt,
              group = "1990"
  ) %>%  
  addPolygons(data =  mp3,opacity = opac,color = ~Col,fillColor = ~Col,stroke = TRUE,weight = 0.5, 
              popup = paste0("<strong>",mp3$Country,"</strong><br/><br/>","<strong> Protection score </strong><br/><span style=color:",mp$Col,";>", mp3$Score,"</span><br/>") %>% purrr::map(htmltools::HTML),
              label=paste(mp3$Country), 
              labelOptions = text_label_opt, popupOptions = text_label_opt,
              group = "2000"
  ) %>%  
  addPolygons(data =  mp4,opacity = opac,color = ~Col,fillColor = ~Col,stroke = TRUE,weight = 0.5, 
              popup = paste0("<strong>",mp4$Country,"</strong><br/><br/>","<strong> Protection score </strong><br/><span style=color:",mp$Col,";>", mp4$Score,"</span><br/>") %>% purrr::map(htmltools::HTML),
              label=paste(mp4$Country), 
              labelOptions = text_label_opt, popupOptions = text_label_opt,
              group = "2010"
  ) %>%  
  addPolygons(data =  mp5,opacity = opac,color = ~Col,fillColor = ~Col,stroke = TRUE,weight = 0.5, 
              popup = paste0("<strong>",mp5$Country,"</strong><br/><br/>","<strong> Protection score </strong><br/><span style=color:",mp$Col,";>", mp5$Score,"</span><br/>") %>% purrr::map(htmltools::HTML),
              label=paste(mp5$Country), 
              labelOptions = text_label_opt, popupOptions = text_label_opt,
              group = "2017"
  ) %>%  
  addProviderTiles(
    "CartoDB.DarkMatter"
    # "Stamen.TonerLite"
  ) %>% 
  addLegend(pal = pal,
            values  = mp$Score,
            position = "bottomright",
            title = "Protection score",
            opacity = opac) %>% 
  addLayersControl(
    baseGroups = c("1980","1990","2000","2010","2017"),
    options = layer_options) %>% 
  addControl(title, "bottomleft", className = "map-title") %>% 
  addControl(heading_bl,"bottomleft") %>%
  addControl(heading_tr, "topright") %>% 
  addControl(control_box, "topright", className = "layers-base") %>% 
  addEasyButton(reset_zoom) %>% 
  addEasyButton(locate_me) 
map
