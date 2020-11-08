# --------------------------------------------------------------------
# 30 day map challenge 2020
# day 8
# author: Matt Malishev
# @darwinanddavis  

# pkgs --------------------------------------------------------------------
pacman::p_load(here,dplyr,rworldmap,leaflet,readr,rgeos,purrr,stringr,ggthemes,showtext,geosphere,htmlwidgets)

# data --------------------------------------------------------------------
honey <- "https://github.com/darwinanddavis/worldmaps/blob/gh-pages/data/day8.Rda?raw=true" %>% url %>% readRDS

# get geocode \ ggmap rworldmaps
lonlat <- getMap(resolution="low") %>% # get country lonlats from rgeos database
  gCentroid(byid=T) %>% 
  as.data.frame 
lonlat$Country <- rownames(lonlat) # add country col
colnames(lonlat) <- c("Lon", "Lat","Country") # rename cols

# get file name  from fontbook.app 
newfonts <- c("A DAY WITHOUT SUN.otf") 
fontlib <- c("adaywithoutsun")

# load font
font_add(fontlib,regular = newfonts,bold = newfonts)
showtext_auto(enable = T) # auto showtext

# filter data 
oo <- honey %>% filter(origin == "Australia") %>% select(-import_val) %>% mutate_at(vars(export_val) ,funs(as.numeric)) %>% arrange(dest) 
oo <- oo %>% na.omit
oo[str_which(oo$dest,"Hong"),"dest"] <- "Hong Kong S.A.R." # match country names 

# get matrix of latlon from rgeos
om <- lonlat[lonlat$Country %in% oo$origin,c("Lon","Lat")] %>% data.matrix(rownames.force = NA) 
dm_ <- lonlat[lonlat$Country %in% oo$dest,c("Lon","Lat")]  
dm_ <- dm_ %>% mutate(dest = dm_ %>% rownames) %>% arrange(dest) # set same order as oo 
dm_labels <- dm_$dest # get labels
dm <- dm_[,1:2] %>% as.matrix # get latlon matrix
oo <- oo %>% filter(dest %in% dm_$dest) # use retrieved latlon data 

# map ---------------------------------------------------------------------
custom_tile <- "Esri.WorldGrayCanvas"
colv <- "#AD9000"
opac <- 0.9
min_zoom <- 3
ttl_img <- "https://github.com/darwinanddavis/worldmaps/blob/gh-pages/img/day8_ttl.png?raw=true"

# options
layer1 <- "2017"
proj_options <- leafletOptions(worldCopyJump = T) 

style <- list(
  "font-weight" = "normal",
  "font-family" = "Avenir",
  "padding" = "5px"
)

# labels 
point_label <- paste(
  "<div style=\"color:",colv,";\">
  <b>",oo$dest %>% str_to_upper,"</b>
  </div>",
  "<b> Export value </b>","<br/>", paste0("$",oo$export_val %>% format(big.mark=",",scientific = F,trim = T))
) %>% map(htmltools::HTML)

# title 
ttl <- paste0("<div style=\"color:",colv,";\">
              <img style='vertical-align:middle' 
              src=",ttl_img," width='60' height='60'>
              <b>MAKING HONEY</b></div>",
             "AUSTRALIA'S HONEY TRADE, 2017<br>",  
             "Total export: $",oo$export_val %>% sum %>% format(big.mark=",",scientific = F,trim = T)) %>% map(htmltools::HTML)

# bl
heading_bl <- paste0("Data source: <a style=color:",colv,"; href=https://legacy.oec.world/en/resources/data/> 
                     BACI International Trade Database </a><br><br>",
                    "Github: <a style=color:",colv,"; href=https://github.com/darwinanddavis/worldmaps> @darwinanddavis </a><br>",
                    "Website: <a style=color:",colv,"; href=https://darwinanddavis.github.io/DataPortfolio/> Matt Malishev </a><br>",
                    "Spot an error? <a style=color:",colv,"; href=https://github.com/darwinanddavis/worldmaps/issues> Submit an issue </a>"
)

# text options
text_label_opt <- labelOptions(noHide = F, direction = "top", textsize = "15px",
                               textOnly = F, opacity = 0.7, offset = c(0,0),
                               style = style, permanent = T)

ttl_opt <- labelOptions(noHide = T, direction = "top", textsize = "50px",sticky = T,
                                     textOnly = T, opacity = 1, offset = c(0,0),
                                    permanent = T, style = list("font-family" = "A DAY WITHOUT SUN", "line-height" = 0.9,"text-align"="right"))

# map ---------------------------------------------------------------------

mp8 <- leaflet() %>% 
  setView(90,0,zoom=min_zoom) %>% 
  setMaxBounds(190,-90,-130,90) %>% 
  addProviderTiles(custom_tile,
                   options = providerTileOptions(minZoom=min_zoom, maxZoom=min_zoom)) %>% 
  # addPolylines(weight = 1,
  #   opacity = 0.3,
  #   color = colv,
  #   label = point_label, labelOptions = text_label_opt) %>%
  addCircleMarkers(data=dm,dm[,1],dm[,2],
                   radius = oo$export_val %>% sqrt / 50,
                   weight=1,
                   color=colv,
                   fillColor=colv,
                   opacity = opac,
                   label = point_label, labelOptions = text_label_opt
  ) %>% 
  addLabelOnlyMarkers(88,-40,
                      label=ttl,
                      labelOptions = ttl_opt) %>%
  addControl(heading_bl,"bottomleft") %>% 
  addScaleBar(position = "bottomright",options = scaleBarOptions(maxWidth = 100))
mp8
mp8 %>% saveWidget(here::here("worldmaps","30daymap2020","day8.html"))  


