##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                            #30daymapchallenge2021                        ----
##                                day 4 - hexagons                           ~~~~
##                            author: Matt Malishev                         
##                               @darwinanddavis                            
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# source --------------------------------------------------------
citation <- "https://data.melbourne.vic.gov.au/"

# pkgs -----------------------------------------------------
require(pacman)
p_load(mapdeck,readr,ggmap,dplyr,sf,sfheaders,data.table,tigris,sp,maps,colorspace)

# api keys ---------------------------------------------------------------------
set_token("<mapbox_key>") # mapbox key

# load data ------------------------------------------------------------------
df <- here::here("data","30daymap2021","melb_restaurants.Rda") %>% 
  readRDS() #%>% 
# filter(year == 2012:2020)

latlon_data <- with(world.cities, data.frame( # //maps
  "city" = name,"country" = country.etc,"lat" = lat,"lon" = long,"population" = pop)
)
city_text <- latlon_data %>% filter(country == "Australia" & city %in% "Melbourne") %>% select(city,lat,lon)


# style ------------------------------------------------------------------
my_style <- "mapbox://styles/darwinanddavis/ckhe7nocp0euc19oeehfy713s" # style  
my_style_public <- "https://api.mapbox.com/styles/v1/darwinanddavis/ckhe7nocp0euc19oeehfy713s.html?fresh=true&title=copy&access_token="
ttl <- "Seating capacity per area"  
main <- data.frame("Y" = df$lat %>% na.omit() %>% min + 0.05,"X"=df$lon %>% na.omit %>% max - 0.003,
                   "title"= paste0("Where do\n            Melburnians eat?"))

main2 <- data.frame("Y" = df$lat %>% na.omit() %>% min + 0.05,"X"=df$lon %>% na.omit %>% max + 0.006,
                    "title"= paste0("20 years of\n            urban foraging"))


title_text <- list(title = 
                     paste0("<strong style=color:#004616;>Where do Melburnians eat?</strong> <br/>
                            Restaurant/cafe capacity per area <br>
                            Author: <a style=color:",colvl,"; href=https://darwinanddavis.github.io/DataPortfolio/> Matt Malishev </a> <br/>
                            Github: <a style=color:",colvl,"; href=https://github.com/darwinanddavis/worldmaps/tree/gh-pages> @darwinanddavis </a> <br/>
                            Data source: <a style=color:",colvl,"; href=",citation,"> City of Melbourne </a> <br/>
                            Map style: <a style=color:",colvl,"; href=", my_style_public,"MAPBOX_ACCESS_TOKEN> Mapbox </a> <br/>
                            Spot an error? <a style=color:",colvl,"; href=https://github.com/darwinanddavis/worldmaps/issues> Submit an issue </a> <br/>"),
                   css = "font-size: 10px; background-color: rgba(255,255,255,0.5);"
                     )

# map ---------------------------------------------------------------------
my_style <- "mapbox://styles/darwinanddavis/ckh4kmfdn0u6z19otyhapiui3" # style  
colv <- paste0(sequential_hcl(6,"Purple-Yellow"),"B3")
colvl <- colv[1] # link col
zoom <- 12
pitch <- 0
bearing <- -30

mapdeck(
  location = c(df$lon[1],df$lat[1]), 
  zoom = zoom,
  pitch = pitch, bearing = bearing,
  # min_zoom = zoom, max_zoom = zoom,
  # min_pitch = pitch, max_pitch = pitch,
  style = my_style
) %>%
  add_hexagon(data = df, lat = "lat", lon = "lon", 
              radius = 30,
              digits = 20,
              elevation = "number",
              elevation_scale = 5,
              colour_function = "mean",
              elevation_function = "mean",
              colour = "number",
              layer_id = "number",
              update_view = F,
              # focus_layer = T,
              legend = T,
              legend_options = list(title="Mean seating capacity\n per area"),
              auto_highlight = T, highlight_colour = "#FFFFFFFF",
              colour_range = colv) %>%
  add_text(data=main,lat = "Y", lon = "X", 
           text = "title", layer_id = "m1",
           alignment_baseline = "top",anchor = "start",
           fill_colour = colv[1], angle = 82,
           billboard = F,update_view = F,
           font_weight = "bold",
           font_family = "Avenir" 
  ) %>% 
  add_text(data=main2,lat = "Y", lon = "X", 
           text = "title", layer_id = "m2",
           alignment_baseline = "top",anchor = "start",
           fill_colour = colv[1], angle = 82,
           billboard = F,update_view = F,
           font_family = "Avenir" 
  ) %>% 
  add_title(title = title_text, layer_id = "heading")

# save --------------------------------------------------------------------
mp4 %>% htmlwidgets::saveWidget(here::here("worldmaps","30daymap2021","day4.html")) # saved without heading 
