require(readr)
require(leaflet)
require(dplyr)
require(colorspace)
require(here)

url <- "http://data.insideairbnb.com/united-states/ca/san-francisco/2019-12-04/data/listings.csv.gz"
airbnb <- url %>% 
  read_csv
lon <- airbnb$longitude
lat <- airbnb$latitude 
rating <- airbnb$review_scores_rating
hood <- airbnb$neighbourhood_cleansed 

# clean data 
airbnb$review_scores_rating[is.na(airbnb$review_scores_rating)] <- 0

# style
# custom_tile <- "http://b.sm.mapstack.stamen.com/((mapbox-water,$f2f7ff[hsl-color]),(positron,$f2f7ff[hsl-color]),(buildings,$f2f7ff[hsl-color]),(parks,$2c403b[hsl-color]))/{z}/{x}/{y}.png"
custom_tile <- "Esri.WorldGrayCanvas"

colv <- colorRampPalette(diverge_hcl(rating %>% length, "Berlin") %>% sort)
colv_vec <- colv(length(rating))[as.numeric(cut(rating, breaks = length(rating)))]  # define breaks in col gradient
opac <- 0.7

# labels
text_label <- paste(
  "<strong> Type: </strong>",airbnb$property_type,"<br/>",
  "<strong> Rating: </strong>", airbnb$review_scores_rating,"<br/>"
) %>% map(htmltools::HTML)

# fixed heading
fixed_text <- "Spatial snapshot of Airbnb ratings"
fixed_text_latlon <- c(lat[1],lon[1]+0.11) # heading pos
style <- list( # css style 
  "color" = "black",
  "font-size" = "22px",
  "font-weight" = "bold"
)

# heading options 
text_label_opt <- labelOptions(noHide = T, direction = "top", 
                               textOnly = T, offset = c(0,0),
                               style = style
)


href <- paste("Source: <a href=http://insideairbnb.com/get-the-data.html>San Francisco open Airbnb data</a>",
              "<br/> Date: Dec 4, 2019") 

# map
wm <- leaflet() %>% 
  setView(lon[1],lat[1],zoom=12) %>% 
  addTiles() %>% 
  addProviderTiles(custom_tile) %>%
  addCircleMarkers(lon,
                   lat,
                   radius = 6,
                   stroke = T,
                   weight = 1, 
                   color = colv_vec,
                   fillColor = colv_vec,
                   opacity = opac,
                   fillOpacity = opac,
                   label = text_label
  ) %>% 
  addControl(href, position = "topright") %>% 
  addLabelOnlyMarkers(fixed_text_latlon[2], fixed_text_latlon[1], 
                      label = fixed_text, labelOptions = text_label_opt)
wm

# save outputs 
wm %>% saveWidget(here::here("airbnb_sf.html")) 
wm %>% saveWidget(here::here("worldmaps","airbnb_sf.html")) 
