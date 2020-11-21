# https://github.com/hrbrmstr/nominatim
require(devtools)
devtools::install_github("hrbrmstr/nominatim")
library(nominatim)
packageVersion("nominatim")
key <- readr::read_lines("/Users/malishev/Documents/Data/lyft/mqa.txt")

# general search term  
peru <- osm_search("Port Louis",key=key,
                  # country_codes = c("pe")
                  # bounded = T, 
                  # address_details = T,
                  # limit=100
                  )
peru

# https://eriqande.github.io/rep-res-web/lectures/making-maps-with-R.html 

require(leaflet)
leaflet() %>% addTiles() %>% addMarkers(lat=bbw$lat,lng=bbw$lon)
