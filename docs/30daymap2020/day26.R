



hq <- c(52.36983172089777, 52.36683670344817)
b <- c(4.887634615681384, 4.9264369554569285)
tibble("lat" = hq, "lon" = b) %>% 
  readr::write_csv(here::here("worldmaps","data","day26.csv"))

# Datasets > upload latlon data (csv, geojson) 
# save > export to tileset
# styles > select style > layers > search for dataset / upload data (kml, geojson)
# convert to line/point/polygon/heatmap 

### framer 
# new draft
# frame > iphone 11
# insert > mapbox/feather icons/iphone 11 kit
# drag mapbox SequentialLocationMap and Marker components + iphone homebar and status bar into frame 
# insert mapbox access token, then map style > custom > insert mb style link (need to Preview to view updates)
# select code (lhs) > create code file > new override 
# go to https://packages.framer.com/package/mapbox/mapbox > Using the Sequential Location Map component > copy code snippet > paste in the framer .tsx file  (details found in insert > mapbox > package details > scroll to sequentiallocationmap )
# overrrides (rhs) > select file > select cameramovesequence override > preview   
# edit latlon, pitch, bearing, zoom with locations from mb dataset 

## graphics mode / custom icons 
# insert > custom icon > double click graphic layer to enter drawing mode   
# insert > new button 
