##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                            #30daymapchallenge2023                        ----
##                                day 15 - OSM                              ~~~~
##                            author: Matt Malishev                         
##                               @darwinanddavis                            
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# pcks --------------------------------------------------------------------
pacman::p_load(dplyr,readr,sf,stringr,mapdeck)
sf_use_s2(FALSE)

# data --------------------------------------------------------------------

# define read data funcs
get_data <- function(yr){
    here::here("data",paste0("development-activity-monitor_",yr,".csv")) %>% read_csv %>% 
      st_as_sf(coords = c("longitude","latitude"), crs = 4326)
}

get_data_shp <- function(yr){
  here::here(paste0("development-activity-monitor",yr),"development-activity-monitor.shp") %>% st_read %>% 
    mutate("year" = yr)
}

# read in data
d_all <- rbind(get_data(2020),get_data(2021),get_data(2022) %>% filter(status == "COMPLETED"))
sf_all <- rbind(get_data_shp(2020),get_data_shp(2021),get_data_shp(2022) %>% filter(status == "COMPLETED"))

# read in base building footprint
sf_melb <- here::here("data","Building_Footprint.shp") %>% 
  st_read() %>% 
  st_transform(4326) %>% st_make_valid() 

# separate years 
sf_final_ <- rbind(
  sf_melb %>% st_filter(sf_2020,.predicate = st_intersects) %>% 
    mutate("year" = 2020),
  sf_melb %>% st_filter(sf_2021,.predicate = st_intersects) %>% 
    mutate("year" = 2021),
  sf_melb %>% st_filter(sf_2022,.predicate = st_intersects) %>% 
    mutate("year" = 2022)
)
  

# get final buildings addresses
addy <- c(
  "68-82 Southbank Boulevard SOUTHBANK VIC 3006"
  ,"76-84 Collins Street MELBOURNE VIC 3000"
  ,"471-485 Collins Street MELBOURNE VIC 3000"
  ,"433-455 Collins Street MELBOURNE VIC 3000"
  ,"93-129 Therry Street, Melbourne, 3000"
  ,"134-160 Spencer Street MELBOURNE VIC 3000"
  ,"93-119 Kavanagh Street SOUTHBANK VIC 3006"
  ,"Wesley Uniting Church 118-148 Lonsdale Street MELBOURNE VIC 3000"
)


# maually get missing locations
p1 <- c(144.964424,-37.800008) %>% st_point() # 700 Swanston Street, Carlton, VIC 3053
pts1 <- sf_melb_future %>% st_filter(p1,.predicate = st_intersects)

ids <- c(
  sf_all %>% filter(street_addr %in% addy) %>% pull(property_id) %>% as.numeric(),
  pts1 %>% pull(property_i) %>% as.numeric(), # 700 Swanston Street, Carlton, VIC 3053
  sf_melb %>% filter(mccid_int %in% 102132) %>% pull(mccid_int) %>% as.numeric() # 376-390 Collins St, Melbourne VIC 3000
)

# filter by building id
sf_final_ <- sf_melb %>% 
  mutate_at("mccid_int", as.numeric) %>% 
  filter (mccid_int %in% ids) 


# plot --------------------------------------------------------------------

# plot params
style <- "mapbox://styles/darwinanddavis/cl61qvf5s000415mhs7sagnxv"
width <- 2500
height <- 2500
zoom = 10
pitch = 35
opac <- 0.99
colv <- "#EC407A" # add transparency to end 
colv2 <- "#096DD233"
set_token(readLines('mapbox_key.txt')) # mapbox key

colourvalues::colour_palettes() 
colpal1 <-  colourvalues::colour_values(sf_melb$footprin_2, palette = "greys") 
colpal2 <-  colourvalues::colour_values(1, palette = "heat_hcl")
colpal2 %>% unique %>% scales::show_col(labels = F)
colh <- "#FFAC47FF"
  
# title
tlat <- -37.830238 #sf_final_ %>% st_bbox() %>%  min
tlon <- 144.9581183 #sf_final_ %>% st_bbox() %>%  max
main <- data.frame("Y"= tlat + 0.0005,"X"= tlon + 0.0005,
                   "title"= paste0("Melbourne building\ndevelopments\n(2020-2022)") %>% str_to_upper)

# map
m1 <- mapdeck(NULL,
              style = style, width = width, height = height, zoom = zoom, pitch = pitch) %>% 
  mapdeck_view(zoom = zoom) %>% 
  add_sf(data = sf_melb,
         # fill_colour = colpal1,
         fill_colour = paste0("#FFFFFF","5A"),
         elevation = "footprin_2",
         tooltip = "mccid_int",
         fill_opacity = opac/2,
         auto_highlight = FALSE,
         elevation_scale = 1,
         layer_id = "current"
         ) %>% 
  add_sf(data = sf_final_,
         fill_colour = colh, # use instead for single custom colour from palette
         elevation = "footprin_2",
         tooltip = "mccid_int", #"year",
         fill_opacity = opac,
         auto_highlight = T,
         highlight_colour = "#000000E6",
         elevation_scale = 1,
         layer_id = "2020-2022"
  ) 
m1


# save img ----------------------------------------------------------------
pacman::p_load(mapview)
webshot::install_phantomjs()
width <- 1200
height = 800

png_fl = tempfile(fileext = ".png")
html_fl = tempfile(fileext = ".html")

## create standalone .html
mapshot(m1, file = png_fl)
browseURL(png_fl)

