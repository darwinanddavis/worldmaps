##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                            #30daymapchallenge2023                        ----
##                                day 6 - asia                              ~~~~
##                            author: Matt Malishev                         
##                               @darwinanddavis                            
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# pcks --------------------------------------------------------------------
pacman::p_load(here,dplyr,ggplot2,readr,ggthemes,colorspace,sf,rnaturalearth,lubridate,svglite,rgdal,stringr,gganimate,scales,concaveman,patchwork,grid,forcats,rmapshaper,threejs,magick)

# data --------------------------------------------------------------------

# globe vessel data
globe_top3_2020 <- readRDS(here::here("data","globe","top3_2020.Rda"))
globe_all_2020 <- readRDS(here::here("data","globe","all_countries_2020.Rda"))
globe_all_aus_eez <- readRDS(here::here("data","globe","globe_all_aus_eez.Rda"))

# read and save target country/vessels to dir -----------------------------

tcs <- "TWN"
df <- here::here("data","vessels") %>% list.files(full.names = T) %>% read_csv
for(tc in tcs){
  target_country <- df %>% 
    filter(flag_gfw %in% tc)
  target_country$flag_gfw %>% unique
  mmsi_target <- target_country %>% pull(mmsi) %>% unique
  target_country_activity <- c()
  yrs <- 2020
  for(yr in yrs){
    fh <- "" # "2020-01-01" $ use "" for all years 
    activity <- here::here("data","daily_activity",paste0("mmsi-daily-csvs-10-v2-",yr)) %>% list.files(fh,full.names = T) %>% read_csv 
    activity_distinct <- activity %>% 
      filter(mmsi %in% mmsi_target & fishing_hours > 0) %>% # or get target countries
      distinct(cell_ll_lat, cell_ll_lon, .keep_all = T) %>% # get distinct latlon to simplify data
      rename("lat" = 2, "lon" = 3) %>% 
      st_as_sf(coords = c("lon","lat"),crs = 4326)
    
    target_country_activity <- target_country_activity %>% rbind(activity_distinct)
  }
  dir.create(here::here("data","globe","shp",tc))
  target_country_activity %>% saveRDS(here::here("data","globe",paste0("globe_",tc,"_",yrs,".Rda")))
  target_country_activity %>% st_write(here::here("data","globe","shp",tc,paste0("globe_",tc,"_",yrs,".shp")))
}


# world data --------------------------------------------------------------


bg <- "#FFFFFF"  
opac <- 1

# add world map img bg
here::here("img","world.topo.bathy.200412.3x5400x2700.jpg") %>% 
  image_read(strip = T) %>%
  image_convert(colorspace = "gray", matte = T,depth = 8)  %>%
  image_colorize(color = paste0(bg,"ff"), opacity = opac) %>% 
  image_write(here::here("img",paste0("globe",bg,opac,"convert_colorise.png")) ,format = "png", depth=NULL)
earth <- here::here("img",paste0("globe",bg,opac,"convert_colorise.png"))

# function to load data and plot globe 
plot_globe <- function(mm,colors,cex,fov,rlon,rlat){
  x <- activity %>%
    filter(mmsi %in% mm) %>% #  target_mmsi tv_top
    distinct(cell_ll_lat, cell_ll_lon, .keep_all = T) %>% # get distinct latlon to simplify data
    rename("lat" = 2, "lon" = 3)
  val <- x$fishing_hours
  fov <-fov
  rlon <- rlon
  rlat <- rlat
  # globe
  globejs(
    img = earth,
    lat = x$lat, long = x$lon, val = val , # use 'val' to show fishing as columns 
    color = colors, pointsize = cex, atmosphere = T, bg = bg, fov = fov, rotationlong = rlon,rotationlat = rlat # ,height = height,width = height
  )
}

# plot globe ----------------------------------------------------------------
fov = 30
rlon = 90
rlat = 0
colors <-"#E3D20F" 
mm <- df %>% filter(flag_gfw == "TWN") %>% pull(mmsi) 
plot_globe(mm,colors,0.4,fov,rlon,rlat)

