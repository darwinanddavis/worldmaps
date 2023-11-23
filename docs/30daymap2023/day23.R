##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                            #30daymapchallenge2023                        ----
##                                day 23 - 3D                               ~~~~
##                            author: Matt Malishev                         
##                               @darwinanddavis                            
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# pcks --------------------------------------------------------------------
pacman::p_load(sf,rmapshaper,ggplot2,colorspace,here,rnaturalearth,patchwork, dplyr,stringr,readr,maps,rnaturalearth,colorspace,grid,forcats,rmapshaper,threejs,magick,readxl,stringi)
sf_use_s2(FALSE)

# data --------------------------------------------------------------------

# load data (oct to nov 2023)
get_data <- function(fh) here::here("data","pro_neutral") %>% list.files(full.names = T) %>% 
  lapply(read_xlsx) %>% rlist::list.rbind() %>% 
  filter(`Israel-Palestine demos` %in% fh) %>% suppressWarnings()

df <- here::here("data","pro_neutral") %>% list.files(full.names = T) %>% 
  lapply(read_xlsx) %>% rlist::list.rbind()
d_neutral <- "Israel-Palestine neutral" %>% get_data() 
d_israel <- "pro-Israel" %>% get_data()
d_palestine <- "pro-Palestine" %>% get_data()


# plot --------------------------------------------------------------------

# check data by plotting per continent and save to dir
colpal <- diverge_hcl(100,"Tofino")[c(10,50,100)] %>% darken(0.4)
width <- 20
height <- 20

# loop through regions and save 
for(i in dd$region_un %>% unique){
  bbox <- dd %>% filter(region_un == i) %>% 
    # mutate("area" = st_area(.)) %>% arrange(area %>% desc) %>% .[1:3,] %>%  
    st_bbox
  ggplot() + 
    geom_sf(data = dd %>% filter(region_un == i)) +
    geom_point(data = df, aes(longitude,latitude, col = `Israel-Palestine demos` %>% factor)) +
    scale_colour_manual(values = colpal, aesthetics = "col") +
    coord_sf(xlim = c(bbox[1],bbox[3]), ylim = c(bbox[2],bbox[4])) +
    labs(title = i) +
    theme_bw() +
    theme(legend.position = "bottom")
  ggsave(here::here("plot",paste0(i,".png")),device = "png", width = width, height = height, units = "cm", dpi = "print")
}


# load data ----------------------------------------------------------------

# get start latlon based on first event date per country  
get_latlon_origin <- function(df) df %>% 
  group_by(country) %>% 
  arrange(event_date) %>% # order by time
  mutate("From_Lat" = latitude %>% .[1], "From_Long" = longitude %>% .[1]) %>% # get starting latlon
  arrange(country) %>% # order by country 
  mutate("side" = `Israel-Palestine demos`) %>% # define side
  as.data.frame() 

d_palestine_ <- d_palestine %>% get_latlon_origin()
d_neutral_ <- d_neutral %>% get_latlon_origin()
d_israel_ <- d_israel %>% get_latlon_origin()

dfinal <- rbind(d_palestine_,d_neutral_,d_israel_) %>%  # reorder df to match colpal order
  mutate("region_new" = case_when( # collapse continents 
    region %in% str_subset(region,"Africa")  ~ "Africa",
    region %in% str_subset(region,"Asia")  ~ "Asia",
    region %in% c("Caribbean","North America")  ~ "North America",
    region %in% c("South America","Central America")  ~ "South America",
    T ~ region
  ))

# 3d plot -----------------------------------------------------------------
pacman::p_load(mapdeck)
here::here("mb.txt") %>% read_lines %>% set_token() # mapbox token
my_style <- "mapbox://styles/darwinanddavis/ckvxevehj5sui15qp3l80qfll" # style  

# map vars 
zoom <- 5
height_arc <- 0.3
stroke <- 0.5
opac <- 100
rad <- 5 * 10^3
s <-  0.5 # speed
tl <- 100 # 12 # trail length
ti <- 0 # -40 # tilt
freq <- 0.3
colp <- "#4F53B7" # palestine 
coln <- "#6B6B6B"  # neutral
coli <- "#F7921F" # israel
colpal <- c(colp,coln,coli)

# load data
dm <- dfinal %>% dplyr::select(event_date,country,side,latitude,longitude,From_Long,From_Lat,region_new)

# create function to plot mapdeck arcs and points per continent  
get_arc <- function(reg,ss){
  dm_ <- dm %>% filter(region_new == reg & side == ss) # filter data by region and protest side
  dm_n <- dm %>% filter(region_new == reg & side == "Israel-Palestine neutral") # neutral data for comparison
  ifelse(dm_n %>% nrow == 0, dm_n <- dm_, dm_n <- dm_n) # if no neutral data per region, plot other protest dataset
  ifelse(ss == "pro-Palestine", colv <- colp, colv <- coli) # update colour 
  mapdeck(location = c(dm_$From_Long[1],dm_$From_Lat[1]), # load mapdeck
          style = my_style, show_view_state = T) %>% 
    add_animated_arc(data = dm_, # add arc
                     layer_id = ss,
                     origin = c("From_Long","From_Lat"),
                     destination = c("longitude","latitude"),
                     stroke_from = colv, stroke_to = colv,
                     stroke_width = stroke,
                     frequency = "event_date",
                     height = height_arc,
                     animation_speed = s, trail_length = tl, tilt = ti, # animation options
                     update_view = T, focus_layer = T,
                     legend = F) %>%
    add_animated_arc(data = dm_n, # add neutral protest data
                     layer_id = "neutrala",
                     origin = c("From_Long","From_Lat"),
                     destination = c("longitude","latitude"),
                     stroke_from = coln, stroke_to = coln,
                     stroke_width = stroke,
                     frequency = "event_date",
                     height = height_arc,
                     animation_speed = s, trail_length = tl, tilt = ti, # animation options
                     update_view = T, focus_layer = F,
                     legend = F) %>%
    # add points 
    add_scatterplot(data = dm_, layer_id = ss, lon = "longitude", lat = "latitude",fill_colour = colv, stroke_colour = colv, radius = rad) %>% 
    add_scatterplot(data = dm_, layer_id = "origin_data", lon = "From_Long", lat = "From_Lat",fill_colour = colv, stroke_colour = colv, radius = rad * 1.5) %>% 
    add_scatterplot(data = dm_n, layer_id = "neutralp", lon = "longitude", lat = "latitude",fill_colour = coln, stroke_colour = coln, radius = rad) %>% 
    add_scatterplot(data = dm_n, layer_id = "origin_neutral", lon = "From_Long", lat = "From_Lat",fill_colour = coln, stroke_colour = coln, radius = rad * 1.5)

}


# final plots -------------------------------------------------------------
# read mapdeck pov string from viewer pane and save as vector (region order is alphabetical)
tibble("continent" = c("Africa","Asia","Europe","Middle East","North America","Oceania","South America"),
       "pov" = here::here("data","map","mapdeck_povs.txt") %>% read_lines() %>% stringi::stri_remove_empty(na_empty = F)) %>% 
  saveRDS(here::here("data","map","mapdeck_povs_df.Rda"))
gnv <- function(reg) here::here("data","map","mapdeck_povs_df.Rda") %>% readRDS() %>% filter(continent == reg) %>% pull(pov) %>% str_split_fixed(",",Inf) %>% .[1,] %>% .[3:8] %>% readr::parse_number() %>% return()

# africa
reg <- "Africa"; ss <- "pro-Palestine" #"pro-Israel"
mp <- get_arc(reg,ss) # plot mapdeck (need to load map first before updating view)
mp %>% mapdeck_view(location = c(gnv(reg)[2],gnv(reg)[1]), zoom = gnv(reg)[3], bearing = gnv(reg)[4],pitch = gnv(reg)[5]) 

# asia
reg <- "Asia"; ss <- "pro-Palestine" #"pro-Israel"
mp <- get_arc(reg,ss) 
mp %>% mapdeck_view(location = c(gnv(reg)[2],gnv(reg)[1]), zoom = gnv(reg)[3], bearing = gnv(reg)[4],pitch = gnv(reg)[5]) 

# europe
reg <- "Europe"; ss <- "pro-Palestine" #"pro-Israel"
mp <- get_arc(reg,ss) 
mp %>% mapdeck_view(location = c(gnv(reg)[2],gnv(reg)[1]), zoom = gnv(reg)[3], bearing = gnv(reg)[4],pitch = gnv(reg)[5]) 

# middle east
reg <- "Middle East"; ss <- "pro-Palestine" #"pro-Israel"
mp <- get_arc(reg,ss)
mp %>% mapdeck_view(location = c(gnv(reg)[2],gnv(reg)[1]), zoom = gnv(reg)[3], bearing = gnv(reg)[4],pitch = gnv(reg)[5]) 

# north america
reg <- "North America"; ss <- "pro-Palestine" #"pro-Israel"
mp <- get_arc(reg,ss)
mp %>% mapdeck_view(location = c(gnv(reg)[2],gnv(reg)[1]), zoom = gnv(reg)[3], bearing = gnv(reg)[4],pitch = gnv(reg)[5]) 

# Oceania
reg <- "Oceania"; ss <- "pro-Palestine" #"pro-Israel"
mp <- get_arc(reg,ss)
mp %>% mapdeck_view(location = c(gnv(reg)[2],gnv(reg)[1]), zoom = gnv(reg)[3], bearing = gnv(reg)[4],pitch = gnv(reg)[5]) 


reg <- "South America"; ss <- "pro-Palestine" #"pro-Israel"
mp <- get_arc(reg,ss)
mp %>% mapdeck_view(location = c(gnv(reg)[2],gnv(reg)[1]), zoom = gnv(reg)[3], bearing = gnv(reg)[4],pitch = gnv(reg)[5]) 



# stacked bar plot -------------------------------------------------------------------------
my_theme <- theme( # regular map bg
  plot.background = element_rect(fill = "transparent", color = "transparent"),
  panel.background = element_rect(fill = "transparent", color = "transparent"),
  text = element_text(color = coln), axis.text = element_text(color = coln),
  axis.line = element_line(color = coln), axis.ticks = element_line(color = coln)
)

# plot 
coll <- "#efeef1" # bar line col
locs <- dfinal$region_new %>% unique
width <- 20
height <- 8
for(loc in locs){
  limits <- dfinal$side %>% unique #%>% .[c(3,1,2)] # set colpal order
  colpal <- c(colp,coln,coli)
  limits; colpal %>% scales::show_col()
  # protest side over time by region
  dfinal %>% 
    filter(region_new %in% loc) %>% 
    group_by(event_date,side) %>% 
    count(side) %>% 
    ggplot() +
    geom_bar(aes(event_date, n, fill = factor(side, level = limits)), col = coll, size = 0.3, stat="identity",show.legend = F) +
    labs(x = "", y = "") +
    scale_fill_manual("Protest side",values = colpal, aesthetics = "fill", limits = limits) +
    theme_classic() + my_theme
  ggsave(here::here("plot","bar",paste0(loc,"_.png")), device = "png", dpi = "retina", width = width, height = height, units = "cm", bg = "transparent")
  
  # sum protest side by region 
  require(gghighlight)
  dfinal %>%  
    group_by(region_new,event_date,side) %>%
    count(side) %>% 
    ggplot() +
    geom_bar(aes(region_new, n, fill = factor(side, level = limits)), col = NA, size = 0, stat="identity",inherit.aes = T, show.legend = F) +
    scale_fill_manual("Protest side",values = colpal, aesthetics = "fill", limits = limits) +
    gghighlight(region_new == loc, unhighlighted_params = list(fill = NULL, alpha = 0.3)) +
    labs(x = "", y = "") +
    # coord_flip() +
    theme_classic() + my_theme 
  ggsave(here::here("plot","bar",paste0(loc,"_region.png")), device = "png", dpi = "retina", width = width, height = height, units = "cm", bg = "transparent")
}


# 3d globe ----------------------------------------------------------------

# earth image texture
bg <- "#F1C775" 
coll <- "#000000"
colh <- "#CE7024"
opac <- 1
bg %>% scales::show_col()

# add world map img bg
# colorspace_types()
"http://eoimages.gsfc.nasa.gov/images/imagerecords/73000/73909/world.topo.bathy.200412.3x5400x2700.jpg" %>% 
  image_read(strip = T) %>%
  image_convert(colorspace = "LinearGray",matte = T,depth = 8)  %>%
  image_colorize(color = paste0(bg,"ff"), opacity = opac) %>%
  image_colorize(color = bg, opacity = 1) %>%
  image_write(here::here("img",paste0("globe",bg,opac,"convert_colorise.png")) ,format = "png", depth=NULL)
earth <- here::here("img",paste0("globe",bg,opac,"convert_colorise.png"))


# function for loading data and plotting globe 
plot_globe <- function(mm,colors,cex,fov,rlon,rlat){
  x <- df %>%
    distinct(longitude, latitude, .keep_all = T) %>% # get distinct latlon to simplify data
    rename("lat" = 2, "lon" = 3)
  val <- 2
  fov <-fov
  rlon <- rlonf
  rlat <- rlat
  # globe
  globejs(
    img = earth,
    lat = x$lat, long = x$lon, val = val , # use 'val' to show fishing as columns 
    color = colors, pointsize = cex, atmosphere = F, bg = "#000000", fov = fov, rotationlong = rlon,rotationlat = rlat # ,height = height,width = height
  )
}

# define data colpal and globe texture 
bg <- "#6A6A5E" #"#F1C775" 
fov = 30
rlon = 90
rlat = 0
archeight <- 0.1
colors <- c(
  rep(colp,d_palestine_ %>% nrow)
  ,rep(coln,d_neutral_ %>% nrow)
  ,rep(coli,d_israel_ %>% nrow)
)
colors %>% scales::show_col(labels = F,borders = "#FFFFFF")

dg <- d_palestine_
globejs(
  img = earth,
  # lat = ddd1$latitude, long = ddd1$longitude,
  val = 1, color = colors, pointsize = 1, atmosphere = F, bg = bg, fov = fov, rotationlong = rlon,rotationlat = rlat, # ,height = height,width = height
  arcs = dg %>% dplyr::select(From_Lat,From_Long,latitude,longitude) %>% distinct(), arcsHeight = archeight, arcsLwd = 1,
  arcsColor=colors,arcsOpacity = 0.7
  ,lightcolor = "#d6d6d6"
  # ,bodycolor = "#e9e9e9"
  # ,emissive = "#6A6A5E"
)

