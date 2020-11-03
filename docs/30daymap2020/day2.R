# --------------------------------------------------------------------
# 30 day map challenge 2020
# day 2
# author: Matt Malishev
# @darwinanddavis  


plot_final_pdf <- T
source("funcs/user_read_func.R") # load user and map details
source("funcs/page_print_func.R") # load page print func 
source("funcs/add_font_func.R") # load font lib func 

if(!is.null(paste0(here::here(month_current,user),"/",user,".Rda"))){
  cat("Reading in final city_df from dir\n")
  city_df <- readRDS(paste0(here::here(month_current,user),"/",user,".Rda")) 
} 

# packages  ---------------------------------------------------------------

# pcks
pacman::p_load(here,sf,RColorBrewer,dplyr,ggmap,RgoogleMaps,sp,maptools,scales,rgdal,ggplot2,jsonlite,readr,devtools,colorspace,mapdata,ggsn,mapview,mapproj,ggthemes,reshape2,grid,rnaturalearth,rnaturalearthdata,ggtext,purrr)
# high res map version 
require(mapdata) # high res data
require(ggsn) # north symbols and scale bars

# load world latlon, region, country names --------------------------------

# get city names
data(world.cities) # /maps
world_cities <- world.cities

# get country names  
world <- ne_countries(scale = "large", returnclass = "sf") # /rnaturalearth /rnaturalearthdata
world_points<- st_centroid(world) # /sf
world_points <- cbind(world, st_coordinates(st_centroid(world$geometry)))

world %>% class
st_centroid(world$geometry) 
# data(us.cities) # option 2 for us cities  

# find missing cities
find_city <- NULL

if(!is.null(find_city)){
  find_city_save <- world_cities[str_which(world_cities$name,c(find_city)),"name"] 
  find_city_save
}

# load data  --------------------------------------------------------------

city_df <- world_cities %>% 
  filter(country.etc %in% countries, name %in% sites) %>% 
  select(lat = lat,
         lon = long,
         city = name
  ) %>% 
  arrange(match(city,sites))   # reorder to match route path 

write_csv(city_df,here::here(month_current,user,paste0(fh,".csv")))

# add global label nudge position to df -------------------------------------------------------------------
label_xy <- data.frame( # global xy label positions, positions 0 to 7 (uses ggtext positioning)
  "hjust" = c(0.5,0,0,0,0.5,1,1,1),
  "vjust" = c(0,0,0.5,1,1,1,0.5,0)
)
label_nudge <- data.frame( # global xy label nudge positions, positions 0 to 7 (uses ggtext positioning)
  "nudge_x" = rep(0,city_df %>% nrow),
  "nudge_y" = rep(0.1,city_df %>% nrow)
)

# ----------------------------------------------------------------------------------------------------------------------
# ----------------------------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------------------------------
# read in kml data ------------------------------------------------------------

# read in kml data from my google maps (drive route)
path <- sf::st_read(here::here(month_current,user,paste0(fh,".kml")))

# high res map
d <- map_data("worldHires"
              # , countries # select which countries to plot  
)
usa <- map_data("worldHires",
                c("USA","Canada","Mexico")
                # , countries # select which countries to plot  
)

# nudge labels ------------------------------------------------------------
add_label_box = F

# add text label positioning columns
cat(rep("\n",3), "Total no. of cities = ",city_df %>% nrow)
hv_just <- c(1,1,2,7,1,3,3,6,7,7) # set city label xy
city_df[,c("hjust","vjust")] <- label_xy[hv_just,] # add hust/vjust
city_df[,c("nudge_x","nudge_y")] <- label_nudge  # add nudge_x/nudge_y


# city_df[9,"city"] <- "Ho Chi Minh"

# check what labels need to be nudged  
require(ggtext) 
text_test <- ggplot() + geom_sf(data=path,color=path_col,size=path_size) + geom_point(data=city_df,aes(lon,lat),col=path_col,size=city_size) + 
  if(add_label_box==F){geom_richtext(data=city_df,aes(lon,lat,label=city,family=city_label_font,hjust=hjust,vjust=vjust), color=path_col, size = city_label_size, fill = NA, label.color = NA, nudge_x = city_df$nudge_x,nudge_y = city_df$nudge_y)
    # add label box 
  }else{
    geom_richtext(data=city_df,aes(lon,lat,label=city,family=city_label_font,size=city_label_size, color=path_col,hjust=hjust,vjust=vjust), size = city_label_size, fill = "white", color = path_col, label.color = path_col, label.padding = grid::unit(rep(7.5, 4), "pt"),nudge_x = city_df$nudge_x,nudge_y = city_df$nudge_y)
  } 
ggsave(here::here(month_current,user,paste0(paste(fh,user,"labels_to_nudge",sep="_"),".png")), text_test, width = 21, height = 29.7, units = units)

# adjust labels xy pos as needed
city_to_nudge <- c(2,3,4,6,7,8,9,10)

# nudge labels in df
city_df <- city_df %>% 
  mutate(nudge_x = nudge_x %>% 
           replace(city_to_nudge,
                   c(0.5,-0.5,-0.2,0.2,0.2,0.2,-0.2,-0.2)),
         nudge_y = nudge_y %>% 
           replace(city_to_nudge,
                   c(0,0,0,0,0,-0.2,0,0.2)))
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# plot --------------------------------------------------------------------
# save final city_df to dir
saveRDS(city_df,paste0(here::here(month_current,user),"/",user,".Rda")) 

source("funcs/user_read_func.R") # load user and map details
source("funcs/page_print_dims.R") # load user and map details
source("funcs/title_xy.R") # load user and map details

graphics.off()

print_func(paper_size,orientation) # print func


if(orientation=="l"){
  bbox <- city_df %>% summarise(mean(lon)-(height/2), # x axis
                                mean(lon)+(height/2), 
                                mean(lat)-(width/2),
                                mean(lat)+(width/2)) %>% unlist 
}else{
  bbox <- city_df %>% summarise(mean(lon)-(width/2), # x axis
                                mean(lon)+(width/2), 
                                mean(lat)-(height/2),
                                mean(lat)+(height/2)) %>% unlist 
}

# title and subtitle position // title_xy.R
map_title_final <- title_xy_func(title_xy,0,2)

# color palette 
# recover default plot margins 
theme_nothing()$plot.margin

p <- ggplot() + 
  geom_polygon(data=usa,aes(x=long, y=lat, group = group), fill = fg, col=border_col,size=0.1) +
  geom_polygon(data=d,aes(x=long, y=lat, group = group), fill = fg, col=border_col,size=0.1) +
  geom_sf(data=path,color=path_col,size=path_size) + # add path
  geom_point(data=city_df,aes(lon,lat),col=path_col,size=city_size) + # add cities
  coord_sf(xlim = c(bbox[1],bbox[2]), ylim = c(bbox[3],bbox[4])) + # bbox
  theme_nothing() +
  theme(panel.grid.major = element_line(colour = bg),
        plot.background = element_rect(fill = bg),
        axis.text = element_blank(), 
        axis.ticks.length=unit(0, "null"),
        panel.ontop = F
  ) +
  labs(x = NULL, y = NULL) + 
  # no label box
  if(add_label_box==F){geom_richtext(data=city_df,aes(lon,lat,label=city,family=city_label_font,hjust=hjust,vjust=vjust), color=path_col, size = city_label_size, fill = NA, label.color = NA, nudge_x = city_df$nudge_x,nudge_y = city_df$nudge_y)
    # add label box 
  }else{
    geom_richtext(data=city_df,aes(lon,lat,label=city,family=city_label_font,size=city_label_size, color=path_col,hjust=hjust,vjust=vjust), size = city_label_size, fill = "white", color = path_col, label.color = path_col, label.padding = grid::unit(rep(7.5, 4), "pt"),nudge_x = city_df$nudge_x,nudge_y = city_df$nudge_y)
  } 
if(plot_country_labels==T){ # add country labels 
  p <- p + geom_text(data= world_points,aes(x=X+1, y=Y+1, label=name, family=country_label_font),color = country_label_colour, size = country_label_size, check_overlap = T)
}
p
dev.off()

# cowplot::save_plot(paste0(here::here(month_current,user),"/",paste(fh,user,sep="_"),"_cow.pdf"),p, ncol=1,nrow=1,base_width = 16.5, base_height = NULL, base_asp = 2.4)
ggsave(here::here(month_current,user) %>% paste0("/",paste(fh,user,sep="_"),".pdf"), p, width = height, height = width,limitsize = F,units = units)
if(plot_final_pdf==T){
  if(orientation=="p"){
    ggsave(here::here(month_current,user) %>% paste0("/",paste(user,"final",sep="_"),".pdf"), p, width = width, height = height, dpi = "retina", units = units, limitsize = F)
  }else{
    ggsave(here::here(month_current,user) %>% paste0("/",paste(user,"final",sep="_"),".pdf"), p, width = height, height = width, dpi = "retina", units = units, limitsize = F)
  }
  write_lines(c(width,height,orientation,paper_size),paste0(here::here(month_current,user),"/",paste(fh),".txt"),append = T)
}
