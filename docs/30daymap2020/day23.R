# --------------------------------------------------------------------
# 30 day map challenge 2020
# day 23
# author: Matt Malishev
# @darwinanddavis  

# pkgs --------------------------------------------------------------------
pacman::p_load(here,dplyr,rworldmap,mapdeck,sf,sfheaders,data.table,readr,rgeos,purrr,stringr,ggthemes,showtext,geosphere,htmlwidgets)

# data --------------------------------------------------------------------
rus <- "https://query.data.world/s/5mx46siuiq62l6y4m6che6ute47iys" %>% read_csv(trim_ws = T,skip = 1)
rus <- rus %>% select(c(1,3,4)) %>% 
  rename("Country" = 1,"Year" = 2,"Number" = 3) %>% 
  mutate_at(vars(Number), funs(as.numeric(Number))) # convert to num
rus <- rus[complete.cases(rus),]

# get country latlon  -----------------------------------------------------
# get geocode \ ggmap rworldmaps
lonlat <- getMap(resolution="low") %>% # get country lonlats from rgeos database
  gCentroid(byid=T) %>% 
  as.data.frame 
lonlat$Country <- rownames(lonlat) # add country col
colnames(lonlat) <- c("Lon", "Lat","Country") # rename cols

# get matrix of latlon \rgeos
dm <- lonlat[lonlat$Country %in% rus$Country,c("Lon","Lat")] 
dm$Country <- rownames(dm) # add country col
om <- lonlat[lonlat$Country %in% "Russia",c("Lon","Lat")] # make russia origin
rownames(dm) <- NULL; rownames(om) <- NULL # rm rownames

# fill rows with origin latlon
dm %>% setDT() 
dm[,'oLon':=om$Lon] 
dm[,'oLat':=om$Lat] 

# create df and vars 
rusdf <- merge(rus,dm,by="Country")

# summarise df
rusdf <- rusdf %>% mutate_at(vars(Year), funs(
  case_when(Year %>% str_detect("195") ~ 1950, # collapse years 
            Year %>% str_detect("196") ~ 1960,
            Year %>% str_detect("197") ~ 1970,
            Year %>% str_detect("198") ~ 1980,
            Year %>% str_detect("199") ~ 1990,
            Year %>% str_detect("200") ~ 2000,
            Year %>% str_detect("201") ~ 2010
  ))) %>% 
  group_by(Country,Year,Lon,Lat,oLon,oLat) %>% # create new df
  summarise_at(vars(Number), funs(Number %>% sum)) %>% # get summed n
  arrange(Year) 

# add map vars to df
v1 <- "Country" # var for colvec 
v2 <- "Year" # var for stroke 
colv <- sequential_hcl(rusdf[,v1] %>% unique %>% lengths,"Burg")
stroke <- seq_along(rusdf[,v2] %>% unique %>% unlist)
cc <- data.frame(colv,rusdf[,v1] %>% unique) # create unique dfs 
ss <- data.frame(stroke,rusdf[,v2] %>% unique)
colnames(cc) <- c("Colour",v1)
colnames(ss) <- c("Stroke",v2)
rusdf <- merge(rusdf,cc,by=v1) # match colours to cc
rusdf <- merge(rusdf,ss,by=v2) # match stroke to ss
# height <- rnorm(rusdf %>% nrow,1,0.2) # scatter height 
height <- rusdf$Stroke/10 # stagger height
freq <- rusdf$Number/500 # lag

# label  
family <- "Candara"
label <- paste0( # add label tooltip  
  "<div style=\"background-color: rgba(255,255,255,0.9); border-color: #FFFFFF; color:",rusdf$Colour,"; padding: 8px; font-family:",family,";\">
  <b>",rusdf$Country %>% str_to_upper(),"<b><br><br>
  <b>Decade <b><br>", rusdf$Year,"'s <br><br> 
  <b>No. of refugees <b><br>", rusdf$Number %>% format(big.mark=",",scientific = F,trim = T), 
  "</div>")

# add vars to df 
rusdf <- rusdf %>% mutate(Label = label,
                          Height = height,
                          Freq = freq)

# style -------------------------------------------------------------------
my_style <- "mapbox://styles/darwinanddavis/ckhxh9u580u8819noa3ucl3q1" # style  
my_style_public <- "https://api.mapbox.com/styles/v1/darwinanddavis/ckhxh9u580u8819noa3ucl3q1.html?fresh=true&title=copy&access_token="
ttl <- paste("70 years of\nRussian refugee\nresettlement") 
subttl <- paste0("___________________ \n",
                 "Width = Decade \n  ",
                 "Frequency = No. of refugees \n")
dttl <- "UN Refugee Agency" 
durl <- "https://data.world/unhcr"
colvl <- colv[1] # link col

# labels 
main <- data.frame("Y"=-38,"X"=75.3,"size" = 20,
                   "angle" = -55, "title"= ttl) 

main2 <- data.frame("Y"=main$Y - 12,"X"=main$X,
                    "size" = main$size/2,
                    "angle" = main$angle, 
                    "title"= subttl
                    )

title_text <- list(title = 
                     paste0("<strong style=color:",colvl,";>70 years of Russian<br>refugee resettlement</strong><br/>
                            Author: <a style=color:",colvl,"; href=https://darwinanddavis.github.io/DataPortfolio/> Matt Malishev </a> <br/>
                            Github: <a style=color:",colvl,"; href=https://github.com/darwinanddavis/worldmaps/tree/gh-pages> @darwinanddavis </a> <br/>
                            Data source: <a style=color:",colvl,"; href=",durl,"/>",dttl,"</a> <br/>
                            Map style: <a style=color:",colvl,"; href=", my_style_public,"MAPBOX_ACCESS_TOKEN> Mapbox </a> <br/>
                            Spot an error? <a style=color:",colvl,"; href=https://github.com/darwinanddavis/worldmaps/issues> Submit an issue </a> <br/>"),
                   css = paste0("font-size: 11px; background-color: rgba(255,255,255,0.7); font-family: ",family,";")
                     )
# map vars 
zoom <- 5
s <- 25 # speed
tl <- 200 # trail length
ti <- -40 # tilt

# map
mapdeck(
  location = c(rusdf$Lon[1],rusdf$Lat[1]),
  zoom = zoom,
  style = my_style
) %>% 
  add_animated_arc(data = rusdf,
                   layer_id = "year",
                   origin = c("oLon","oLat"),
                   destination = c("Lon","Lat"),
                   stroke_from = "Colour",
                   stroke_to = "Colour",
                   stroke_width = "Stroke",
                   frequency = "Freq",
                   height = "Height",
                   animation_speed = s,
                   trail_length = tl,
                   tilt = ti,
                   update_view = F, focus_layer = T,
                   auto_highlight = T,
                   highlight_colour = "#000000E6",
                   tooltip = "Label",
                   legend = F
  ) %>% add_text(data=main,lat = "Y", lon = "X",
                 text = "title", layer_id = "m1",
                 alignment_baseline = "top",anchor = "start",
                 fill_colour = colvl,
                 size = "size", # angle = "angle",
                 billboard = F,update_view = F,
                 font_weight = "bold",
                 font_family = family
  ) %>%
  add_text(data=main2,lat = "Y", lon = "X",
           text = "title", layer_id = "m2",
           alignment_baseline = "top",anchor = "start",
           fill_colour = colvl,
           size = "size", #angle = "angle",
           billboard = F,update_view = F,
           font_family = family
  ) %>%
  add_title(title = title_text,layer_id = "heading") %>% 
  htmlwidgets::saveWidget(here::here("worldmaps","30daymap2020","day23.html"))
