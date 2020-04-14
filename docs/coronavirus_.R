# 2019-nCov distribution 

# web scraped data from the european centre for disease control
# up to date link ---------------------------------------------------------
# https://darwinanddavis.github.io/worldmaps/coronavirus.html

# packages ----------------------------------------------------------------
# install.packages("pacman")
require(pacman)
p_load(maps,dplyr,leaflet,xml2,rvest,ggmap,geosphere,htmltools,mapview,purrr,rworldmap,rgeos,stringr,here,htmlwidgets,readxl,httr,readr,stringi)

# set wd
here::set_here("/Users/malishev/Documents/Data/worldmaps/worldmaps/")

# scrape data from web \xml2 ---------------------------------------------------------------
url <- "https://www.ecdc.europa.eu/en/geographical-distribution-2019-ncov-cases" # get today's data
# link: https://www.ecdc.europa.eu/en/publications-data/download-todays-data-geographic-distribution-covid-19-cases-worldwide
url2 <- "https://www.ecdc.europa.eu/sites/default/files/documents/COVID-19-geographic-disbtribution-worldwide.csv" # get historical data as of today
url3 <- "https://google.org/crisisresponse/covid19-map" # recovery data from google

# end user input ----------------------------------------------------------
# . -----------------------------------------------------------------------
# . -----------------------------------------------------------------------

# get geocode \ rgeos rworldmaps ------------------------------------------
lonlat <- getMap(resolution="low") %>% # get country lonlats from rgeos database
  gCentroid(byid=TRUE) %>% 
  as.data.frame 
lonlat$Country <- rownames(lonlat) # add country col
colnames(lonlat) <- c("Lon", "Lat","Country") # rename cols

# function for getting lonlat from rgeos database 
find_lonlat <- function(country_string){
  country_string_return <- lonlat %>% filter(Country %in% str_subset(lonlat$Country,country_string))
  country_string_return_name <- country_string_return %>% select("Country") # get country string
  print(country_string_return)
}

# function for getting current country name in cv
set_country_name <-  function(country_name){
  cv[str_which(cv$Country,c(country_name)),"Country"] 
}

# save historical data to dir 
GET(url2, authenticate(":", ":", type="ntlm"), write_disk(tf <- tempfile(fileext = ".csv")))
cv_historical <- read_csv(tf)
cv_historical  %>% head
# write_csv(cv_historical,paste0(here(),"/cv_historical.csv")) # write historical data to file

# convert cv webtable to tibble \rvest 
web_data <- url %>% read_html
tb <- web_data %>% html_table(trim = T) 
cv <- tb[[1]] # get df
cv[is.na(cv)] <- 0 # rm nas

# get recovery and cases per million data from google
web_data_recovered <- url3 %>% read_html
cv2 <- web_data_recovered %>% html_table(trim = T) 
cv2 <- cv2[[2]] # get df
cv2[is.na(cv2)] <- 0

# mod data
cv <- setNames(cv,c("Continent","Country","Cases","Deaths","Cases_last_15_days")) # set names 
cv$Deaths <- cv$Deaths %>% stri_replace_all_charclass("\\p{WHITE_SPACE}","") # remove middle white space
cv$Cases <- cv$Cases %>% stri_replace_all_charclass("\\p{WHITE_SPACE}","")
cv$Cases_last_15_days <- cv$Cases_last_15_days %>% stri_replace_all_charclass("\\p{WHITE SPACE}","")
cv$Deaths <- cv$Deaths %>% as.integer() # set as int
cv$Cases <- cv$Cases %>% as.integer() # set as int
cv$Cases_last_15_days <- cv$Cases_last_15_days %>% as.integer()

# get totals
cv_total <- cv %>% summarise(Total_cases = max(Cases,na.rm = T),
                             Total_deaths = max(Deaths,na.rm = T),
                             Total_recent_cases = max(Cases_last_15_days,na.rm = T))
cv <- cv[!cv$Continent=="Total",] # rm total from country df
cv <- cv[!cv$Country=="Other",] # remove 'other' country
cv <- cv[!cv$Country=="Asia",] # remove 'other' country
# cv <- cv[!cv$Country==stringr::str_subset(cv$Country,"Place"),] # remove descriptive row header

# clean strings
cv$Cases <- cv$Cases %>% str_replace(" ","") %>% as.numeric()
cv$Deaths <- cv$Deaths %>% str_replace(" ","") %>%  as.numeric()
cv$Country <- cv$Country %>% str_replace_all("_"," ") %>% as.character()
cv2$Recovered <- cv2$Recovered %>% str_replace_all(",","") %>% as.character()

# fix anomalies in country entries
# cv[cv$Country=="Japan",c("Cases","Deaths")] <- cv[cv$Country=="Japan",c("Cases","Deaths")] %>% as.numeric + cv[cv$Country=="Cases on an international conveyance Japan",c("Cases","Deaths")] %>% as.numeric
cv <- cv[!cv$Country==str_subset(cv$Country,"conveyance Japan"),] # remove japan duplicate
# rename countries for getting centroid later 
cv[str_which(cv$Country,"Korea"),"Country"] <- "South Korea" 
cv[str_which(cv$Country,"Iran"),"Country"] <- "Iran"
cv[str_which(cv$Country,"Maced"),"Country"] <- "Macedonia"
cv[str_which(cv$Country,"Pales"),"Country"] <- "Palestine* (as neither recognition nor prejudice towards the State)"
cv[str_which(cv$Country,"Ser"),"Country"] <- "Republic of Serbia" # match geocode country string in lonlat /rgeos
cv[str_which(cv$Country,"Holy"),"Country"] <- "Vatican" # match geocode country string in lonlat /rgeos 
cv[str_which(cv$Country,"Brun"),"Country"] <- "Brunei" # match geocode country string in lonlat /rgeos 
cv[str_which(cv$Country,"Congo"),"Country"][1] <- "Republic of the Congo" # republic of the congo
cv[str_which(cv$Country,"Democratic"),"Country"] <- "Democratic Republic of the Congo" # DRC
cv[str_which(cv$Country,"Eswa"),"Country"] <- "Swaziland" # swaziland
cv[str_which(cv$Country,"Ivo"),"Country"] <- "Ivory Coast" # ivory coast
cv[str_which(cv$Country,"Baha"),"Country"] <- "The Bahamas" # bahamas
cv[str_which(cv$Country,"Nether"),"Country"][1] <- "Netherlands"
# cv[str_which(cv$Country,"Nether"),"Country"][2] <- "Netherlands Antilles"
cv[str_which(cv$Country,"Timor"),"Country"] <- "East Timor"
cv[str_which(cv$Country,"Turks"),"Country"] <- find_lonlat("Turks")$Country
cv[str_which(cv$Country,"Cura"),"Country"] <- find_lonlat("Curac")$Country
cv[str_which(cv$Country,"Falkland"),"Country"] <- find_lonlat("Falk")$Country
cv[str_which(cv$Country,"Czech"),"Country"] <- find_lonlat("Czech")$Country


# get totals per continent ## not run 24-2-20  
# cv_continent_cases <- cv %>% filter(Country=="") %>% select(Cases)
# cv_continent_deaths <- cv %>% filter(Country=="") %>% select(Deaths)
# cv_continent_cases$Continent <- cv[,"Continent"] %>% unique
# cv_continent_deaths$Continent <- cv[,"Continent"] %>% unique

# remove empty country rows
# cv <- cv[!cv$Country=="",] 

# rank data
cv <- cv %>% arrange(desc(Cases)) # rank data in descending order to layer map points 
# get global case and death rankings 
cv <- cv %>% mutate(Cases_ranked = (Cases %>% dense_rank %>% max + 1) - (Cases %>% dense_rank),
              Deaths_ranked = (Deaths %>% dense_rank %>% max + 1) - (Deaths %>% dense_rank),
              Cases_15days_ranked = (Cases_last_15_days %>%  dense_rank %>% max + 1) - (Cases_last_15_days %>% dense_rank)
              ) 

# subset
cv_country <- cv$Country
cv_cases <- cv$Cases %>% as.numeric()
cv_deaths <- cv$Deaths %>% as.numeric()
cv_total_cases <- cv_total$Total_cases
cv_total_deaths <- cv_total$Total_deaths
cv_total_recent_cases <- cv_total$Total_recent_cases
cv_recent_cases <- cv$Cases_last_15_days %>% as.numeric()
cv_cases_ranked <- cv$Cases_ranked %>% as.numeric()
cv_deaths_ranked <- cv$Deaths_ranked %>% as.numeric()
cv_cases_15days_ranked <- cv$Cases_15days_ranked %>% as.numeric()
# recovery data
cv2_country <- cv2$Location
cv2_recovered <- cv2$Recovered %>% as.numeric()


# match cv country lonlat to lonlat rgeos database
lonlat_final <- lonlat[cv_country,] 
lonlat_final2 <- lonlat[cv2_country,] 
lonlat_final  %>%   # write to dir
  readr::write_csv("cv_lonlat.csv")

# add lonlat to df
cv[,c("Lon","Lat")] <- lonlat_final[,c("Lon","Lat")]
cv2[,c("Lon","Lat")] <- lonlat_final2[,c("Lon","Lat")] # recovery data
# check country name with latlon
if(any(lonlat_final$Country == cv$Country)!=TRUE){
  cat("\n\n\nCheck country lonlat before plotting\n\n\n",rep("*",10))}

# fix misc latlon
cv[cv$Country=="Malaysia",c("Lon","Lat")] <- c(101.975769,4.210484) # malaysia
cv[cv$Country==cv[str_which(cv$Country,"Pales"),"Country"],c("Lon","Lat")] <- cv %>% filter(Country=="Israel") %>% select(c("Lon","Lat")) + 0.05 # displace Palestine latlon from israel
cv[cv$Country==cv[str_which(cv$Country,"Gibral"),"Country"],c("Lon","Lat")] <- cv %>% filter(Country=="Spain") %>% select(c("Lon","Lat")) + 0.05 # displace gibraltar latlon from spain
# cv[cv$Country==cv[str_which(cv$Country,"Antill"),"Country"],c("Lon","Lat")] <- lonlat %>% filter(Country=="Aruba") %>% select(c("Lon","Lat")) + 0.2  # displace dutch caribbean from aruba
cv[cv$Country==cv[str_which(cv$Country,"Bonai"),"Country"],c("Lon","Lat")] <- lonlat %>% filter(Country=="Curacao") %>% select(c("Lon","Lat")) + 0.2  # displace dutch caribbean2 latlon from curacao

# check NAs
if(any(is.na(cv$Lat))==TRUE){
  cat("\n\n\nLatlon in cv dataset contains NAs\n",rep("*",10),"\n")
  cv[which(is.na(cv$Lat)),"Country"]
}
   

# find which countries show NAs/anomalies from latlon database  
find_lonlat("Czech")
# get current country name in cv for replacement   
set_country_name("Czech") 

# get numeric
lon <- cv$Lon 
lat <- cv$Lat 
lonlat_matrix <- matrix(c(lon,lat), ncol = 2) # get matrix for arcs 

# if using nafta coords # not run 3-3-20
# get character string for nafta to rm from df and get latlon 
# nafta_string <- str_subset(cv$Country,"Amer|Cana|Ecu|Mexi")
# lonlat_matrix <- cv %>% # filter out nafta 
#   filter(!Country %in% nafta_string) %>% 
#   select(c("Lon","Lat")) %>% 
#   unlist %>% 
#   matrix(ncol=2)
# nafta_lon <- cv %>% filter(Country %in% nafta_string) %>% select(c("Lon")) %>% unlist
# nafta_lat <- cv %>% filter(Country %in% nafta_string) %>% select(c("Lat")) %>% unlist

# death latlon
death_lon <- cv %>% filter(Deaths>0) %>% select(c("Lon")) %>% unlist
death_lat <- cv %>% filter(Deaths>0) %>% select(c("Lat")) %>% unlist

# get death labels
cv_deaths_labels <- cv %>% filter(Deaths>0) %>% select(Country) %>% unlist

# style -------------------------------------------------------------------
custom_tile <- names(providers)[113] # choose tiles
custom_tile2 <- names(providers)[110]
colv <- "#F90F40" # cases
colv2 <- "#FA0303" # deaths
colv3 <- "#DA740D" # recent cases 
opac <- 0.7
colvec_cases <- ifelse(cv_cases > 0, colv,NaN) # get colvec w/o nafta cases
colvec_deaths <- ifelse(cv_deaths > 0 ,colv2,NaN) # remove 0 points
colvec_recent_cases <- ifelse(cv_recent_cases > 0, colv3,NaN) # remove 0 points

# set colvec for if removing nafta polylines # not run 3-3-20
# nafta_cases <- cv %>% filter(Country %in% nafta_string) %>% select("Cases") %>% unlist
# colvec_cases <- ifelse(cv_cases %in% nafta_cases,NaN,colv) # get colvec w/o nafta cases
# colvec_deaths <- ifelse(cv_deaths %in% c(0,nafta_cases),NaN,colv2) # remove 0 points 

# add deaths latlon manually # not run 3-2-20
# cv_deaths_lon <- cv %>% filter(Deaths>0) %>% select("Lon") %>% unlist
# cv_deaths_lat <- cv %>% filter(Deaths>0) %>% select("Lat") %>% unlist

# text --------------------------------------------------------------------

# title 
ttl <- paste0("<div style=\"color:#F90F40;\"> 
              2019-nCov 
              </div>","global distribution")

# tr
heading_tr <- paste(
                    "<strong> Total cases <div style=\"color:#F90F40; font-size:150%\">",format(cv_total_cases,big.mark=",",scientific = F,trim = T),"</div> </strong>", "<br/>",
                    "<strong> Total deaths <div style=\"color:#FA0303; font-size:150%\">",format(cv_total_deaths,big.mark = ",",scientific = F,trim = T),"</div> </strong>","<br/>",
                    "<strong> Total cases in last 15 days <div style=\"color:#DA740D; font-size:150%\">",format(cv_total_recent_cases,big.mark = ",",scientific = F,trim = T),"</div> </strong>"
                    )

# bl
heading_bl <- paste(sep = "<br/>",
                    "Data source: <a href=https://www.ecdc.europa.eu/en/geographical-distribution-2019-ncov-cases> 
                    ECDC
                    </a>",
                    "Last data scrape: ", Sys.time(),
                    "",
                    "Github: <a href=https://github.com/darwinanddavis/worldmaps> @darwinanddavis </a>"
                    )



# labels ## not run 
label_cases <- paste(
  "<strong> Continent: </strong>", cv$Continent, "<br/>"
) %>% map(htmltools::HTML)


# popups
popup_cases <- paste(
                     "<strong> Country </strong>","<br/>", cv_country,"<br/>","<br/>",
                     "<strong> Cases </strong>","<br/>", cv_cases,"<br/>","<br/>",
                     "<strong> Global cases ranking </strong>","<br/>", cv_cases_ranked,"/",cv_cases_ranked %>% max,"<br/>","<br/>"
                     # "<strong> Total population: </strong>", world_pop$Country,"(1000s)","<br/>",
                     # "<strong> Percent of population affected: </strong>", cv_cases[1:length(world_pop$Country)]/world_pop$Country,"%","<br/>",
                     # "<strong> Median age: </strong>", world_medage$Country,"<br/>","<br/>"
)

popup_deaths <- paste(
                      "<strong> Country </strong>","<br/>", cv_country,"<br/>","<br/>",
                      "<strong> Deaths </strong>", "<br/>", cv_deaths,"<br/>","<br/>",
                      "<strong> Global death ranking </strong>","<br/>", cv_deaths_ranked,"/",cv_deaths_ranked %>% max
)

popup_recent_cases <- paste(
                             "<strong> Country </strong>","<br/>", cv_country,"<br/>","<br/>",
                             "<strong> Cases in last 15 days </strong>","<br/>", cv_recent_cases,"<br/>","<br/>",
                            "<strong> Global recent cases ranking </strong>","<br/>", cv_cases_15days_ranked, "/",cv_cases_15days_ranked %>% max
                            )

# controlbox 
layer1 <- "Cases"
layer2 <- "Deaths"
layer3 <- "Cases in last 15 days"  

# style options -----------------------------------------------------------

# css

# title 
map_title <- tags$style( 
  HTML(".leaflet-control.map-title { 
       transform: translate(-50%,-20%);
       position: fixed !important;
       left: 50%;
       text-align: center;
       padding-left: 10px; 
       padding-right: 10px; 
       background: white; opacity: 0.5;
       font-size: 40px;
       font-family: Optima;
       }"
       ))

title <- tags$div(
  map_title, HTML(ttl)
)  

# control box
map_control_box <- tags$style( 
  HTML(".leaflet-control-layers-base { 
       text-align: left;
       padding-left: 10px; 
       padding-right: 10px; 
       background: white; opacity: 1;
       font-size: 15px;
       }"
       ))

control_box <- tags$div(
  map_control_box, HTML("")
)  

# text labels 
style <- list(
  "color" = "black",
  "font-weight" = "normal",
  "font-family" = "Optima",
  "padding" = "3px 3px"
)

style_nafta <- list(
  "color" = "#F90F40",
  "font-weight" = "normal",
  "font-family" = "Optima",
  "padding" = "3px 3px"
)

# text label options 
text_label_opt <- labelOptions(noHide = F, direction = "top", textsize = "20px",
                               textOnly = F, opacity = 0.5, offset = c(0,0),
                               style = style, permanent = T
)

text_label_opt_nafta <- labelOptions(noHide = T, direction = "top", textsize = "15px",
                               textOnly = F, opacity = 0.5, offset = c(0,0),
                               style = style_nafta, permanent = T
)

# layer options 
layer_options <- layersControlOptions(collapsed = F)

# tile options
min_zoom <- 3
max_zoom <- 10

# set max map bounds
latlon_origin <- cv %>% filter(Country=="China") %>% select(c("Lon","Lat")) %>% as.numeric() # china lonlat
max_bound1 <- c(-150,90)
max_bound2 <- c(180,-90)

# layers ------------------------------------------------------------------

# titles
layer1 <- "Cases"
layer2 <- "Deaths"
layer3 <- "Cases in last 15 days"  

# point size
radius_cases <- sqrt(cv_cases) * 1500 # 3rd radius reduction 13-4-20
radius_deaths <- sqrt(cv_deaths) * 1500
radius_recent_cases <- sqrt(cv_recent_cases) * 1500

# easy buttons 
locate_me <- easyButton( # locate user
  icon="fa-crosshairs", title="Zoom to my position",
  onClick=JS("function(btn, map){ map.locate({setView: true}); }"));

reset_zoom <- easyButton( # reset zoom 
  icon="fa-globe", title="Reset zoom",
  onClick=JS("function(btn, map){ map.setZoom(3);}"));  
  
# map ---------------------------------------------------------------------

# set arc matrix
cvm <- gcIntermediate(latlon_origin,
                      # lonlat_matrix[1,],
               lonlat_matrix,
               n=100,
               addStartEnd=T,
               breakAtDateLine = T,
               sp=T
) %>% 
  leaflet() %>% 
  setMaxBounds(max_bound1[1],max_bound1[2],max_bound2[1],max_bound2[2]) %>% 
  setView(latlon_origin[1],latlon_origin[2],zoom=min_zoom) %>% 
  addTiles(custom_tile,
           options = providerTileOptions(minZoom=min_zoom, maxZoom=max_zoom) # set zoom bounds
           ) %>% 
  addProviderTiles(custom_tile, 
                   group = c(layer1,layer2,layer3),
                   options = providerTileOptions(minZoom=min_zoom, maxZoom=max_zoom) # set zoom bounds
                   ) %>% 
  addPolylines(color=colvec_cases, # cases
               opacity = opac,
               weight = 0.5,
               group = layer1) %>%
  addPolylines(color=colvec_deaths, # deaths
               opacity = opac,
               weight = 0.5,
               group = layer2) %>%
  addCircles(lon,lat, # cases
             weight=1,
             radius= radius_cases,
             color=colv,
             fillColor=colv,
             label = cv_country,
             popup = popup_cases,
             labelOptions = text_label_opt,
             group = layer1) %>% 
  addCircles(lon,lat, # deaths 
             weight=1,
             radius=radius_deaths,
             color=colvec_deaths,
             fillColor=colvec_deaths,
             label = cv_country,
             popup = popup_deaths,
             labelOptions = text_label_opt,
             group = layer2) %>%
  addCircles(lon,lat, # recent cases 
             weight=1,
             radius=radius_recent_cases,
             color=colvec_recent_cases,
             fillColor=colvec_recent_cases,
             label = cv_country,
             popup = popup_recent_cases,
             labelOptions = text_label_opt,
             group = layer3) %>%
  # addLabelOnlyMarkers(nafta_lon,nafta_lat, # add labels for cases outside of polylines
  #                     label=nafta_string,
  #                     labelOptions = text_label_opt_nafta,
  #                     group=layer1) %>% 
  # addLabelOnlyMarkers(death_lon,death_lat, # add labels for deaths outside of polylines
  #                     label=cv_deaths_labels,
  #                     labelOptions = text_label_opt_nafta,
  #                     group=layer2) %>% 
  addLayersControl( 
    baseGroups = c(layer1,layer2,layer3),
    options = layer_options) %>% 
  hideGroup(c(layer2,layer3)) %>% 
  addControl(title, "bottomleft", className = "map-title") %>% 
  addControl(heading_bl,"bottomleft") %>%
  addControl(heading_tr, "topright") %>% 
  addControl(control_box, "topright", className = "control-layers-base") %>% 
  addEasyButton(reset_zoom) %>% 
  addEasyButton(locate_me) 
 
cvm 
  

# save outputs  ----------------------------------------------------------------
last.warning; geterrmessage() # get last warning and error message 

cvm %>% saveWidget(here("/coronavirus.html"))
cvm %>% saveWidget(here("/worldmaps/coronavirus.html")) # save to local dir 

# save daily totals 
cv_total_df <- data.frame("Date" = Sys.Date(),
                          cv_total,
                          "Space" = NA)
    
# append new total to file and save to dir 
start_date <- "2020-03-26"
if(start_date!=Sys.Date()){
  write_delim(cv_total_df,paste0(here(),"/cv_total_df.csv"),append = T,col_names = F, delim=",")
  cat("New historical data saved to ",here(),"/cv_total_df.csv\n\n");Sys.Date()
}



