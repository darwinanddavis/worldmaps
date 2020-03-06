# 2019-nCov distribution 

# web scraped data from the european centre for disease control
# up to date link ---------------------------------------------------------
# https://darwinanddavis.github.io/worldmaps/coronavirus.html

# . -----------------------------------------------------------------------
# . -----------------------------------------------------------------------


# packages ----------------------------------------------------------------
# install.packages("pacman")
require(pacman)
p_load(maps,dplyr,leaflet,xml2,rvest,ggmap,geosphere,htmltools,mapview,purrr,rworldmap,rgeos,stringr,here,htmlwidgets)

# read data ---------------------------------------------------------------
here::set_here("/Users/malishev/Documents/Data/worldmaps/worldmaps/")
# scrape data from web \xml2
url <- "https://www.ecdc.europa.eu/en/geographical-distribution-2019-ncov-cases"
web_data <- url %>% read_html

# convert to tibble \rvest
tb <- web_data %>% html_table(trim = T) 
cv <- tb[[1]] # get df
cv <- setNames(cv,c("Continent","Country","Cases","Deaths")) # set names 
cv_total <- cv[cv$Deaths %>% length,] # get total count
cv <- cv[-length(cv$Deaths),] # rm total from country df

# remove white space from chars
cv$Cases <- cv$Cases %>% str_replace(" ","") %>% as.numeric()
cv$Deaths <- cv$Deaths %>% str_replace(" ","") %>%  as.numeric()

# remove duplicate entries
cv[cv$Country=="Japan",c("Cases","Deaths")] <- cv[cv$Country=="Japan",c("Cases","Deaths")] %>% as.numeric + cv[cv$Country=="Cases on an international conveyance Japan",c("Cases","Deaths")] %>% as.numeric
cv <- cv[!cv$Country=="Cases on an international conveyance Japan",] # remove japan duplicate
# rename countries for getting centroid later 
cv[stringr::str_which(cv$Country,"Korea"),"Country"] <- "South Korea" 
cv[stringr::str_which(cv$Country,"Iran"),"Country"] <- "Iran"
cv[stringr::str_which(cv$Country,"Maced"),"Country"] <- "Macedonia"

# get totals per continent ## not run 24-2-20  
# cv_continent_cases <- cv %>% filter(Country=="") %>% select(Cases)
# cv_continent_deaths <- cv %>% filter(Country=="") %>% select(Deaths)
# cv_continent_cases$Continent <- cv[,"Continent"] %>% unique
# cv_continent_deaths$Continent <- cv[,"Continent"] %>% unique

# remove empty country rows
# cv <- cv[!cv$Country=="",] 

# subset
cv_country <- cv$Country
cv_cases <- cv$Cases %>% as.numeric()
cv_deaths <- cv$Deaths %>% as.numeric()
cv_total_cases <- cv_total$Cases
cv_total_deaths <- cv_total$Deaths

# get geocode \ rgeos rworldmaps
lonlat <- getMap(resolution="low") %>% # get country lonlats from database
  gCentroid(byid=TRUE) %>% 
  as.data.frame 
lonlat$Country <- rownames(lonlat) # add country col
colnames(lonlat) <- c("Lon", "Lat","Country") # rename cols
lonlat <- lonlat[cv_country,] # match country lonlat 
lonlat  %>%   # write to dir
  readr::write_csv("cv_lonlat.csv")

# add lonlat to df
cv[,c("Lon","Lat")] <- lonlat[,c("Lon","Lat")]
if((any(lonlat$Country == cv$Country)!=TRUE)){# check country name with latlon
  cat("\n\n\nCheck country lonlat before plotting\n\n\n")}

# fix malaysia latlon
cv[cv$Country=="Malaysia",c("Lon","Lat")] <- c(101.975769,4.210484)
# fix palestine latlon
cv[cv$Country==cv[stringr::str_which(cv$Country,"Pales"),"Country"],c("Lon","Lat")] <- cv %>% filter(Country=="Israel") %>% select(c("Lon","Lat")) + 0.05 # displace Palestine latlon from israel
  
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
colv <- "#F90F40"
colv2 <- "#FA0303"
opac <- 0.7
colvec_cases <- ifelse(cv_cases > 0, colv,NaN) # get colvec w/o nafta cases
colvec_deaths <- ifelse(cv_deaths > 0 ,colv2,NaN) # remove 0 points


# set colvec for if removing nafta polylines # not run 3-3-20
# nafta_cases <- cv %>% filter(Country %in% nafta_string) %>% select("Cases") %>% unlist
# colvec_cases <- ifelse(cv_cases %in% nafta_cases,NaN,colv) # get colvec w/o nafta cases
# colvec_deaths <- ifelse(cv_deaths %in% c(0,nafta_cases),NaN,colv2) # remove 0 points 

# add deaths latlon manually # not run 3-2-20
# cv_deaths_lon <- cv %>% filter(Deaths>0) %>% select("Lon") %>% unlist
# cv_deaths_lat <- cv %>% filter(Deaths>0) %>% select("Lat") %>% unlist


# text --------------------------------------------------------------------

# title 
ttl <- paste0("<div 
              style=\"color:#F90F40;\"> 
              2019-nCov 
              </div>","global distribution")

# tr
heading_tr <- paste(sep = "<br>",
                    "<strong> Total cases </strong>", cv_total_cases,
                    "",
                    "<strong> Total deaths </strong>", cv_total_deaths)

# bl
heading_bl <- paste(sep = "<br>",
                    "Data source: <a href=https://www.ecdc.europa.eu/en/geographical-distribution-2019-ncov-cases> 
                    ECDC
                    </a>",
                    "Last data scrape: ", Sys.time(),
                    "",
                    "Github: <a href=https://github.com/darwinanddavis/worldmaps> @darwinanddavis </a>")

# labels ## not run 
label_cases <- paste(
  "<strong> Continent: </strong>", cv$Continent, "<br/>"
) %>% map(htmltools::HTML)


# popups
popup_cases <- paste(sep = "<br/>",
                     "<strong> Country: </strong>", cv_country,
                     "",
                     "<strong> Cases: </strong>", cv_cases,
                     ""
)

popup_deaths <- paste(sep = "<br/>",
                      "<strong> Country: </strong>", cv_country,
                      "",
                      "<strong> Deaths: </strong>", cv_deaths,
                      ""
)

# style options -----------------------------------------------------------

# css
map_title <- tags$style( # title 
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
max_bound1 <- c(-150,90)
max_bound2 <- c(180,-90)

# layers ------------------------------------------------------------------

# titles
layer1 <- "Cases"
layer2 <- "Deaths"

# point size
radius_cases <- sqrt(cv_cases) * 5000 
radius_deaths <- sqrt(cv_deaths) * 5000

# map ---------------------------------------------------------------------

# set arc matrix
cvm <- gcIntermediate(lonlat_matrix[1,],
               lonlat_matrix,
               n=100,
               addStartEnd=T,
               breakAtDateLine = T,
               sp=T
) %>% 
  leaflet() %>% 
  setMaxBounds(max_bound1[1],max_bound1[2],max_bound2[1],max_bound2[2]) %>% 
  setView(lonlat[1,1],lonlat[1,2],zoom=min_zoom) %>% 
  addTiles(custom_tile,
           options = providerTileOptions(minZoom=min_zoom, maxZoom=max_zoom)
           ) %>% 
  addProviderTiles(custom_tile, 
                   group = c(layer1,layer2),
                   options = providerTileOptions(minZoom=min_zoom, maxZoom=max_zoom)
                   ) %>% 
  addPolylines(color=colvec_cases, # cases
               opacity = opac,
               weight = 1,
               group = layer1) %>%
  addPolylines(color=colvec_deaths, # deaths
               opacity = opac,
               weight = 2,
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
  # addLabelOnlyMarkers(nafta_lon,nafta_lat, # add labels for cases outside of polylines
  #                     label=nafta_string,
  #                     labelOptions = text_label_opt_nafta,
  #                     group=layer1) %>% 
  # addLabelOnlyMarkers(death_lon,death_lat, # add labels for deaths outside of polylines
  #                     label=cv_deaths_labels,
  #                     labelOptions = text_label_opt_nafta,
  #                     group=layer2) %>% 
  addLayersControl( 
    baseGroups = c(layer1,layer2),
    options = layer_options) %>% 
  hideGroup(layer2) %>% 
  addControl(title, "bottomleft", className = "map-title") %>% 
  addControl(heading_bl,"bottomleft") %>%
  addControl(heading_tr, "topright") 

cvm

# save outputs ------------------------------------------------------------
last.warning; geterrmessage() # get last warning and error message

cvm %>% saveWidget(here("Data/worldmaps/coronavirus.html"))
cvm %>% saveWidget(here("Data/worldmaps/worldmaps/coronavirus.html")) # save to dir 


