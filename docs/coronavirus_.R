# 2019-nCov distribution 

# web scraped data from the european centre for disease control
# you will need a google api to generate new latlons for any new countries added to the online database
# otherwise, you can watch the live updates here

# up to date link ---------------------------------------------------------
# https://darwinanddavis.github.io/worldmaps/airbnb_sf.html

# . -----------------------------------------------------------------------
# . -----------------------------------------------------------------------

# register google api 
api <- "your api key"
register_google(api)

# packages ----------------------------------------------------------------
# install.packages("pacman")
require(pacman)
p_load(maps,dplyr,leaflet,xml2,rvest,ggmap,geosphere,htmltools,mapview,rnaturalearth,purrr)

# read data ---------------------------------------------------------------
# scrape data from web \xml2
url <- "https://www.ecdc.europa.eu/en/geographical-distribution-2019-ncov-cases"
web_data <- url %>% read_html

# convert to tibble \rvest
tb <- web_data %>% html_table 
cv <- tb[[1]] # get df
cv <- setNames(cv,c("Continent","Country","Cases","Deaths")) # set names 
cv_total <- cv[cv$Deaths %>% length,] # get total count
cv <- cv[-length(cv$Deaths),] # rm total from country df

# subset
cv_country <- cv$Country
cv_cases <- cv$Cases
cv_deaths <- cv$Deaths
cv_total_cases <- cv_total$Cases
cv_total_deaths <- cv_total$Deaths

# get latlons from google api \ ggmap
# lonlat <- ggmap::geocode(cv_country) # run once

# read in datafile  
lonlat <- readr::read_csv("cv_lonlat.csv") # get lonlat from dir 
cv[,c("Lon","Lat")] <- lonlat # add to df
cv %>% str

# get numeric
lon <- lonlat[1][[1]]
lat <- lonlat[2][[1]]

# get matrix for poly arcs (omits nafta)
lonlat_matrix <- cv %>% 
  filter(Continent!="America") %>% 
  select(c(Lon,Lat)) %>% 
  unlist %>% 
  matrix(ncol=2)

# get country polygons \naturalearth
cv_polygons <- ne_countries(country=cv_country,returnclass = "sp")

# style -------------------------------------------------------------------
custom_tile <- names(providers)[113] # choose tiles
custom_tile2 <- names(providers)[110]
colv <- "#F90F40"
colv2 <- "#FA0303"
opac <- 0.7
colvec_deaths <- ifelse(cv_deaths == 0,"#090909",colv2) # remove 0 points 

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

# style options -----------------------------------------------------------

# text label options 
text_label_opt <- labelOptions(noHide = F, direction = "top", textsize = "15px",
                               textOnly = F, opacity = 0.5, offset = c(0,0),
                               style = style, permanent = T
)

# layer options 
layer_options <- layersControlOptions(collapsed = F)

# layers ------------------------------------------------------------------

# titles
layer1 <- "Cases"
layer2 <- "Deaths"

# point size
radius_cases <- sqrt(cv_cases) * 5000 
radius_deaths <- sqrt(cv_deaths) * 5000


# map ---------------------------------------------------------------------

# set arc matrix
gcIntermediate(lonlat_matrix[1,],
               lonlat_matrix,
               n=100,
               addStartEnd=T,
               breakAtDateLine = T,
               sp=T
) %>% 
  leaflet() %>% 
  setView(lonlat[1,1],lonlat[1,2],zoom=4) %>% 
  addTiles(custom_tile) %>% 
  addProviderTiles(custom_tile, group = c(layer1,layer2)) %>% 
  addPolylines(color=colv, # cases
               opacity = opac,
               weight = 1,
               popup = popup_cases,
               group = layer1) %>%
  addPolylines(color=colvec_deaths, # deaths 
               opacity = opac,
               weight = 1,
               popup = popup_cases,
               group = layer2) %>%
  addCircles(lon, lat, # cases
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
  addLayersControl(
    baseGroups = c(layer1,layer2),
    options = layer_options) %>% 
  hideGroup(layer2) %>% 
  addControl(title, "bottomleft", className = "map-title") %>% 
  addControl(heading_bl,"bottomleft") %>%
  addControl(heading_tr, "topright")


