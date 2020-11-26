# creds -------------------------------------------------------------------
# airbnb shiny app 
# https://darwinanddavis.shinyapps.io/airbnb/

# Matt Malishev 
# @darwinanddavis
# matthew.malishev[at]gmail.com 

# pcks --------------------------------------------------------------------

# install.packages('rsconnect')
require(shiny)
require(shinythemes)
require(dplyr)
require(readr)
require(leaflet)
require(colorspace)
require(leaflet.extras)
require(reshape2)
require(stringr)
require(scales)
require(rsconnect)
require(htmltools)
require(here)
require(RColorBrewer)
require(tidyr)
require(purrr)
require(lubridate)
require(metathis)
require(shinycssloaders)

server_dims <- "auto" 
colvec <- c("Sunset-Dark","Inferno","Brwn-Yl","Burg","Teal")

# funcs ---------------------------------------------------------------------
# webscrape func
url <- "http://insideairbnb.com/get-the-data.html"
web_info <- function(node,node2){
  # pull just first row of table  
  if(node2==""){  
    url %>% read_html %>% html_nodes(node) %>% html_text(trim = T)
  }else{
    url %>% read_html %>% html_nodes(node) %>% html_node(node2)
  }
}

# criteria to subset from df 
criteria_candidates <- c(
  "Bed type",
  "Room type",
  "Property type",
  "Bathrooms",           
  "Cancellation policy",
  "Reviews per month",
  "Review scores rating",
  "Security deposit",    
  "Cleaning fee", 
  "Accommodates"
)

# read in web df
fwd <- "https://github.com/darwinanddavis/worldmaps/blob/gh-pages/docs/shiny/airbnb/fwd.Rdata?raw=true" %>% url %>% readRDS
city_names <- fwd$city 
city_urls <- fwd$url
shinyServer(function(input, output){
  # get url for user city 
  url_read <- reactive({
    fwd[fwd$city %>% str_which(input$select_city),"url"] %>% unlist # get url
  })
  # get base data --------------------------------------------------------------
  get_airbnb <- reactive({
    airbnb <- url_read() %>% read_csv
    # clean data 
    cols_clean <- airbnb %>% names %>% str_to_title() %>% str_replace_all("_", " ") # make cols title case 
    colnames(airbnb) <- cols_clean 
    # change char to num
    airbnb <- airbnb %>% mutate(`Price` = `Price` %>% str_sub(2) %>% str_trim("both") %>% as.numeric,
                                `Cleaning fee` = `Cleaning fee` %>% str_sub(2) %>% str_trim("both") %>% as.numeric,
                                `Security deposit` = `Security deposit` %>% str_sub(2) %>% str_trim("both") %>% as.numeric,
                                `Cancellation policy` = `Cancellation policy` %>% str_to_title() %>% str_replace_all("_", " ")
    ) %>% 
      select(`Host name`, # subset data 
             `Host url`,
             Longitude, Latitude,
             Price, Bedrooms, Neighbourhood, 
             `Review scores rating`,
             `Review scores location`,
             `Review scores cleanliness`,
             `Review scores communication`,
             `Review scores accuracy`,
             criteria_candidates) # add criteria candidates 
    airbnb <- airbnb %>% mutate_at("Neighbourhood",replace_na,"NA") # replace NAs in neighbourhood
    airbnb <- airbnb %>% # round off data 
      mutate_at("Security deposit",funs(plyr::round_any(.,100))) %>% 
      mutate_at("Cleaning fee",funs(plyr::round_any(.,100))) %>% 
      mutate_at("Accommodates",funs(plyr::round_any(.,1))) %>% 
      mutate_at("Reviews per month",funs(plyr::round_any(.,5))) %>% 
      mutate_at("Review scores rating",funs(plyr::round_any(.,10))) %>% 
      mutate_at("Bathrooms",funs(plyr::round_any(.,1))) 
    airbnb <- airbnb %>% mutate_all(replace_na, 0) # replace NAs
    airbnb
  })
  
  # map tile
  custom_tile <- "Esri.WorldGrayCanvas"
  
  # set all user input changes to action button in ui
  # subset data based on user input 
  map_event <- eventReactive(input$make_map,{
    input_price <- input$price
    input_bedroom <- input$bedrooms
    # filter by criteria data
    get_airbnb() %>% filter(Price >= input$price[1] & Price <= input$price[2],
                            Bedrooms == input_bedroom)
  },ignoreNULL = T)
  
  # map latlon
  map_points <- eventReactive(input$make_map,{
    map_event() %>% # use filtered data
      select(Longitude, Latitude) # to get latlon
  },ignoreNULL = T)
  
  # user criteria 
  criteria_output <- eventReactive(input$make_map,{
    map_event() %>%
      pull(input$select_criteria)
  },ignoreNULL = T)

  # no of entries 
  entries <- eventReactive(input$make_map, {
    criteria_output() %>% length %>% format(big.mark=",",scientific = F,trim = T)
  })
  
  # # col pal reactive 
  col_pal <- eventReactive(input$make_map,{
    input_col <- criteria_output() %>% # pull user criteria 
      as.factor %>% as.integer() %>% # turn into num for col pal
      na.omit() # rm nas
    colv <- colorRampPalette(sequential_hcl(input_col %>% length, input$colpal))
    colv_vec <- colv(input_col %>% length)[as.numeric(cut(input_col, breaks = length(input_col)))]  # define breaks in col gradient
    colv_vec[is.na(colv_vec)] <- "black" # change NA to black
    colv_vec 
    # colorNumeric(colv, input_col) # user for leaflet proxy to convert to leaflet color func
  },ignoreNULL = T)
  
  # for col pal as layers
  col_pal_2 <- eventReactive(input$make_map,{
    input_col <- criteria_output() %>% # pull user criteria 
      as.factor %>% as.integer() %>% # turn into num for col pal
      na.omit() # rm nas
    colv <- colorRampPalette(sequential_hcl(input_col %>% length, "Viridis"))
    colv
  },ignoreNULL = T)
  
  # popup event
  popup_event <- eventReactive(input$make_map,{
    host_name <- map_event() %>% 
      pull(`Host name`) # host name 
    host_url <- map_event() %>% 
      pull(`Host url`) # url
    neighbourhood <- map_event() %>% 
      pull(Neighbourhood) # hood
    rating_overall <- map_event() %>% 
      pull(`Review scores rating`) # overall rating
    rating_location<- map_event() %>% 
      pull(`Review scores location`) # location
    rating_cleanliness<- map_event() %>%
      pull(`Review scores cleanliness`) # cleanliness
    rating_communication <- map_event() %>%
      pull(`Review scores communication`) # comm
    rating_accuracy <- map_event() %>%
      pull(`Review scores accuracy`) # accuracy 
    href <- paste0("<strong><a href=",host_url,">",host_name,"</a></strong>") # web link
    site_names <- paste(
      "<div style=\"font-size:20px;\"> ","<br/>",href,"</div>","<br/>",
      "<strong> Neighbourhood </strong>","<br/>",neighbourhood,"<br/>",
      "<br/>", 
      "<strong> Ratings </strong>","<br/>",
      "Overall: ",rating_overall,"<br/>",
      "Location: ",rating_location,"<br/>",
      "Cleanliness: ",rating_cleanliness,"<br/>",
      "Communication: ",rating_communication,"<br/>",
      "Accuracy: ",rating_accuracy,"<br/>",
      "<br/>", 
      "<strong>",input$select_criteria,"</strong>", "<br/>", 
      criteria_output() # print user criteria 
    )
  },ignoreNULL = T)
  
  # price label
  price_label <- eventReactive(input$make_map,{
    map_event() %>%
      pull(Price) # get price from filtered data 
  })
  
  # criteria label
  criteria_label <- eventReactive(input$make_map,{
    label <- criteria_output() # use filtered user criteria data 
    # paste0(input$select_criteria,": ",label)
    paste0(
      "<div style=\"font-size:15px;\">", 
      "<strong>",input$select_criteria,"</strong>",": ",label,
      "<br/>",
      "<strong>Price</strong>",": $",price_label(), # use filtered price data 
      "<br/>",
      "</div>"
    ) %>% map(htmltools::HTML) # turn into html for styling 
  },ignoreNULL = T)
  
  # legend title 
  legend_title <- eventReactive(input$make_map,{
    map_event() %>%  
      select(input$select_criteria) %>% names
  },ignoreNULL = T)
  
  # style -------------------------------------------------------------------
  
  # current colpal
  opac <- 0.7
  font_size <- 40
  
  # style
  style <- list(
    "color" = "black",
    "font-size" = "10px",
    "font-weight" = "normal",
    # "font-family" = "Optima",
    "padding" = "5px 5px"
  ) 
  # label options
  text_label_opt <- labelOptions(noHide = F, direction = "top",
                                 textOnly = F, opacity = 1, offset = c(0,0),
                                 style = style, permanent = T
  )
  
  # easy buttons
  locate_me <- easyButton( # locate user
    icon="fa-crosshairs", title="Zoom to my position",
    onClick=JS("function(btn, map){ map.locate({setView: true}); }"));
  
  reset_zoom <- easyButton( # reset zoom
    icon="fa-home", title="Reset zoom",
    onClick=JS("function(btn, map){ map.setZoom(10);}"));
  
  # pulse icons
  marker_pulse <- makePulseIcon(
    color = "orange",
    iconSize = 5,
    animate = T,
    heartbeat = 0.05
  )
  pulse_options <- markerOptions(opacity=0.5, riseOnHover = T, interactive = T)
  
  # layer options
  layer_options <- layersControlOptions(collapsed = F)
  min_zoom = 3
  max_zoom = 16

  # map ---------------------------------------------------------------------
  
  observeEvent(input$make_map, { # watch action button
    if(is.null(input$select_criteria)){ # if criteria empty
      output$user_message <- renderText({"Select criteria to map"})
    }else{
      sc <- 1 # count if web data is loaded 
      output$user_message <- renderText({""})
      output$map <- renderLeaflet({
        leaflet(options = leafletOptions(worldCopyJump = T)) %>%
          # setView(zoom=12) %>%
          addTiles(#custom_tile,
            options = providerTileOptions(minZoom=min_zoom, maxZoom=max_zoom)) %>%
          addProviderTiles("CartoDB",
                           options = providerTileOptions(minZoom=min_zoom, maxZoom=max_zoom)) %>%
          addCircles(data = map_points(),
                     radius = 5,
                     color = col_pal(),
                     fill = col_pal(),
                     fillColor = col_pal(),
                     weight = 1,
                     opacity = opac,
                     fillOpacity = opac,
                     label = criteria_label(),
                     popup = popup_event(),
                     labelOptions = text_label_opt) %>%
          addEasyButton(reset_zoom) %>%
          addEasyButton(locate_me) %>%
          addLegend("bottomright",title = legend_title(),
                    colors = col_pal() %>% unique %>% sort,
                    labels = criteria_output() %>% unique %>% sort)
      })
    } # end else
  }) # end observe action button
  
  # print outputs 
  output$select_city <- renderText({
    if(input$select_city %>% str_length() > 1){
      paste0("for ",input$select_city)
    }else{
      "2020"
    }})
  output$bedrooms <- renderText({input$bedrooms})
  output$price <- renderText({paste0("$",input$price[1],"—$",input$price[2])})
  output$select_criteria <- renderText({input$select_criteria})
  output$entries <- renderText({entries()})
  output$date <- renderText({
    if(input$select_city %>% str_length() > 1){
      # paste0(fwd[fwd$city %>% str_which(input$select_city),"month"] %>% unlist %>% as.character(),", 2020") # add month
    }else{
      "2020"
    }})
  
}) # end server 
