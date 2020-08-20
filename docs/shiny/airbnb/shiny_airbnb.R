pacman::p_load(readr,dplyr,rvest,DataExplorer,purrr,stringr,lubridate,magrittr)

# https://www.datacamp.com/community/tutorials/r-web-scraping-rvest

# TO DO 
# either 1) make a download most recent data button in ui or 2) after first city is mapped, save urls/month/city/country data to file by creating counter  

# funcs -------------------------------------------------------------------
# webscrape func
web_info <- function(node,node2){
  # pull just first row of table  
  if(node2==""){  
    url %>% read_html %>% html_nodes(node) %>% html_text(trim = T)
  }else{
    url %>% read_html %>% html_nodes(node) %>% html_node(node2)
  }
  # url %>% read_html %>% html_table(trim = T) %>% map(info) # html_table version 
}

# read url ----------------------------------------------------------------
url <- "http://insideairbnb.com/get-the-data.html"

# get most recent url, date, city, and country for each dataset
dl <- web_info("table","a") %>% html_attr("href") # url
dm <- web_info("table","td") %>% html_text %>% dmy %>% month(label = T,abbr = F) # month
dc <- web_info("h2","") %>% str_split_fixed(", ",4) # split string into city and country
dc[str_which(dc[,4],""),3] <- dc[str_which(dc[,4],""),4] # replace missing countries
city <- dc[,1] # city
country <- dc[,3] # country

list(dl)

# server ------------------------------------------------------------------
fwd <- tibble("url"=dl,"city"=paste0(city,", ",country))


# find string in url name 
city %>% str_to_lower() %>% str_replace_all(fixed(" "),"-")
dl %>% str_match(city %>% str_to_lower() %>% str_replace_all(fixed(" "),"-"))





# get df
ad <- web_data[[1]] 
ad[is.na(ad)] <- 0 # rm nas



#  old ver ----------------------------------------------------------------

web_data <- url %>% read_html() %>% html_table(trim = T)  
web_data %>% map(names) %>% unique
url %>% read_html %>% html_nodes("table") %>% map("Date Compiled")
web_info("Date Compiled")  # html_table version 

# get city
web_data %>% map("Country/City") %>% map(1)
city_country <- url %>% read_html %>% html_nodes("h2") %>% html_text(trim = T) %>%
  str_split_fixed(", ",4) # split string into city and country and replace spaces with "-"
# replace missing countries
city_country[str_which(city_country,"")][38] 
city_country[str_which(city_country[,4],""),3] <- city_country[str_which(city_country[,4],""),4]
city <- city_country[,1]
country <- city_country[,3]
