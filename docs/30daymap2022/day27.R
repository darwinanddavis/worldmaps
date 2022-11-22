pacman::p_load(here,dplyr,ggplot2,readr,ggthemes,colorspace,sf,rnaturalearth,lubridate,svglite,rgdal,stringr,rvest,xml2,stringi)

url <- "https://genius.com/Rap-genius-ad-libs-explained-lyrics"
string_rm <- ("\\(Please add in alphabetical order, first by artist, then by ad-lib, to maintain a clean and concise page\\)")
artist <- url %>% read_html() %>% html_nodes("b") %>% html_text
adlib <- url %>% read_html() %>% 
  html_nodes(".YYrds") %>% 
  html_text %>% 
  str_remove_all(string_rm) 


# rm artist names from adlibs
"Zack de la Rocha• Come on• UghZillaKami• Aye, hold up• Boom-boom-boom-boom• Huh?• Na na na na na• What's up?• Yeah" %>% 
  str_remove_all(artist[209])

adlib %>% str_split_fixed("•",Inf) %>% 
  stri_remove_empty(na_empty = F) %>% 
  str_trim(side = "both") %>% 
  strsplit(artist) %>% 
  purrr::map(str_remove_all,artist) %>% 
  purrr::map(unique) 







"RollieWestside Gunn" %>% setdiff("Westside Gunn")

# option 2
url %>% read_html() %>% html_nodes("a")

# create df
tibble(
  "adlib" = adlib
  "artist" = artist
)



url %>% read_html() %>% html_nodes(":contains('•')") %>%
  .[c(19,20)] %>% 
  html_text


# data clean  ---------------------------------------------------------
clean <- c("uck","igga","ussy","Bitch","bitch","dick","Dick")


