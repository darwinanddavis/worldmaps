pacman::p_load(here,dplyr,ggplot2,readr,ggthemes,colorspace,sf,rnaturalearth,lubridate,svglite,rgdal,stringr,rvest,xml2,stringi,tidyr)

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

adl <- adlib %>% str_split_fixed("•",Inf) %>% 
  stri_remove_empty(na_empty = F) %>% 
  str_trim(side = "both") %>% 
  strsplit(artist) %>% 
  purrr::map(str_remove_all,artist) %>% 
  purrr::map(unique) 
(lnames <- adl[adl %>% purrr::map(length) == 2] %>%
  purrr::map(1) %>% 
  unlist)
names(adl) <- adl %>% purrr::map(length) == 2

ifelse(names(adl)=='TRUE', names(adl) <- artist, names(adl) <- NA)

adl[adl %>% purrr::map(length) == 2] %>% head
adlib %>% View


adl %>% unlist %>% str_which(lnames)

df <- tibble(
  "ablib" = adl,
  "split" = adl %>% purrr::map(length) ==2
)
df %>% filter(split == TRUE) 


# convert each distinct value in var2 into new column while maintaining var1
require(reshape2)
require(tidyr)
df %>% dcast(adlib ~ split, fill = 0)  # fill NAs with 0
df %>% dcast(. ~ factor(adlib, levels = unique(adlib)), drop = T)  # retain row order when converting to cols 

df %>% melt() %>% tidyr::uncount(value)  # lengthen df by each row value

# transpose
df %>% t %>% data.frame

adl[755] %>% str_detect(artist[209])
index <- adl %>% purrr::map(str_which,artist) %>% unlist



# data clean  ---------------------------------------------------------
clean <- c("uck")


