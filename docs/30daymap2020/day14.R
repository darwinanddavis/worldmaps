# --------------------------------------------------------------------
# 30 day map challenge 2020
# day 14
# author: Matt Malishev
# @darwinanddavis  

# pkgs --------------------------------------------------------------------
pacman::p_load(ggfortify,dplyr,here,foreign,rgdal,sp,sf,mapdata,patchwork,stringr,readr,purrr,ggplot2,ggthemes,ggnetwork,elevatr,raster,colorspace,ggtext,ggsn,ggspatial,showtext,ggtext)

# data --------------------------------------------------------------------
# read df and shp data 
url <- "https://dataverse.harvard.edu/file.xhtml?persistentId=doi:10.7910/DVN/O8GOHP/QZT3YQ#"
v <- here::here("worldmaps","data","day14.shp") %>% st_read()
bb <- v$geometry %>% attr(which = "bbox")
v %>% names
v <- v %>% filter(Cas_R | Cff_R | Csh_r==1) # filter by coffee, cashew, cassava, 

# filter data -------------------------------------------------------------
coff <- v["vul_coffee"] # get item
cash <- v["vul_cash"]
cass <- v["vul_cass"]

# /mapdata
countries <- c("Vietnam","Cambodia","Laos","Malaysia","Thailand","China","Myanmar","Philippines","Taiwan","Brunei")
wm <- map_data("world",region = countries)

# add font
newfonts <- "/Fonts/ADAYWITHOUTSUN.otf"
fontlib <- "adaywithoutsun"
font_add(fontlib,regular = newfonts,bold = newfonts)
showtext_auto(enable = T) # auto showtext

# style -------------------------------------------------------------------
colv <- "#E3E9EA"
bg <- "#FFFFFC"  
border <- "#FFFFFF"
colv_na <- "black"
family <- "A DAY WITHOUT SUN"
opac <- 0.7
size_t <- 25 # title size
ldf <- data.frame("name" = c("Coffee","Cashew","Cassava"),
                  "colour" = c("#893816","#C8B272","#4B953C"),
                  "pal" = c("YlOrBr","RedOr","Mint"),
                  "x"= rep(bb[3]+2.5,3),
                  "y" = rep(bb[2]+1,3))

ttl <-"The climate risk of crops in Vietnam" %>% str_to_upper()
caption <- paste0("Matt Malishev | @darwinanddavis\n Risk measured by climate change representative concentration pathway (8.5 2050), exposure to natural hazards, poverty rate (Gini coefficient), health care, infrastructure, organisational capacity, and education\n Data: International Center for Tropical Agriculture - CIAT")

# plot func 
plotv <- function(dd,name){
  colnames(dd) <- c("id","geometry") # change col names
  colvec <- sequential_hcl(dd[,1] %>% nrow,palette=ldf[ldf$name==name,"pal"]) %>% rev
  b <- dd %>% st_drop_geometry() # get breaks for legend 
  bks <- c(b %>% min,b %>% unlist %>% median,b %>% max)
  ggplot() +
    geom_polygon(data=wm,aes(long,lat,group=group),col=colvec %>% tail(1), fill=alpha(colv,opac), size = 0.2) +
    geom_sf(data = dd, 
            aes(fill = id),
            colour = border, size=0.01, show.legend = T) +
    coord_sf(xlim=c(bb[1],bb[3]+1.5),ylim=c(bb[2],bb[4])) + # bbox
    scale_fill_gradientn(colours = colvec,
                         na.value = colv_na,
                         breaks=bks,
                         labels = c("Low","Medium","High") %>% str_to_upper,
                         guide = "colourbar",
                         name = ldf[ldf$name==name,"name"] %>% str_to_upper) +
    geom_richtext(data=ldf,aes(x,y), show.legend = F,
                  label = ldf[ldf$name==name,"name"] %>% str_to_upper(),
                  family = family, color=colvec %>% tail(1), size = size_t/2.5) +
    theme_blank() +
    theme(
      plot.background = element_rect(fill = bg),
      panel.background = element_rect(fill = bg, color = colvec %>% tail(1)),
      legend.background = element_rect(fill=alpha(colv, opac)),
      legend.box.background = element_rect(fill=alpha(border, opac), color = border),
      legend.title = element_text(family = family,colour = colvec %>% tail(1),size = size_t/2),
      legend.text = element_text(family = family,colour = colvec %>% tail(1),size = size_t/2.5),
      legend.key.size = unit(0.25, "cm"),
      legend.position=c(0.25,0.4), # xy from bottomleft
      plot.margin=unit(rep(0.25,4),"cm")
    )
}

# plots
cf <- plotv(coff,"Coffee") 
ch <- plotv(cash,"Cashew")
cs <- plotv(cass,"Cassava")

# combine 
m14 <- cf + ch + cs + 
  plot_annotation(title = ttl,caption = caption,
                  theme = theme(plot.background = element_rect(fill = bg, color = bg),
                                plot.title = element_text(family = family,size = size_t),
                                text = element_text(family = family,size = size_t/1.5)))
m14

# save --------------------------------------------------------------------
ggsave(here::here("worldmaps","img","day14.jpg"),m14, device = "jpg", dpi = "retina", width = 30, height = 30, units = "cm")




