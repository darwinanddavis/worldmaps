pacman::p_load(here,ggplot2,lubridate,stringr, dplyr,tibble,readr,palmerpenguins,plotly,htmlwidgets,cowplot,gridExtra,colorspace,readxl,sf,htmltools)

here::here("jips_test")
# data ----------------------------------------------------------
df <- here::here("jips_test","jips_data.xlsx") %>% readxl::read_xlsx()
# df <- here::here("data.txt") %>% read_lines()
df <- df %>% rename(.cols = 3,
              "Score" = 3)
top20 <- df %>%
  filter(top20 == "x")
bottom20 <- df %>%
  filter(bottom20 == "b")

# clean data ----------------------------------------------------

# summarise
df %>% 
  summarise("Mean" = mean(Score,na.rm = T),
            "Counts" = n()
  )

# get distinct count per grouped var
df %>% group_by(Year) %>% 
  summarise("n" = n_distinct(Score),
            "Mean" = mean(Score,na.rm = T)) 

# mutate
df %>% 
  mutate("new_beaks" = case_when(
    bill_length_mm < 40 ~ colpal[2],
    bill_length_mm > 50 ~ colpal[3],
    T ~ colpal[1]
  ))

# nas -----------------------------------------------------------

# get total NAs
sapply(df, function(x) sum(is.na(x)))
df %>% sapply(is.na) %>% sum 

df[is.na(df)] <- 0
df[is.nan(df)] <- 0 # good for matrices 
df$col1[which(!is.finite(df$col1))] <-  NA # Replace `NaN` and `Inf` values with `NA` 
data <- data[!is.na(data$X),] # rm all rows with nas
global_output <- rapply(global_output, f=function(x) ifelse(is.na(x),0,x), how="replace")  # Turn NaN or NAs in list into 0s  





# . -------------------------------------------------------------
# . -------------------------------------------------------------
# plot params --------------------------------------------------------

width <- 15
height <- 15
units <- "cm"
my_save <- function(fh) ggsave(here::here("plots") %>% paste0("/",fh,".png"), dpi = "retina", width = width, height = height, bg = "transparent", units = units, limitsize = F)

my_theme <- function(){
  list(theme_classic(),
       labs(x = xlab, y = ylab),
       theme(
         panel.background = element_rect(fill = "transparent"), # bg of the panel
         plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
         panel.grid.major = element_blank(), # get rid of major grid
         panel.grid.minor = element_blank(), # get rid of minor grid
         text = element_text(size = 10, face = "bold", color  = "#000000"),
         legend.background = element_rect(fill = "transparent"), # get rid of legend bg
         legend.box.background = element_rect(fill = "transparent", color = NA),
         legend.key.size = unit(0.3, "cm"),
         # legend.position = c(0.9,0.9), # xy from bottom left
         legend.title = element_text(size = 8),
         legend.text = element_text(size = 8),
         plot.margin=unit(rep(0.15,4),"cm"),
         panel.ontop = F),
       labs(title = ttl, subtitle = subttl,x = xlab, y = ylab,caption = caption)
  )
}
  

  
# bar chart -----------------------------------------------------
xlab <- "Country"
ylab <- "Protection Score"
ttl <- "Global human rights protection scores"  
subttl <- "Per country from 1980 to 2017"
caption <- "Data: JIPS.org | Matt Malishev | @darwinanddavis"



ranked <- df %>% 
  group_by(Year) %>% 
  mutate("Rank" = Score %>% sort
  )

colpal <- sequential_hcl(df$Year %>% unique %>% length,
                         "Purple-Orange")
opac <- 0.7
ggplot() +
  geom_col(data = ranked, 
           aes(Country, Rank, fill = Year %>% as.factor(), col = Year %>% as.factor())) +
  coord_flip() +
  facet_wrap(~Year,nrow = 1) +
  scale_fill_manual(name = "Year", values = adjustcolor(colpal,opac), aesthetics = c("col","fill")) +
  my_theme() 


fh <- "p1"
my_save(fh) # save to dir 





# box -----------------------------------------------------------

xlab <- "Year"
ylab <- "Protection Score"
ttl <- "Human rights protection scores per year"  
subttl <- "Summary for 1980 to 2017"
caption <- "Data: JIPS.org | Matt Malishev | @darwinanddavis"


colpal <- sequential_hcl(df$Year %>% unique %>% length,
                         "Purple-Orange")
opac <- 0.7

require(ggdist)
p3 <- ggplot(data = df, 
             aes(x = Year, y = Score, group = Year,
                 col = Year %>% as.factor(), fill = Year %>% as.factor()
             )) +
  geom_boxplot(outlier.color = NA) +
  coord_flip() +
  geom_jitter(height = 0.05) + # jitter points
  # scale_y_discrete(limits=limits) + # reorder xaxis
  scale_colour_manual(name = "Year", values = colpal,aesthetics = "col") +
  scale_fill_manual(name = "Year", values = adjustcolor(colpal,0.7), aesthetics = "fill") +
  my_theme()

p3
fh <- "p3"
my_save(fh) # save to dir 




# ridges 
xlab <- "Protection Score"
ylab <- "Country"
ttl <- "Distribution of protection scores per country"  
subttl <- "Data from 1980 to 2017"
caption <- "Data: JIPS.org | Matt Malishev | @darwinanddavis"

colpal <- sequential_hcl(df$Score %>% unique %>% length,
                         "SunsetDark")

# try positive/negative colpal 
df <- df %>% 
  mutate("Col" = case_when(
    Score < 0 ~ colpal[1],
    Score < 0 ~ colpal[length(colpal)],
    T ~ colpal[length(colpal)/2]
  ))

p2 <- ggplot() + 
  ggridges::geom_density_ridges_gradient(data = df, 
                                         aes(x = Score, y = Country, 
                                             fill = Score),
                                         col = "#EFEFEF",
                                         scale = 5, size=0.2, 
                                         rel_min_height = 0.01,
                                         panel_scaling=T, show.legend = F) + 
  # facet_wrap(~Year) +
  scale_fill_manual(values = adjustcolor(colpal,0.7), aesthetics = c("col","fill")) +
  my_theme()
p2

# scatter -------------------------------------------------------
xx <- df$Country
yy <- df$Score

ggplot() +
  geom_point(data = df, aes(Country,Score, fill = Year %>% as.factor(), col = Year %>% as.factor())) +
  my_theme() +
  scale_fill_manual(values = adjustcolor(colpal,opac), aesthetics = c("col","fill")) 

fh <- "p1"
my_save(fh) # save to dir 


# hist ----------------------------------------------------------


ggplot() + 
  geom_histogram(data = df,aes(flipper_length_mm, group = species, fill = species), binwidth = 2) +
  my_theme()

fh <- "p1"
my_save(fh) # save to dir 


# density -------------------------------------------------------
require(ggridges)


ggplot() +
  geom_density(data = df, 
               aes(x = Score, y = ..density.., 
                   col = Year %>% as.factor(), fill = Year %>% as.factor())) +
  facet_wrap(~Year %>% as.factor()) +
  scale_fill_manual(values = adjustcolor(colpal,opac), aesthetics = c("col","fill")) +
  my_theme()




# polar
colpal <- sequential_hcl(df$Country %>% unique %>% length,
                         "BuGn")
ggplot() +
  geom_histogram(data = df, 
                 aes(Score, fill = Country), col = "#FFFFFF", 
                 size = 0.1,
                 position = "stack", show.legend = F) + # normal polar
  scale_fill_manual(values = colpal, aesthetics = "fill", guide = F) + # normal
  facet_wrap(~Year) +
  coord_polar() +
  my_theme()

    


# arrange plots in grid -----------------------------------------

# put plots into list 
gglist <- list(p,p1,p2)

# 3 plots above, 1 below
gridExtra::grid.arrange(
  grobs = gglist, # list with ggplots or grobs
  widths = c(1, 1, 1),
  layout_matrix = rbind(c(1, 2, 3),
                        c(4, 4, 4))
)


# plotly --------------------------------------------------------
pp <- p %>% ggplotly(tooltip = c("body_mass_g","bill_length_mm"),
         dynamicTicks = T)

pp <- p1 %>% ggplotly()

# two plotly objects 
pp <- subplot(pp, pp, nrows = 1, 
              shareX = T, shareY = T,titleX = T)

fh <- "p1"
pp %>% htmlwidgets::saveWidget(here::here("plots",paste0(fh,".html")))




# maps ----------------------------------------------------------
colpal <- sequential_hcl(6,"ag_GrnYl")
colv <- colpal[1]
scales::show_col(colpal)

# world data
require(mapdata) # high res data
data(world.cities) # /maps
world_cities <- world.cities 

# get country names  
library(rnaturalearth)
library(rnaturalearthdata)
require(leaflet)  
require(geosphere) # for flight arc paths 

ic <- df$Country %>% unique
mp <- ne_countries(scale = "medium", returnclass = "sf") %>% 
  filter(name %in% ic) %>% 
  rename("Country" = name) %>% 
  left_join(df,by = "Country") %>% 
  mutate("Col" = case_when(
    Score < -3 ~ colpal[1],
    Score < -2 ~ colpal[2],
    Score <= 0 ~ colpal[3],
    Score > 0 ~ colpal[4],
    Score > 2 ~ colpal[5],
    Score > 3 ~ colpal[6],
    T ~ colpal[6]
  ))

pal <- colorNumeric(
  palette = colpal,
  domain = mp$Score
)

# leaflet
setview <- c(0,0)
custom_tile <- "http://d.sm.mapstack.stamen.com/((darkmatter,$00ffff[hsl-color]),(mapbox-water,$00589c[hsl-color]),(parks,$ff9a30[source-in]))/{z}/{x}/{y}.png"
par(bg="black")
opac <- 1
mp_scores <- mp$Score
mp_names <- mp$Country

# style

# text labels 
style <- list(
  "color" = colv,
  "font-weight" = "normal",
  "padding" = "8px"
)
layer_options <- layersControlOptions(collapsed = F)
text_label_opt <- labelOptions(noHide = F, direction = "top", textsize = "15px",
                               textOnly = F, opacity = 0.7, offset = c(0,0),
                               style = style, permanent = T
)

# titles
ttl <- paste0("<div style=\"color:",colv,";\"> 
              Human rights protection scores 
              </div>","by country (1980-2017)")

map_title <- tags$style( 
  HTML(".leaflet-control.map-title { 
       transform: translate(-50%,-20%);
       position: fixed !important;
       left: 50%;
       text-align: center;
       padding-left: 10px; 
       padding-right: 10px; 
       background: white; opacity: 0.5;
       font-size: 25px;
       }"
       ))

title <- tags$div(
  map_title, HTML(ttl)
)  

# bl
heading_bl <- paste0(
  "Data source: <a style=color:",colv,"; href=www.jips.org>JIPS.org</a><br>
  Author: <a style=color:",colv,"; href=https://darwinanddavis.github.io/DataPortfolio/> Matt Malishev </a><br>
  Twitter/Github: <a style=color:",colv,"; href=https://github.com/darwinanddavis/worldmaps/tree/gh-pages> @darwinanddavis </a><br>
  Spot an error? <a style=color:",colv,"; href=https://github.com/darwinanddavis/worldmaps/issues> Submit an issue </a>"
)

# tr
heading_tr <- paste(
  "<strong> Total countries <div style=\"color:",colv,"; font-size:150%\">",mp$Country %>% unique %>% length,"</div> </strong>", "<br/>",
  "<strong> Years evaluated <div style=\"color:",colv,"; font-size:150%\"> 1980-2017 </div> </strong>","<br/>"
)

# easy buttons 
locate_me <- easyButton( # locate user
  icon="fa-crosshairs", title="Zoom to my position",
  onClick=JS("function(btn, map){ map.locate({setView: true}); }"));

reset_zoom <- easyButton( # reset zoom 
  icon="fa-globe", title="Reset zoom",
  onClick=JS("function(btn, map){ map.setZoom(3);}"));  

map_control_box <- htmltools::tags$style( 
  htmltools::HTML(".leaflet-control.layers-base { 
       text-align: left;
       padding-left: 10px; 
       padding-right: 10px; 
       background: white; opacity: 1;
       font-size: 20px;
       }"
       ))

control_box <- htmltools::tags$div(
  map_control_box, htmltools::HTML("")
)  





# layers
mp1 <- mp %>% filter(Year == 1980)
mp2 <- mp %>% filter(Year == 1990)
mp3 <- mp %>% filter(Year == 2000)
mp4 <- mp %>% filter(Year == 2010)
mp5 <- mp %>% filter(Year == 2017)

map <- leaflet() %>% 
  setView(setview[1],setview[2],zoom=2) %>% 
  addTiles(custom_tile) %>% 
  addPolygons(data =  mp1, opacity = opac,color = ~Col,fillColor = ~Col,stroke = TRUE,weight = 0.5, 
              # popup=paste0("<br>",mp1$Score,"<br>"),
              popup = paste0("<strong>",mp1$Country,"</strong><br/><br/>","<strong> Protection score </strong><br/><span style=color:",mp$Col,";>", mp1$Score,"</span><br/>") %>% purrr::map(htmltools::HTML),
              label=paste(mp1$Country), 
              labelOptions = text_label_opt, popupOptions = text_label_opt,
              group = "1980"
  ) %>%  
  addPolygons(data =  mp2, opacity = opac,color = ~Col,fillColor = ~Col,stroke = TRUE,weight = 0.5, 
              popup = paste0("<strong>",mp2$Country,"</strong><br/><br/>","<strong> Protection score </strong><br/><span style=color:",mp$Col,";>", mp2$Score,"</span><br/>") %>% purrr::map(htmltools::HTML),
              label=paste(mp2$Country), 
              labelOptions = text_label_opt, popupOptions = text_label_opt,
              group = "1990"
  ) %>%  
  addPolygons(data =  mp3,opacity = opac,color = ~Col,fillColor = ~Col,stroke = TRUE,weight = 0.5, 
              popup = paste0("<strong>",mp3$Country,"</strong><br/><br/>","<strong> Protection score </strong><br/><span style=color:",mp$Col,";>", mp3$Score,"</span><br/>") %>% purrr::map(htmltools::HTML),
              label=paste(mp3$Country), 
              labelOptions = text_label_opt, popupOptions = text_label_opt,
              group = "2000"
  ) %>%  
  addPolygons(data =  mp4,opacity = opac,color = ~Col,fillColor = ~Col,stroke = TRUE,weight = 0.5, 
              popup = paste0("<strong>",mp4$Country,"</strong><br/><br/>","<strong> Protection score </strong><br/><span style=color:",mp$Col,";>", mp4$Score,"</span><br/>") %>% purrr::map(htmltools::HTML),
              label=paste(mp4$Country), 
              labelOptions = text_label_opt, popupOptions = text_label_opt,
              group = "2010"
  ) %>%  
  addPolygons(data =  mp5,opacity = opac,color = ~Col,fillColor = ~Col,stroke = TRUE,weight = 0.5, 
              popup = paste0("<strong>",mp5$Country,"</strong><br/><br/>","<strong> Protection score </strong><br/><span style=color:",mp$Col,";>", mp5$Score,"</span><br/>") %>% purrr::map(htmltools::HTML),
              label=paste(mp5$Country), 
              labelOptions = text_label_opt, popupOptions = text_label_opt,
              group = "2017"
  ) %>%  
  addProviderTiles(
    "CartoDB.DarkMatter"
    # "Stamen.TonerLite"
  ) %>% 
  addLegend(pal = pal,
            values  = mp$Score,
            position = "bottomright",
            title = "Protection score",
            opacity = opac) %>% 
  addLayersControl(
    baseGroups = c("1980","1990","2000","2010","2017"),
    options = layer_options) %>% 
  addControl(title, "bottomleft", className = "map-title") %>% 
  addControl(heading_bl,"bottomleft") %>%
  addControl(heading_tr, "topright") %>% 
  addControl(control_box, "topright", className = "layers-base") %>% 
  addEasyButton(reset_zoom) %>% 
  addEasyButton(locate_me) 
map

# save
df %>% saveRDS(here::here("jips_test","jips_df.Rda"))  
mp %>% saveRDS(here::here("jips_test","jips_mp.Rda"))  
map %>% saveWidget(here::here("plots","jips.html"))  


