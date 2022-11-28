##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                            #30daymapchallenge2022                        ----
##                                day 26 - islands                             ~~~~
##                            author: Matt Malishev                         
##                               @darwinanddavis                            
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

fh <- "day26"
citation <- "https://senseable.mit.edu/treepedia/"

# pcks ----------------------------------------------------------
pacman::p_load(here,ggplot2,stringr,dplyr,tibble,readr,readr,sf,sfheaders,raster,ggmap,colorspace,concaveman,ggspatial,colorspace,ggspatial,scales,foreign,ggtext,showtext,patchwork)

slist_boundaries <- readRDS(here::here("data","30daymap2022","day4_boundaries.Rda"))
slist_points <-  readRDS(here::here("data","30daymap2022","day4_points.Rda"))

# data ----------------------------------------------------------
dr <- here::here("data","30daymap2022")
fhs <-  dr %>% list.files(full.names = T)
labels <- fhs %>% str_remove_all(paste0(dr,"/greenview")) %>% str_remove_all("_|.json|.geojson")
prj <- 4326
slist_points <- st_sfc(crs = prj)
slist_boundaries <- st_sfc(crs = prj)
for(f in seq_along(fhs)){
  s <- fhs[f] %>% read_sf() %>% dplyr::select(geometry) 
  ss <- s %>% concaveman(concavity = 1) %>% 
    mutate("id" = labels[f])
  slist_points <-  c(s,slist_points) # read and save points
  slist_boundaries <- rbind(ss,slist_boundaries) # read and save boundaries
}
slist_points <- slist_points %>% purrr::map(st_sf) # convert elements to sf
names(slist_points) <- labels %>% rev
slist_boundaries <- slist_boundaries %>% 
  mutate("area" = st_area(slist_boundaries)
  )

# save to dir
slist_boundaries %>% saveRDS(here::here("data","30daymap2022","day4_boundaries.Rda"))
slist_points %>% saveRDS(here::here("data","30daymap2022","day4_points.Rda"))

# add font
newfonts <- "/Fonts/DK Lemon Yellow Sun.otf"
fontlib <- "dklemon"
font_add(fontlib,regular = newfonts,bold = newfonts)
showtext_auto(enable = T) # auto showtext

# plot ----------------------------------------------------------
fg <- "#DAF7A6"
fg2 <- "#5B8017"
bg <- "#031200"
colt <- "transparent"
hue <- 0.1
border <- bg %>% lighten(0.2)
opac <- 0.5
c(fg,fg2,bg,border) %>% scales::show_col()
family <- fontlib
width <- 15; height <- width
size <-  45

my_theme <- theme(
  plot.background = element_rect(fill = colt, color = colt),
  panel.background = element_rect(fill = colt, color = colt),
  text = element_text(family = family, color = fg),
  legend.title = element_text(family = family,colour = fg),
  legend.text = element_text(family = family,colour = fg),
  legend.background = element_rect(fill = colt),
  axis.text = element_text(color = colt),
  axis.ticks = element_line(color = colt),
  panel.grid.major = element_line(color = colt),
  panel.grid.minor = element_line(color = colt),
  margin(0,0,0,0,"pt")
)

# colpal
colv <- colorRampPalette(colors = c(fg, fg2))
colpal <- colv(df_bound$id  %>% n_distinct())
colpal %>% show_col()

# sort by area
df_bound <- slist_boundaries %>% arrange(area) %>% 
  mutate("colpal" = colpal)
df_point <- slist_points[df_bound$id]
buff <- df_bound %>% top_n(n = 1, wt = area) %>% st_buffer(1) %>% suppressWarnings() # set buff to equal largest city

for(nid in seq_along(df_bound$id)){
  df1 <- df_bound[nid,]
  df2 <- df_point[[nid]]
  ttl <- df1$id
  p <- ggplot() +
    geom_sf(data = df1, aes(col = colpal[nid] %>% darken(hue), fill = colpal[nid] %>% darken(hue) %>% adjustcolor(opac)), size = 0.2) +
    geom_sf(data = df2, size = 0.001, col = colpal[nid]) +
    scale_fill_manual(values = colpal[nid], aesthetics = "col") + scale_fill_manual(values = adjustcolor(colpal[nid],opac), aesthetics = "fill") +
    ggspatial::annotation_scale(location = 'bl', bar_cols = c(colt,colpal[nid]), line_width = 0.3, pad_x = unit(0.5, "cm"), pad_y = unit(0.1, "cm"), text_col = colpal[nid], line_col = colpal[nid], style = "bar" , width_hint = 0.1, text_family = family, text_face = "bold") +
    my_theme  +
    theme_nothing() %>% suppressWarnings()
  p <- p + plot_annotation(
    subtitle = ttl %>% str_replace_all("-"," ") %>% str_to_upper(),
    theme = theme(plot.background = element_rect(fill = colt, color = colt),
                  plot.title = element_text(family = family,size = size, color = colpal[nid], vjust = 1, hjust = 1),
                  text = element_text(family = family,size = size,color = colpal[nid]))) 
  ggsave(paste0("plot",ttl,".png"),p,device = "png", width = width, height = height, units = "cm", dpi = "retina", bg = colt)
}
