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
# dr <- here::here("data","30daymap2022")
dr <- "/Users/malishev/Documents/Data/worldmaps/30daymapchallenge/2022/day26/data"
fhs <-  dr %>% list.files(full.names = T)
labels <- fhs %>% str_remove_all(paste0(dr,"/greenview")) %>% str_remove_all("_|.json|.geojson")
prj <- 4326
slist_points <- st_sfc(crs = prj)
# slist_boundaries <- st_sfc(crs = prj)
for(f in fhs){
  s <- f %>% read_sf() %>% dplyr::select(geometry) 
  # ss <- s %>% concaveman(concavity = 1)
  slist_points <-  c(s %>% st_as_sf(),slist_points) # read and save points
  # slist_boundaries <- rbind(ss,slist_boundaries) # read and save boundaries
}
slist_points <- slist_points %>% purrr::map(st_sf) # convert elements to sf
names(slist_points) <-labels
colpal <- sequential_hcl(slist_boundaries %>% n_distinct(),"Dark Mint")
slist_boundaries <- slist_boundaries %>% mutate("id" = labels,
                                                "area" = st_area(slist_boundaries),
                                                "colpal" = colpal
                                                )
# save to dir
slist_boundaries %>% saveRDS(here::here("data","30daymap2022","day4_boundaries.Rda"))
slist_points %>% saveRDS(here::here("data","30daymap2022","day4_points.Rda"))

# add font
newfonts <- "/Fonts/ADAYWITHOUTSUN.otf"
fontlib <- "adaywithoutsun"
font_add(fontlib,regular = newfonts,bold = newfonts)
showtext_auto(enable = T) # auto showtext

# plot ----------------------------------------------------------
fg <- "#DAF7A6"
fg2 <- "#5B8017"
bg <- "#23351C"
border <- bg %>% lighten(0.2)
opac <- 0.5
c(fg,fg2,bg,border) %>% scales::show_col()
family <- fontlib
width <- 15; height <- width
size <-  40

my_theme <- theme(
  plot.background = element_rect(fill = bg, color = "transparent"),
  panel.background = element_rect(fill = bg, color = "transparent"),
  text = element_text(family = family, color = fg),
  legend.title = element_text(family = family,colour = fg),
  legend.text = element_text(family = family,colour = fg),
  legend.background = element_rect(fill = bg),
  axis.text = element_text(color = "transparent"),
  axis.ticks = element_line(color = "transparent"),
  panel.grid.major = element_line(color = "transparent"),
  panel.grid.minor = element_line(color = "transparent"),
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
buff <- df_bound %>% top_n(n = 1, wt = area) %>% st_buffer(1) # set buff to equal largest city

df_bound[3,] %>% st_bbox

# df_bound %>% 
#   mutate("x" =  bb[1]+1.5,
#          "y" =  bb[2]+1.5
#          ) 

for(nid in df_bound$id[1:3]){
p <- ggplot() +
  # geom_sf(data = slist_boundaries, aes(group = id,  col = colpal, fill = fg %>% adjustcolor(opac)),size = 0.2) +
  geom_sf(data = df_bound %>% filter(id == nid), aes(col = colpal %>% factor, fill = colpal %>% factor %>% adjustcolor(opac),group = id), size = 0.2) +
  geom_sf(data = df_point[[nid]], size = 0.001, col = fg ) +
  ggspatial::annotation_scale(location = 'bl',bar_cols = c("transparent",fg), line_width = 0.3, pad_x = unit(0.5, "cm"), pad_y = unit(0.1, "cm"), text_col = fg, line_col = fg, style = "bar" , width_hint = 0.1, text_family = family,text_face = "bold") +
  # geom_sf_label(data=df_bound %>% filter(id == nid), aes(label=id %>% str_to_upper),family=family, color=fg, size = size, label.size = 0, fill = NA,nudge_x = -0.02, nudge_y = -0.02) +
  # annotate("text", 0, 0, label = nid,family = family, size = size) +
  # geom_richtext(data=df_bound %>% filter(id == nid),
  #               aes(x,y,label = id %>% str_to_upper()), show.legend = F,
  #               family = family, color=fg, size = 10) +
  # facet_wrap(~id) +
  # coord_sf(xlim = bb[c(1,3)], ylim = bb[c(1,3)]) +
  my_theme  +
  theme_nothing() 
  
  
p <- p +
  plot_annotation(subtitle =  nid %>% str_to_upper(),
                theme = theme(plot.background = element_rect(fill = "transparent", color = "transparent"),
                              plot.title = element_text(family = family,size = size, color = fg,vjust = 1, hjust = 1),
                              text = element_text(family = family,size = size,color = fg))) 

dr2 <- "/Users/malishev/Documents/Data/worldmaps/30daymapchallenge/2022/day26/"
ggsave(paste0(dr2,nid,".png"),p,device = "png", width = width, height = height, units = "cm", dpi = "print")
}


s <- "/Users/malishev/Documents/Data/worldmaps/worldmaps/data/30daymap2022/greenview_tampa.json" %>% 
  read_sf() %>% dplyr::select(geometry) 
ss <- s %>% concaveman(concavity = 1)


ggplot() +
  geom_sf(data = slist_boundaries %>% filter(id == "tampa")) +
  geom_sf(data = slist_points$tampa, size = 0.001, col = fg) +
  my_theme  +
  theme_nothing() 







# .. ------------------------------------------------------------


# convert to spdf
spp <- slist_boundaries[1:2,] %>% as("Spatial")
ggplot() +
  geom_spatial_tile(data = spp) + # 
  facet_wrap(~id, scales = "free")

# tmap
tm_shape() +
  tm_polygons('attacks', title = 'PKO targeting\nevents (logged)') +
  tm_facets('NAME_0')

require(tmap)
slist_boundaries[1:2,] %>% 
  tm_shape() +
  tm_borders(group = "id") +
  # tm_polygons('attacks', title = 'PKO targeting\nevents (logged)') +
  # tm_polygons("area",palette = "BuPu",legend.show = FALSE) +
  tm_facets(by = "id") +
  tm_layout(frame = FALSE, 
            frame.lwd = NA, 
            panel.label.bg.color = NA,
            panel.label.size = 2,
            main.title = "Trail Running generated polygons \nand its covered areas (km2)", 
            main.title.position = "center",
            main.title.size = 3,
            main.title.fontface = "bold") 



samplelist = list(a = data.frame(x=c(1:10), y=rnorm(10)),
                  b = data.frame(x=c(5:10), y=rnorm(6)),
                  c = data.frame(x=c(2:12), y=rnorm(11)))

ggplot() +
  geom_sf(data = bind_rows(slist_points[1:4]))
