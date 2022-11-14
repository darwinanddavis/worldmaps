##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                            #30daymapchallenge2022                        ----
##                                day 4 - green                             ~~~~
##                            author: Matt Malishev                         
##                               @darwinanddavis                            
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

fh <- "day4"
citation <- "https://senseable.mit.edu/treepedia/"

# pcks ----------------------------------------------------------
pacman::p_load(here,ggplot2,stringr,dplyr,tibble,readr,readr,sf,sfheaders,raster,ggmap,colorspace,concaveman,ggspatial,colorspace,ggspatial)



# df <- here::here("data","30daymap2022","day4") %>% list.files("geojson", full.names = T) %>% 
#   read_sf()

# data ----------------------------------------------------------
dr <- "/Users/malishev/Documents/Data/worldmaps/30daymapchallenge/2022/day4/data"
labels <- fhs %>% str_remove_all(paste0(dr,"/greenview")) %>% str_remove_all("_|.json|.geojson")
fhs <-  dr %>% list.files(full.names = T)
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



# plot ----------------------------------------------------------
fg <- "#23351C"
bg <- "#D3D6ED" %>% lighten(0.3)
opac <- 0.5

my_theme <- theme(
  plot.background = element_rect(fill = bg),
  panel.background = element_rect(fill = bg),
  text = element_text(color = fg),
  legend.background = element_rect(fill = bg),
  axis.text = element_text(color = fg),
  panel.grid.major = element_line(color = "transparent"),
  panel.grid.minor = element_line(color = "transparent"),
  margin(0,0,0,0,"pt")
)


# sort by area
df <- slist_boundaries %>% arrange(area) 
buff <- df %>% top_n(n = 1, wt = area) %>% st_buff(1) # set buff to equal largest city


mm <- reshape2::melt(slist_points[1:4])

ggplot() +
  geom_sf(data = slist_boundaries[1:4,], aes(group = id,  col = colpal, fill = fg %>% adjustcolor(opac)),size = 0.2) +
  geom_sf(data = slist_boundaries %>% filter(id == "kobe"), size = 0.2, col = fg, fill = fg %>% adjustcolor(opac)) +
  geom_sf(data = slist_points[[1]], size = 0.05, col = fg) +
  ggspatial::annotation_scale(location = 'bl',bar_cols = c(bg,fg), line_width = 0.5, pad_x = unit(0.5, "cm"), pad_y = unit(0.1, "cm"), text_col = fg, line_col = fg, style = "bar" , width_hint = 0.1) +
  facet_wrap(~id) +
  # coord_sf(xlim = bb[c(1,3)], ylim = bb[c(1,3)]) +
  my_theme + 
  theme_nothing()

# convert to spdf
spp <- slist_boundaries[1:4,] %>% as("Spatial")
ggplot() +
  geom_spatial_tile(data = spp) + # 
  facet_wrap(~id, scales = "free")

# tmap
tm_shape() +
  tm_polygons('attacks', title = 'PKO targeting\nevents (logged)') +
  tm_facets('NAME_0')

slist_boundaries[6:8,] %>%
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
