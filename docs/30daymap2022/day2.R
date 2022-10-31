##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                            #30daymapchallenge2022                        ----
##                                day 2 - lines                                 ~~~~
##                            author: Matt Malishev                         
##                               @darwinanddavis                            
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# isometric distance calcs and isochrones 
fh <- "isochrone"

# pcks --------------------------------------------------------------------
pacman::p_load(sf,here,dplyr,ggplot2,colorspace,osrm,osmextract,osmdata,data.table,magrittr,sfheaders,mapsf,patchwork,ggtext,maps,stringr)

# get data ----------------------------------------------------------------

# find centroids 
prj <- 4326
site <- "melbourne"
sfcent <- world.cities %>% # get sites 
  filter(country.etc %in% "Australia", name %in%  str_to_title(site)) %>% 
  dplyr::select(name,long,lat) %>% dplyr::rename(.cols = 2, lon = long) %>% 
  sfheaders::sf_point(keep = T, x = "lon", y = "lat") 
st_crs(sfcent) <- prj # set proj
sfpath <- data.frame(id='center', lon=c(sfcent %>% st_coordinates() %>% .[1],sfcent %>% st_coordinates() %>% .[1] +0.5), lat=c(sfcent %>% st_coordinates() %>% .[2],sfcent %>% st_coordinates() %>% .[2]+0.5)) %>% sfheaders::sfc_linestring(x='lon', y='lat') %>% st_as_sf(crs = prj)
sfpoly <- sfcent %>% st_buffer(1000) %>% st_cast("POLYGON")

# osrm  -------------------------------------------------------------------
dir.create(here::here(fh)); dir.create(here::here(fh,"data",site)); dir.create(here::here(fh,"plot",site))

mmin <- 0 # min travel minutes
mmax <- 60 # max travel minutes
mint <- 10 # time break interval 
breaks <- seq(mmin, mmax, mint)
rmode <- "car" # "bike" "foot"
res <-  70  # final res = res * res
iso <- osrmIsochrone(loc = sfcent, breaks = breaks, res = res, osrm.profile = rmode,returnclass = "sf")
iso %>% saveRDS(here::here(fh, "data",site,paste(site,rmode,mmin,mmax,"res",res,".Rda", sep="_")))

# get osm ---------------------------------------------------------------
library(osmdata)
bb <- iso %>% st_bbox()
get_osm <- function(bb,key,value,type) opq(bbox = bb) %>%
  add_osm_feature(key = key, value = value) %>%
  osmdata_sf() %>% .[[type]]

# roads
o1 <- c("motorway","primary","trunk")
o2 <- "secondary"
o3 <- c("tertiary","footpath","sidewalk","pedestrian","living_street")

osm_primary <- get_osm(bb,"highway",o1,"osm_lines")
osm_secondary <- get_osm(bb,"highway",o2,"osm_lines")
osm_tertiary <- get_osm(bb,"highway",o3,"osm_lines")

# save to dir
osm_primary %>% saveRDS(here::here(fh,"data",site,paste("osm_primary.Rda")))
osm_secondary %>% saveRDS(here::here(fh,"data",site,paste("osm_secondary.Rda")))
osm_tertiary %>% saveRDS(here::here(fh,"data",site,paste("osm_tertiary.Rda")))

# get iso lines -----------------------------------------------------------
iso_lines <- iso %>% st_make_valid() %>% st_intersection(dplyr::bind_rows(list(osm_primary,osm_secondary,osm_tertiary))) # get all overlapping roads
iso_lines %>% saveRDS(here::here(fh,"data",site,paste(site,rmode,mmin,mmax,"res",res,"lines.Rda", sep="_"))) # save to dir

# plot --------------------------------------------------------------------
ttl <- paste0("Travel time\n(min) by ", rmode)
leg <- "Isochrones\n(min)"
width = 40
height = 40
opac <- 0.8
cex <- 8
size <- 0.4  
bg <- "#FFFFFF"; fg <- "#e6e6e6"; col_na <- "#82093E"
my_theme <-  theme(panel.grid.major = element_line(colour = NA),
                   panel.grid.minor = element_line(colour = NA),
                   panel.background = element_rect(fill = bg, colour = NA),
                   plot.background = element_rect(fill = bg, colour = NA),
                   axis.text = element_text(colour = bg, size = cex),
                   text = element_text(color = fg, size = cex),
                   legend.background = element_rect(fill = bg, colour = "transparent"),
                   legend.box.background = element_rect(fill = bg, colour = "transparent"))


df <- iso
ttldf <- tibble(label = site %>% str_to_upper() %>% str_sub(0,3),
                x = bb %>% unlist %>% .[1] + 0.01,
                y = bb %>% unlist %>% .[4] - 0.02,
                angle = 90, color = colpal[1], label.color = NA, fill = NA
)

# plot iso polygons
var1 <- "max"
ttldf <- tibble(label = site %>% str_to_upper() %>% str_sub(0,3),
                x = bb %>% unlist %>% .[1] + 0.05,
                y = bb %>% unlist %>% .[4] - 0.1,
                angle = 90, color = colpal[1], label.color = NA, fill = NA
)

colpal <- sequential_hcl(df[var1] %>% n_distinct(), "Dark Mint")
p1 <- ggplot() + 
  geom_sf(data = df, aes(fill = .data[[var1]]),col = NA) +
  geom_sf(data = iso_lines %>% filter(highway %in% o1), aes(col = .data[[var1]], fill = .data[[var1]]),size = size) +
  geom_sf(data = iso_lines %>% filter(highway %in% o2), aes(col = .data[[var1]], fill = .data[[var1]]),size = size/2) +
  geom_sf(data = iso_lines %>% filter(highway %in% o3), aes(col = .data[[var1]], fill = .data[[var1]]),size = size/3) +
  geom_richtext(data = ttldf, aes(x, y, label = label, angle = angle), size = 12, color = colpal %>% unique %>% .[1], label.color = NA, fill = NA, show.legend = F) +
  geom_sf(data = sfcent, col = colpal[1] %>% darken(0.5), fill = col_na, size = 2) +
  ggspatial::annotation_scale(location = 'bl',bar_cols =c(colpal[1]), line_width = 0.5, pad_x = unit(1.5, "cm"), pad_y = unit(0.1, "cm"), text_col = colpal[1], line_col = colpal[1], style = "ticks", width_hint = 0.1) +
  scale_fill_gradientn(name = paste0(ttl,"\n"), colours = adjustcolor(colpal,opac),aesthetics = c("col","fill"), na.value = col_na) +
  ggthemes::theme_map() +
  theme(
    panel.background = element_rect(fill = "transparent", colour = NA),
    plot.background = element_rect(fill =  "transparent", colour = NA),
    legend.background = element_rect(fill="transparent"),
    legend.text = element_text(color = colpal[1])) + 
  coord_sf(xlim = bb[c(1,3)], bb[c(2,4)],crs = prj) +
  labs(title="", colour=ttl,fill=ttl) 
p1

wrap_plots(list(p1,p1,p1,p1), ncol = 2, guides = "collect")
ggsave(here::here(fh,"plot",site, paste(site,rmode,mmin,mmax,"res",res,"_iso.png",sep="_")), device = "png", width = 40, height = 40, units = "cm", dpi = "retina", bg = "transparent")

# plot iso linestrings 
df <- iso_lines # get current data 
ttldf <- tibble(label = site %>% str_to_upper() %>% str_sub(0,3),
                x = bb %>% unlist %>% .[1] + 0.01,
                y = bb %>% unlist %>% .[4] - 0.05,
                angle = 90, color = colpal[1], label.color = NA, fill = NA
)
var1 <- "max"
var2 <- ""
size <- 0.4
colpal <- sequential_hcl(df[var1] %>% n_distinct(), "PinkYl") 
ggplot() + 
  geom_sf(data = df %>% filter(highway %in% o1), aes(col = .data[[var1]], fill = .data[[var1]]),size = size) +
  geom_sf(data = df %>% filter(highway %in% o2), aes(col = .data[[var1]], fill = .data[[var1]]),size = size/2) +
  geom_sf(data = df %>% filter(highway %in% o3), aes(col = .data[[var1]], fill = .data[[var1]]),size = size/3) +
  scale_fill_gradientn(name = paste0(ttl,"\n"), colours = adjustcolor(colpal,opac),aesthetics = c("col","fill"), na.value = col_na) +
  geom_sf(data = sfcent, col = col_na, fill = adjustcolor(col_na,opac), size = 2) +
  geom_richtext(data = ttldf, aes(x, y, label = label, angle = angle), size = 15, color = colpal %>% unique %>% .[200], label.color = NA, fill = NA, show.legend = F) +
  ggspatial::annotation_scale(location = 'bl',bar_cols =c(bg,fg), line_width = 0.5, pad_x = unit(1.5, "cm"), pad_y = unit(0.1, "cm"), text_col = fg, line_col = fg, style = "ticks", width_hint = 0.1) +
  ggthemes::theme_map() +
  theme(legend.background = element_rect(fill="transparent", color = "transparent")) +
  coord_sf(xlim = bb[c(1,3)], bb[c(2,4)],crs = prj) +
  labs(colour=ttl,fill=ttl, x = "", y = "") +
  my_theme 

ggsave(here::here(fh,"plot",site,paste(site,rmode,mmin,mmax,"res",res,".png", sep ="_")), device = "png", width = 30, height = 30, units = "cm", dpi = "retina", bg = "transparent")
