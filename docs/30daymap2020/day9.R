# --------------------------------------------------------------------
# 30 day map challenge 2020
# day 9
# author: Matt Malishev
# @darwinanddavis  

# pkgs --------------------------------------------------------------------
pacman::p_load(dplyr,readr,rvest,xml2,magrittr,ggplot2,stringr,ggthemes,ggnetwork,elevatr,raster,colorspace,ggtext,ggsn,ggspatial,magick,cowplot)

# data --------------------------------------------------------------------
site <- here::here("worldmaps","data","day9_site.Rda") %>% readRDS
rrdf_ <- here::here("worldmaps","data","day9_raster.Rda") %>% readRDS

plat <- 0 # 34.866215 # lat for proj centre 
plon <- 0 # -84.326248 # lon for proj centre
pname <- "longlat" # "aeqd"
zoom <- 6
prj <- paste0("+proj=",pname," +lat_0=",plat," +lon_0=",plon," +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
# prj <- "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
rrdf <- get_elev_raster(site,z = zoom, prj, expand = 5) %>%  # get elev raster
  rasterToPoints() %>% # convert to df
  as.data.frame  

# transform data
rrdf_ <- rrdf %>% 
  mutate_at(vars(layer), funs(sqrt))
rrdf_$layer[is.nan(rrdf_$layer)] <- 0 # change nan to 0 

# labels
bg <- "#802A07"
colv <- "#FFFFFF"
alpha <- 0.7
hjust <- c(0.5,1.1,0.1,0.2,0.5)
vjust <- c(1.1,0.5,1,1.1,1)*1.3
size <- 1.6
site_size <- 1.2
ttl_size <- 4
credits_size <- 1.6
family <- "Cinzel"

ttl <- data.frame("lon" = site[5,"lon"]+5,
                  "lat" = site[5,"lat"]-8,
                  "main" = "Scaling the <br> Appalachian Trail")

credits <- data.frame("lon" = ttl[1,"lon"]+3.5,
                      "lat" = ttl[1,"lat"]-8,
                      "main" = "**Matt Malishev <br> @darwinanddavis**")


# map
p <- ggplot(data=rrdf_) +
  geom_raster(aes(x,y,fill=layer),show.legend = F) +
  scale_fill_gradientn(name = "Elevation",
                       colors = sequential_hcl(9,"Oranges")) +
  geom_point(data=site,aes(lon,lat),shape=24,size=size,colour = colv,fill=colv, alpha=0.8) +
  geom_richtext(data=site,aes(lon,lat,label=sites, # add sites
                              hjust=hjust,vjust=vjust,
                              size=site_size,family = family),
                color=bg, size = size, fill = colv, label.color = NA,alpha = alpha) +
  geom_richtext(data=ttl,aes(lon,lat,label=main, # add title
                             size=ttl_size,family = family),
                color=colv, size = ttl_size, fill = NA, label.color = NA,alpha = 1) +
  geom_richtext(data=credits,aes(lon,lat,label=main, # add credits
                                 size=credits_size,family = family),
                color=colv, size = credits_size, fill = NA, label.color = NA,alpha = 1) +
  theme_blank() +
  theme(
    plot.margin=unit(rep(0,4),"cm"),
    panel.background = element_rect(fill = bg), 
    plot.background = element_rect(fill = NULL, color = bg), 
    legend.background = element_rect(fill = bg),
    legend.box.background = element_rect(fill = bg)
  ) +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_minimal(
                           line_width = 0,
                           line_col = colv,
                           fill = colv,
                           text_col = colv,
                           text_family = family,
                           text_face = NULL,
                           text_size = 1.5
                         ))
p <- p + ggsn::scalebar(data = site,
                        x.min = -76, x.max = -75.5,
                        y.min = 29, y.max = 28.5,
                        dist = 500, st.size=3, height=0.5,dist_unit = "km", 
                        location = "bottomleft",
                        # st.bottom = T, st.color = bg, 
                        transform = T, model = "WGS84")
p

# save --------------------------------------------------------------------
ggsave(here::here("worldmaps","img","day9__.png"),p, device = "png", width=15,height=15,units = "cm", dpi="screen")

