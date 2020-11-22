# --------------------------------------------------------------------
# 30 day map challenge 2020
# day 16
# author: Matt Malishev
# @darwinanddavis  
# og code
# https://stackoverflow.com/questions/28206611/adding-custom-image-to-geom-polygon-fill-in-ggplot

# pkgs --------------------------------------------------------------------
pacman::p_load(dplyr,readr,tidyr,rnaturalearth,rnaturalearthdata,sf,raster,png,jpeg,plyr,cowplot,mapdata,rvest,xml2,magrittr,sp,ggplot2,stringr,ggthemes,ggnetwork,colorspace,ggtext,ggsn,ggspatial)

# get map poly --------------------------------------------------------------------
d <- map_data("world","japan") # \mapdata
hon <- d$group[which(d$subregion == "Honshu")][1] # choose polygon 

# img ---------------------------------------------------------------------

# totoro
imgr <- here::here("worldmaps","img","day16_img.png") %>%  readPNG()
imgr2 <- here::here("worldmaps","img","day16_img2.png") %>%  readPNG()

# akira
imgr <- here::here("worldmaps","img","akira.jpg") %>%  readJPEG()
imgr2 <- here::here("worldmaps","img","akira2.jpg") %>%  readJPEG()

# convert raster img to plottable df
ggplot_rasterdf <- function(color_matrix, bottom = 0, top = 1, left = 0, right = 1) {
  if (dim(color_matrix)[3] > 3) hasalpha <- T else hasalpha <- F
  outMatrix <- matrix("#00000000", nrow = dim(color_matrix)[1], ncol = dim(color_matrix)[2])
  for (i in 1:dim(color_matrix)[1])
    for (j in 1:dim(color_matrix)[2]) 
      outMatrix[i, j] <- rgb(color_matrix[i,j,1], color_matrix[i,j,2], color_matrix[i,j,3], ifelse(hasalpha, color_matrix[i,j,4], 1))
  colnames(outMatrix) <- seq(1, ncol(outMatrix))
  rownames(outMatrix) <- seq(1, nrow(outMatrix))
  as.data.frame(outMatrix) %>% mutate(Y = nrow(outMatrix):1) %>% gather(X, color, -Y) %>% 
    mutate(X = left + as.integer(as.character(X))*(right-left)/ncol(outMatrix), Y = bottom + Y*(top-bottom)/nrow(outMatrix))
}

# get bounds of imported img 
imgr_df <- ggplot_rasterdf(imgr, 
                  left = min(d[d$group==hon,]$long), 
                  right = max(d[d$group==hon,]$long),
                  bottom = min(d[d$group==hon,]$lat),
                  top = max(d[d$group==hon,]$lat))

# determine if points fall within polygon 
imgr_dfp <- imgr_df[point.in.polygon(imgr_df$X, imgr_df$Y, 
                                    d[d$group==hon,]$long , 
                                    d[d$group==hon,]$lat) %>% as.logical(),]

# totoro
colv <- "#090613"
bg <- "#9F9986"
family <- "Bauhaus 93"
xx <- 140
yy <- 25
cs <- 2

# akira
colv <- "#C6D4D7"
bg <- "#14192C"
family <- "Bauhaus 93"
xx <- 140
yy <- 25
cs <- 1.5

cred <- "Matt Malishev<br>@darwinanddavis"

# map 
m16 <- ggplot() + 
  geom_polygon(data=d, aes(x=long, y=lat, group=group),colour=colv, fill = colv) +
  geom_tile(data = imgr_dfp, aes(x = X, y = Y), fill = imgr_dfp$color) + 
  draw_image(imgr2,  x = xx, y = yy, scale = 10) + # totoro
  geom_richtext(aes(xx+1,yy-0.5),label=cred, family = family, show.legend = F,color=colv, size = cs, fill = NA, label.color = NA) +
  # draw_image(imgr2,  x = xx, y = yy, scale = 4) + # akira 
  # geom_richtext(aes(xx+0.5,yy-1.5),label=cred, family = family, show.legend = F,color=colv, size = cs, fill = NA, label.color = NA) +
  theme_blank() + 
  theme(panel.background = element_rect(fill = bg, colour = bg), 
        plot.background = element_rect(fill = bg, colour = colv))
m16
ggsave(here::here("worldmaps","img","day16.png"),m16,device = "png",width = 9,height = 11, units = "cm")


