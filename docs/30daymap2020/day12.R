# --------------------------------------------------------------------
# 30 day map challenge 2020
# day 10
# author: Matt Malishev
# @darwinanddavis  

# pkgs --------------------------------------------------------------------
pacman::p_load(dplyr,readr,RCurl,png,jpeg,mapdata,rvest,xml2,magrittr,sp,ggplot2,stringr,ggthemes,ggnetwork,colorspace,ggtext,ggsn,ggspatial)



# data --------------------------------------------------------------------

d <- map_data("world","japan") # \mapdata
d <- d %>% filter(subregion=="Honshu")

# map ---------------------------------------------------------------------

img <- "https://m.media-amazon.com/images/M/MV5BM2ZiZTk1ODgtMTZkNS00NTYxLWIxZTUtNWExZGYwZTRjODViXkEyXkFqcGdeQXVyMTE2MzA3MDM@._V1_UX182_CR0,0,182,268_AL_.jpg"
imgr <- img %>% getURLContent %>% readJPEG

bg <- "black"

p <- ggplot() +
  annotation_raster(imgr, -Inf, Inf, -Inf, Inf) +
  geom_polygon(data=d,aes(long,lat,group=group),
               fill="transparent",col="black",size=0.2,
               rule = "evenodd") + 
  theme_blank() + 
  theme(panel.background = element_rect(fill = bg, colour = bg), 
        plot.background = element_rect(fill = bg)) + 
  theme(legend.key = element_rect(fill = bg)) 
p


# 2 -----------------------------------------------------------------------

# getting sample pictures
require(tidyr)
# converting raster image to plottable data.frame
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

imgr_df <- 
  ggplot_rasterdf(imgr, 
                  left = min(d[d$group==30,]$long ), 
                  right = max(d[d$group==30,]$long),
                  bottom = min(d[d$group==30,]$lat),
                  top = max(d[d$group==30,]$lat))


gr_A_df <- imgr_df[point.in.polygon(imgr_df$X, imgr_df$Y, 
                                 d[d$group==30,]$long, 
                                 d[d$group==30,]$lat) %>% as.logical,]

p <- ggplot() + 
  geom_polygon(data=d, aes(x=long, y=lat, group=group))
p

p + geom_tile(data = gr_A_df, aes(x = X, y = Y), fill = gr_A_df$color) + 
   theme_bw()


# 4 -----------------------------------------------------------------------

plot(d$long,d$lat,type="n")
for(g in unique(d$group)){
  ifile= img %>% getURLContent %>% readJPEG
  x=d$long[d$group == g]
  y=d$lat[d$group == g]
  xmin=mean(x)-sd(x)*2
  ymin=mean(y)-sd(y)*2
  xmax=mean(x)+sd(x)*2
  ymax=mean(y)+sd(y)*2
  rasterImage(ifile,xmin,ymin,xmax,ymax)
}
plot.new()
par(mar=rep(0,4))

# 3 -----------------------------------------------------------------------


# https://stackoverflow.com/questions/32848145/heat-map-with-image-as-background
ggplot(d, aes(long,lat))  + 
  annotation_raster(imgr, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) +
  stat_density2d(geom = "polygon", aes(fill=..level..)) + 
  geom_point(size=0.5) +
  scale_fill_gradient(low="green",high="red") + 
  scale_x_continuous(limits=c(0,dim(imgr)[2]),expand=c(0,0))+
  scale_y_continuous(limits=c(0,dim(imgr)[1]),expand=c(0,0))+
  coord_fixed()
  
