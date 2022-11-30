##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                            #30daymapchallenge2022                        ----
##                                day 30 - remix                             ~~~~
##                            author: Matt Malishev                         
##                               @darwinanddavis                            
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

fh <- "day30"
citation <- "https://worldview.earthdata.nasa.gov/"

# pcks ----------------------------------------------------------

pacman::p_load(here,dplyr,osmdata,sf,sfheaders,raster,geoviz,terra,mapdata)
install.packages("remotes")
remotes::install_github("tylermorganwall/rayshader",force = T)
remotes::install_github("tylermorganwall/rayimage", force = T)
require("rayshader")
require("rayimage")

# raster data -----------------------------------------------------------------
require(raster)
urban_flood_levels <- here::here("data") %>% list.files("urban",full.names = T) %>% read_sf()
ras <- here::here("data","ga_s2am_oa_provisional_3-2-1_49HFB_2022-03-12_nrt_satellite-view.tif") %>% raster(resolution = 2)
prj <- ras %>% projection() # prj of raster 
bb <- urban_flood_levels %>% st_transform(prj) %>% extent %>% as('SpatialPolygons') # get bbox for raster
crs(bb) <- prj # match prj with data
zscale <- geoviz::raster_zscale(ras %>% crop(bb))
res <- 2
ras <- ras %>% crop(bb) %>% raster_to_matrix() %>%
  resize_matrix(res)
ras %>% saveRDS(here::here("data","spatial","3d",paste0("ras_base_",res,".Rda")))

# create base map 
df <- ras
base_map <- df %>%
  height_shade(texture = "white") %>% 
  add_overlay(sphere_shade(df, texture = "bw", colorintensity = 1), alphalayer=0.5) %>%
  add_shadow(lamb_shade(df),0.7) %>%
  add_shadow(ray_shade(df)) %>%
  add_shadow(ambient_shade(df, zscale = 5),max_darken = 0.9) %>% 
  add_shadow(texture_shade(df,detail = 1,contrast = 3,brightness = 17),max_darken = 0.7)
base_map %>% plot_map()
base_map %>% saveRDS(here::here("data","spatial","3d",paste0("ras_base_map",res,".Rda")))

# add flood layers
graphics.off()
ll <- urban_flood_levels$Height_Met %>% unique %>% sort %>% .[1:nn]
river <- river %>% st_transform(crs(bb)) %>% st_make_valid() 
nn <- 8 # property layer gradient
add_layer <- read_data("land_features","Property.shp") %>% # properties
  st_transform(crs(bb)) %>% st_make_valid()
add_layer2 <- urban_flood_levels %>% # urban flood levels 
  st_buffer(0) %>% 
  st_boundary() %>% st_cast("POLYGON") %>% 
  st_transform(crs(bb))
add_layer3 <- readRDS(here::here("data","spatial","property_flood_overlap.Rda")) %>% 
  filter(calc_area <= 35000) %>% 
  st_transform(crs(bb)) %>% st_make_valid()

colv <- sequential_hcl(nn,"Light Grays") %>% lighten(0.5)
colvw <- "steel blue" %>% darken(0.1)

# make polygon fill palette for flood layers and flooded properties
colpal <- function(df,col){
  colv <- sequential_hcl(df[,"Height_Met"] %>% st_drop_geometry() %>% n_distinct(), col) %>% darken(0.2)
  colpal <- c("<=02m" = colv[1],  "02-04m" = colv[2], "04-06m" = colv[3], "06-08m" = colv[4], "08-10m" = colv[5], "10-12m" = colv[6], "12-14m" = colv[7], "14-16m" = colv[8]) 
  colpal %>% return()
}

# generate ray base layers
df_layer <- generate_line_overlay(add_layer, extent = bb %>% extent,linewidth = 0.01, color=colv, heightmap = df) 
river_layer <- generate_polygon_overlay(river, extent = bb %>% extent,palette=colvw, heightmap = df, linewidth = 0)
df_layer_polygon <- generate_polygon_overlay(add_layer, 
                                             palette = "white", # sequential_hcl(nn,"Light Grays") %>% lighten(0.5), 
                                             linewidth = NA,
                                             data_column_fill = "classsubty", extent = bb %>% extent, heightmap = df, linecolor = NA)
df_layer_polygon2 <- generate_polygon_overlay(add_layer2, 
                                              palette = colpal(add_layer2,"Blues"),
                                              linecolor = colpal(add_layer2,"Blues"),
                                              linewidth = 0.3,
                                              data_column_fill = "Height_Met", extent = bb %>% extent, heightmap = df)
df_layer_polygon3 <- generate_polygon_overlay(add_layer3, 
                                              palette = colpal(add_layer3,"Reds"),
                                              linecolor = colpal(add_layer3,"Reds"),
                                              linewidth = 0.3,
                                              data_column_fill = "Height_Met",
                                              extent = bb %>% extent, heightmap = df)
watermap <- detect_water(df,cutoff = 2,max_height = 16) # detect water

# add flood height using transition   
water_palette = colorRampPalette(c("darkblue", "dodgerblue", "lightblue"))(157)
bathy_hs = height_shade(df, texture = water_palette)
flevel <- 10

# plot --------------------------------------------------------------------
graphics.off()
flevel <- 2
base_map %>% 
  add_overlay(river_layer) %>%
  add_overlay(df_layer) %>%
  add_overlay(df_layer_polygon) %>%
  add_overlay(df_layer_polygon2,alphalayer = 0.9) %>%
  add_overlay(df_layer_polygon3, alphalayer = 0.9) %>%
  # add_overlay(generate_altitude_overlay(watermap, df, start_transition = flevel + 6,end_transition = flevel * 2 + 6),alphalayer = 0.7,alphacolor = "steel blue") %>%
  plot_map()

### 3d plot
graphics.off()
rgl::rgl.close()
theta <- 20; phi <- 50; zoom <- 0.5; fov <- 3
depth_water <- 14
fh <- here::here("plot","spatial","3d",paste0("river_polygon_add_lamb_depth_flood_polygon_flood_property",depth_water,"2.png"))

base_map %>% 
  add_overlay(df_layer) %>%
  add_overlay(river_layer) %>%
  add_overlay(df_layer_polygon) %>%
  add_overlay(df_layer_polygon2) %>%
  add_overlay(df_layer_polygon3) %>%
  add_shadow(lamb_shade(df),0.5) %>%
  plot_3d(df, triangulate = F, 
          zscale = 3/1, 
          solid = T,shadow = T,shadowcolor = 'black',baseshape = "rectangle",
          # water = T, waterdepth = depth_water, wateralpha = 0.7, watercolor = colvw ,waterlinecolor = "#FFFFFF",
          # dirt = T,
          solidcolor = colv, solidlinecolor = "black",
          windowsize=c(1200,800)
  )
render_camera(theta=theta,  phi=phi, zoom=zoom,  fov=fov)

# render hi def snapshot?
render_snapshot(filename = fh,
                title_text = "Lismore flooding",title_bar_color = "#000000",title_color = "#FFFFFF" %>% darken(0.2),
                vignette = 0.7,vignette_color = "#000000",background = "#000000"
) 


# source save 3d gif and transition values functions
here::here("funcs","save_3d_gif.R") %>% source()
n_frames <- 2
zscale = 3/1
cmin <- 0 ; cmax <-  14
waterdepths <- transition_values(from = cmin, to = cmax, steps = n_frames,type = "lin",one_way = T) 
thetas <- seq(20,30,n_frames/(n_frames/2))
zooms <- seq(0.4,0.7,0.06) %>% rev

graphics.off()
rgl::rgl.close()

### 3d gif
base_map %>% 
  add_overlay(river_layer) %>%
  add_overlay(df_layer_polygon) %>%
  add_overlay(df_layer_polygon2,alphalayer = 0.9) %>%
  add_overlay(df_layer_polygon3, alphalayer = 0.9) %>%
  add_shadow(lamb_shade(df),0.5) %>%
  save_3d_gif(df, file = here::here("plot","spatial","3d",paste0("df_lamb_nframes",n_frames,"_flevel",cmax,"_zscale",zscale,"zt.gif")), 
              solid = T, shadow = T, water = T, zscale = zscale,
              watercolor = colvw, wateralpha = 0.8, 
              waterlinecolor = "#FFFFFF", waterlinealpha = 0.5,
              waterdepth = waterdepths, 
              theta = thetas, phi = 50, zoom = zooms, fov = 20
  )
